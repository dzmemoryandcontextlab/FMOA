library(tidyverse)
library(ggplot2)
library(lme4)
library(lmerTest)
library(emmeans)
library(car)
library(afex)

########################
# DEMOGRAPHICS - DZLAB #
########################
# Folder path
analysis_folder <- "/Users/laurigurguryan/Desktop/FMOA/EXP3/analysis"

# Find/read demographics file
demographics_file <- list.files(
  path = analysis_folder,
  pattern = "^demographics_IRB30014.*\\.csv$",
  full.names = TRUE,
  ignore.case = TRUE)

demographics <- read_csv(
  demographics_file,
  show_col_types = FALSE)

# Clean - select ones to keep & rename 
demographics <- demographics %>%
  dplyr::select(
    `RecordedDate`,
    study_code,
    D_DOB,
    D_Gender,
    D_Gender_4_TEXT,
    D_DomHand,
    D_Education,
    D_RE,
    Q128,
    Q129,
    Q129_7_TEXT,
    Q129_8_TEXT,
    PROLIFIC_PID,
    SubID) %>%
  rename(
    DOB = D_DOB,
    gender = D_Gender,
    gender_other = D_Gender_4_TEXT,
    DomHand = D_DomHand,
    education = D_Education,
    RaceEthnicity = D_RE,
    ethnicity_selfreport = Q128,
    RaceEthnicityGroup = Q129,
    RaceEthnicityGroup_other = Q129_7_TEXT,
    RaceEthnicityGroup_selfreport = Q129_8_TEXT) %>%
  mutate(DateAtTest = as.Date(as.POSIXct(`RecordedDate`, format = "%Y-%m-%d %H:%M:%S")))

# Keep only my study codes
demographics <- demographics %>%
  filter(str_detect(study_code, "^LG([1-9]|1[0-2])$"))

# Delete useless rows
demographics <- demographics[-c(1, 2, 3, 4, 5, 6), ]

# Calculate age
demographics <- demographics %>%
  mutate(
    DOB = as.Date(DOB, format = "%m/%d/%Y"),
    AgeAtTest = as.integer(format(DateAtTest, "%Y")) -
      as.integer(format(DOB, "%Y")) -
      ifelse(
        format(DateAtTest, "%m-%d") < format(DOB, "%m-%d"), 1,0))

# Age group
demographics <- demographics %>%
  mutate(
    AgeGroup = ifelse(AgeAtTest >= 59, "OA", "YA"))

# Missing DOB
missing_DOB <- demographics %>%
  filter(is.na(DOB)) %>%
  dplyr::select(SubID, study_code)

print(missing_DOB)

missing_DOB <- demographics %>%
  filter(is.na(DOB))

missing_DOB

###########################
# DEMOGRAPHICS - PROLIFIC #
##########################
# Find Prolific demographics file
prolific_file <- list.files(
  path = analysis_folder,
  pattern = "prolific_demographic.*\\.csv$",
  full.names = TRUE,
  ignore.case = TRUE)

# Read Prolific demographics
prolific_demographics <- read_csv(
  prolific_file,
  show_col_types = FALSE)

# Keep only APPROVED 
prolific_demographics <- prolific_demographics %>%
  filter(Status == "APPROVED")

# Rename and select - prolific 
prolific_compare <- prolific_demographics %>%
  dplyr::select(
    `Participant id`,
    Age,
    Sex) %>%
  rename(
    SubID = `Participant id`,
    Age_prolific = Age,
    Sex_prolific = Sex)

# Rename and select - dz lab form 
dzlab_compare <- demographics %>%
  dplyr::select(
    SubID,
    AgeAtTest,
    gender) %>%
  rename(
    Age_dzlab = AgeAtTest,
    Sex_dzlab = gender)

# Compare 
compareDEMO <- dzlab_compare %>%
  left_join(
    prolific_compare,
    by = "SubID") %>%
  mutate(
    Age_match = Age_dzlab == Age_prolific,
    Sex_match = Sex_dzlab == Sex_prolific)

compareDEMO

########
# DATA #
#######
# Path 
data_folder <- "/Users/laurigurguryan/Desktop/FMOA/EXP3/analysis/data"

# Find all CSVs
files <- list.files(
  path = data_folder,
  pattern = "\\.csv$",
  full.names = TRUE,
  ignore.case = TRUE)

# Read each CSV and force all columns to character
alldata_list <- purrr::map(
  files,
  function(file) {
    
    df <- read_csv(
      file,
      show_col_types = FALSE)
    
    # Convert every column to character
    df <- df %>%
      mutate(across(everything(), as.character))
    
    return(df)
  })

# Combine all files
alldata <- bind_rows(alldata_list)

# Make sure fullscreen column exists (if not then create with NA; not all have if don't esc)
if (!"fs_events_log" %in% names(alldata)) {
  alldata$fs_events_log <- NA_character_}

# Columns to keep
keep_cols <- c(
  "mapping",
  "left_label",
  "right_label",
  "verification_passed",
  "participant",
  "date",
  "expName",
  "study_word",
  "study_position",
  "version",
  "list_id",
  "condition",
  "theme_1",
  "theme_2",
  "theme_3",
  "test_word",
  "correct_response",
  "participant_response",
  "correct",
  "lag",
  "test_resp.keys",
  "math_check_participant_resp.text",
  "equation",
  "correct_ans",
  "fs_events_log", 
  "PS_Q1_debrief_response_box.text",
  "PS_Q2_TechDiffs_response.text",
  "PS_Q3_comments_response.text")

# Clean df with only columns we want
alldata_clean <- alldata %>%
 dplyr::select(any_of(keep_cols))

# Add age group to experiment data
alldata_clean <- alldata_clean %>%
  left_join(
    demographics %>%
     dplyr::select(SubID, AgeGroup),
    by = c("participant" = "SubID"))

# Number of  participants
n_participants <- alldata_clean %>%
  summarise(n = n_distinct(participant))
cat("Number of participants:", n_distinct(alldata_clean$participant))

# Save DZ demo df of participants that have BOTH (means they actually completed task)
participants_both <- demographics %>%
  filter(!is.na(SubID),
    SubID %in% alldata_clean$participant) %>%
  distinct(SubID, .keep_all = TRUE)

participants_both

cat("Number of participants in BOTH task and demographics:",
  n_distinct(participants_both$SubID),"\n")

#######################################################################
# FILTER DATA SO ONLY KEEP IF BOTH TASK DATA AND DZ DEMO DATA AND DOB #
#######################################################################
# Participant that have bOTH
participants_both_ids <- demographics %>%
  filter(!is.na(SubID),
    SubID %in% alldata_clean$participant) %>%
  distinct(SubID) %>%
  pull(SubID)

alldata_clean <- alldata_clean %>%
  filter(participant %in% participants_both_ids)

# Participants with missing DOB so can't assign age group 
missing_agegroup <- alldata_clean %>%
  filter(is.na(AgeGroup)) %>%
  distinct(participant, expName)

cat("Participants missing AgeGroup:",n_distinct(missing_agegroup$participant), "\n")
cat("Participant IDs:\n", paste(missing_agegroup$participant, collapse = "\n"),"\n")

# Figure out if we can recover missing age group
missing_agegroup <- alldata_clean %>%
  filter(is.na(AgeGroup)) %>%
  distinct(participant, expName) %>%
  
  # Add DZ Lab age
  left_join(
    demographics %>%
     dplyr::select(SubID, AgeAtTest, DOB),
    by = c("participant" = "SubID")) %>%
  
  # Add Prolific age
  left_join(
    prolific_compare %>%
     dplyr::select(SubID, Age_prolific),
    by = c("participant" = "SubID")) %>%
  
  # Use DZ Lab age when available otherwise use Prolific 
  mutate(
    Age_final = coalesce(
      as.numeric(AgeAtTest),
      as.numeric(Age_prolific)),
    AgeGroup_recovered = case_when(
      Age_final >= 59 ~ "OA",
      Age_final < 59 ~ "YA",
      TRUE ~ NA_character_))

missing_agegroup

# Add recovered AgeGroup back into the main dataset
alldata_clean <- alldata_clean %>%
  left_join(
    missing_agegroup %>%
     dplyr::select(participant, AgeGroup_recovered) %>%
      distinct(participant, .keep_all = TRUE),
    by = "participant") %>%
  mutate(
    AgeGroup = coalesce(AgeGroup, AgeGroup_recovered)) %>%
 dplyr::select(-AgeGroup_recovered)

# Report for manuscript who was recovered 
recovered_agegroup <- missing_agegroup %>%
  filter(!is.na(AgeGroup_recovered)) %>%
 dplyr::select(
    participant,
    expName,
    Age_prolific,
    AgeGroup_recovered)

cat("Participants whose age group was recovered from Prolific:",
  n_distinct(recovered_agegroup$participant), "\n")

#################
# MATH CHECK df #
#################
math_check <- alldata_clean %>%
 dplyr::select(
    participant,
    AgeGroup,
    expName,
    version, 
    math_check_participant_resp.text,
    equation,
    correct_ans, 
    fs_events_log) %>%
  filter(
    !is.na(equation),
    equation != "")

# Check if correct #
math_check <- math_check %>%
  mutate(
    math_correct = ifelse(
      math_check_participant_resp.text == correct_ans, 1, 0))

# Percent correct
math_accuracy <- math_check %>%
  group_by(participant, AgeGroup) %>%
  summarise(
    percent_correct = mean(math_correct, na.rm = TRUE) * 100,
    n_trials = n(),
    .groups = "drop")

math_accuracy

# Participants scoring below 75% on math check
math_exclude <- math_accuracy%>%
  filter(percent_correct < 75)

math_exclude

cat("Participants scoring below 75%:", paste(math_exclude$participant, collapse = ", "))

############
# STUDY DF #
############
STUDY_data <- alldata_clean %>%
 dplyr::select(
    participant,
    AgeGroup,
    expName,
    version,
    condition,
    study_word,
    study_position, 
    fs_events_log) %>%
  filter(
    !is.na(study_word),
    study_word != "")

###########
# TEST DF #
###########
TEST_data <- alldata_clean %>%
 dplyr::select(
    participant,
    AgeGroup,
    expName,
    version, 
    condition,
    test_word,
    correct_response,
    participant_response,
    correct,
    lag,
    test_resp.keys, 
    fs_events_log) %>%
  filter(
    !is.na(test_word),
    test_word != "",
    test_word != "ns")

# Count missed responses per participant
missed_responses <- TEST_data %>%
  group_by(participant, AgeGroup) %>%
  summarise(
    missed_trials = sum(
      is.na(participant_response) | participant_response == "",
      na.rm = TRUE),
    total_trials = n(),
    .groups = "drop")

missed_responses

# Participants missing 50% or more of TEST trials
missed_50plus <- missed_responses %>%
  filter(missed_trials / total_trials >= 0.50)

cat("Number of participants missing 50% or more of TEST trials:",
  nrow(missed_50plus),"\n")

cat("Participant IDs:",
  paste(missed_50plus$participant, collapse = ", "),"\n")

#####################
# FULLSCREEN CHECK #
####################
# Math 
fullscreen_math <- math_check %>%
  group_by(participant, AgeGroup) %>%
  summarise(
    math_fullscreen_events = sum(
      !is.na(fs_events_log) & fs_events_log != ""),
    .groups = "drop")

# Study 
fullscreen_study <- STUDY_data %>%
  group_by(participant, AgeGroup) %>%
  summarise(
    study_fullscreen_events = sum(
      !is.na(fs_events_log) & fs_events_log != ""),
    .groups = "drop")

# Test 
fullscreen_test <- TEST_data %>%
  group_by(participant, AgeGroup) %>%
  summarise(
    test_fullscreen_events = sum(
      !is.na(fs_events_log) & fs_events_log != ""),
    .groups = "drop")

# Combine into one dataframe
fullscreen_check <- fullscreen_math %>%
  full_join(fullscreen_study, by = "participant") %>%
  full_join(fullscreen_test, by = "participant")

fullscreen_check

########################
# Exclude participants #
########################

# Participants who still have no AgeGroup
missing_agegroup_final <- alldata_clean %>%
  filter(is.na(AgeGroup)) %>%
  distinct(participant)

# Combine all exclusion lists
exclude_participants <- unique(c(
  math_exclude$participant,
  missed_50plus$participant,
  missing_agegroup_final$participant))

# Info about elcusions 
excluded_info <- alldata_clean %>%
  filter(participant %in% exclude_participants) %>%
  mutate(
    failed_math = participant %in% math_exclude$participant,
    missed_trials = participant %in% missed_50plus$participant,
    missing_agegroup = participant %in% missing_agegroup_final$participant) %>%
  group_by(participant) %>%
  summarise(
    expName = first(na.omit(expName)),
    AgeGroup = first(na.omit(AgeGroup)),
    failed_math = first(failed_math),
    missed_trials = first(missed_trials),
    missing_agegroup = first(missing_agegroup),
    .groups = "drop") %>%
  mutate(
    exclusion_reason = case_when(
      failed_math & missed_trials & missing_agegroup ~ 
        "Math <75%; Missed >=50% of TEST; Missing AgeGroup",
      failed_math & missed_trials ~ 
        "Math <75%; Missed >=50% of TEST",
      failed_math & missing_agegroup ~ 
        "Math <75%; Missing AgeGroup",
      missed_trials & missing_agegroup ~ 
        "Missed >=50% of TEST; Missing AgeGroup",
      failed_math ~ 
        "Math <75%",
      missed_trials ~ 
        "Missed >=50% of TEST",
      missing_agegroup ~ 
        "Missing AgeGroup",
      TRUE ~ "Unknown"))

excluded_info

cat("Number of participants excluded:", length(exclude_participants), "\n")

cat("Math <75%:",n_distinct(math_exclude$participant), "\n")

cat("Missed >=50% of TEST:",n_distinct(missed_50plus$participant),"\n")

cat("Missing AgeGroup:",n_distinct(missing_agegroup_final$participant),"\n")

# Exclude participants from the clean dataset
cat("Number of participants BEFORE excluding:",n_distinct(alldata_clean$participant),"\n")

alldata_clean <- alldata_clean %>%
  filter(!participant %in% exclude_participants)

cat("Number of participants AFTER excluding:",n_distinct(alldata_clean$participant),"\n")

####################
# STUDY DF - CLEAN #
####################
STUDY_data_final <- alldata_clean %>%
 dplyr::select(
    participant,
    AgeGroup,
    expName,
    version,
    list_id,
    condition,
    study_word,
    study_position, 
    fs_events_log) %>%
  filter(
    !is.na(study_word),
    study_word != "")

STUDY_data_final <- STUDY_data_final %>%
  mutate(
    study_position = as.numeric(study_position),
    sublist = case_when(
      study_position >= 1 & study_position <= 4 ~ 1,
      study_position >= 5 & study_position <= 8 ~ 2,
      study_position >= 9 & study_position <= 12 ~ 3,
      TRUE ~ NA_real_),
    sublist = factor(sublist, levels = c(1, 2, 3)))

###################
# TEST DF - CLEAN #
###################
TEST_data_final <- alldata_clean %>%
 dplyr::select(
    participant,
    AgeGroup,
    expName,
    version,
    list_id,
    condition,
    theme_1,
    theme_2,
    theme_3,
    test_word,
    correct_response,
    participant_response,
    correct,
    lag,
    test_resp.keys,
    fs_events_log) %>%
  filter(
    !is.na(test_word),
    test_word != "",
    test_word != "ns")

# Create lookup table based on STUDY linking each studied word to its sublist
study_sublist_lookup <- STUDY_data_final %>%
 dplyr::select(
    participant,
    list_id,
    study_word,
    sublist) %>%
  distinct()

# Add sublist  to TEST studied/old words
TEST_data_final <- TEST_data_final %>%
  left_join(
    study_sublist_lookup,
    by = c(
      "participant",
      "list_id",
      "test_word" = "study_word"))

# Figure out sublist to NEW/LURE words using their theme info
TEST_data_final <- TEST_data_final %>%
  mutate(
    sublist = case_when(
      !is.na(sublist) ~ as.numeric(sublist),
      test_word == theme_1 ~ 1,
      test_word == theme_2 ~ 2,
      test_word == theme_3 ~ 3,
      TRUE ~ NA_real_),
    sublist = factor(
      sublist,
      levels = c(1, 2, 3)))

###################
# CHECK VERSIONS # 
###################

# Prticipants in each version after exclusions
participants_by_exp <- TEST_data_final %>%
  distinct(participant, expName, AgeGroup) %>%
  count(expName, AgeGroup, name = "n_participants")

participants_by_exp

#######################
# RESPONSE CATEGORIES # 
#######################

# Create reponse categories 
TEST_data_final <- TEST_data_final %>%
  mutate(
    response_type = case_when(
      correct_response == "old" & participant_response == "old" ~ "Hit",
      correct_response == "new" & participant_response == "new" ~ "CorReject",
      correct_response == "old" & participant_response == "new" ~ "Miss",
      correct_response == "new" & participant_response == "old" ~ "FalseAlarm",
      TRUE ~ NA_character_))

# Participant proportions
response_subject <- TEST_data_final %>%
  filter(!is.na(response_type)) %>%
  count(participant, AgeGroup, condition, response_type) %>%
  group_by(participant, AgeGroup, condition) %>%
  mutate(
    total_trials = sum(n),
    proportion = n / total_trials) %>%
  ungroup()

# Summery mean and SD 
response_plot_data <- response_subject %>%
  group_by(AgeGroup, condition, response_type) %>%
  summarise(
    mean = mean(proportion, na.rm = TRUE),
    sd = sd(proportion, na.rm = TRUE),
    n = n(),
    .groups = "drop")

response_plot_data

# Plot 
ggplot(response_plot_data,
  aes(x = response_type,y = mean,
    fill = condition)) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7) +
  geom_errorbar(
    aes(ymin = mean - sd, ymax = mean + sd),
    position = position_dodge(width = 0.8),
    width = 0.2) +
  facet_wrap(~AgeGroup) +
  labs(x = "Response Type", y = "Proportion", fill = "List Context") +
  scale_fill_manual(
    values = c(
      "phonological" = "#08306B",
      "semantic" = "#9ECAE1"),
    labels = c(
      "phonological" = "Phonological",
      "semantic" = "Semantic")) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  theme_classic()

###############################
###############################
# DEMOGRAPHICS FOR MANUSCRIPT #
###############################
###############################
# Final participants after all exclusions
final_participants <- alldata_clean %>%
  distinct(participant, AgeGroup)

# Merge final participants with demographic information
final_demographics <- final_participants %>%
  left_join(
    demographics %>%
     dplyr::select(
        SubID,
        AgeAtTest,
        gender,
        DomHand,
        education,
        RaceEthnicity),
    by = c("participant" = "SubID")) %>%
  left_join(
    prolific_compare %>%
     dplyr::select(SubID, Age_prolific),
    by = c("participant" = "SubID")) %>%
  mutate(
    AgeAtTest = as.numeric(AgeAtTest),
    Age_prolific = as.numeric(Age_prolific),
    Age_final = coalesce(AgeAtTest, Age_prolific))

final_demographics

# Age 
table(final_demographics$AgeGroup, useNA = "ifany")

final_age_summary <- final_demographics %>%
  group_by(AgeGroup) %>%
  summarise(
    mean_age = mean(Age_final, na.rm = TRUE),
    sd_age = sd(Age_final, na.rm = TRUE),
    n = n(),
    .groups = "drop")

final_age_summary

# Other info 
table(final_demographics$AgeGroup,
      final_demographics$gender,
  useNA = "ifany")

###########################
###########################
# ANALYSES FOR MANUSCRIPT #
###########################
###########################

# Reset vairbales properly since all character
TEST_data_final <- TEST_data_final %>%
  mutate(
    participant = as.factor(participant),
    AgeGroup = factor(AgeGroup, levels = c("YA", "OA")),
    condition = factor(condition, levels = c("phonological", "semantic")),
    correct = as.numeric(correct),
    lag = as.numeric(lag))

# Repeated measures anova (recogntion accuracy which is hit + corr reject) - FOOTNOTE
accuracy_rm_data <- TEST_data_final %>%
  group_by(participant, AgeGroup, condition, sublist) %>%
  summarise(
    accuracy = mean(correct, na.rm = TRUE),
    .groups = "drop")

accuracy_rm_model <- aov_ez(
  id = "participant",
  dv = "accuracy",
  between = "AgeGroup",
  within = c("condition", "sublist"),
  data = accuracy_rm_data,
  type = 3,
  anova_table = list(es = "pes")
)

accuracy_rm_model

# Condition difference within each age × sublist 
emmeans(accuracy_rm_model, ~ condition | AgeGroup * sublist)

pairs(emmeans(accuracy_rm_model,~ condition | AgeGroup * sublist),
  adjust = "holm")

# Agw difference within each condition × sublist
emmeans(accuracy_rm_model,~ AgeGroup | condition * sublist)

pairs(emmeans(accuracy_rm_model,~ AgeGroup | condition * sublist),
  adjust = "holm")

accuracy_rm_data <- TEST_data_final %>%
  group_by(participant, AgeGroup, condition, sublist) %>%
  summarise(
    accuracy = mean(correct, na.rm = TRUE),
    .groups = "drop")

accuracy_rm_model <- aov_ez(
  id = "participant",
  dv = "accuracy",
  between = "AgeGroup",
  within = c("condition", "sublist"),
  data = accuracy_rm_data,
  type = 3,
  anova_table = list(es = "pes")
)

accuracy_rm_model

# Condition difference within each age × sublist 
emmeans(accuracy_rm_model, ~ condition | AgeGroup * sublist)

pairs(emmeans(accuracy_rm_model,~ condition | AgeGroup * sublist),
  adjust = "holm")

# Agw difference within each condition × sublist
emmeans(accuracy_rm_model,~ AgeGroup | condition * sublist)

pairs(emmeans(accuracy_rm_model,~ AgeGroup | condition * sublist),
  adjust = "holm")

# Plot 
accuracy_plot_data <- TEST_data_final %>%
  group_by(participant, AgeGroup, condition, sublist) %>%
  summarise(
    accuracy = mean(correct, na.rm = TRUE),
    .groups = "drop")

ggplot(accuracy_rm_data,
  aes(x = sublist, y = accuracy,fill = condition)) +
  geom_violin(
    trim = FALSE,
    alpha = 0.7,
    position = position_dodge(width = 0.8)) +
  geom_jitter(
    aes(group = condition),
    position = position_jitterdodge(
      jitter.width = 0.12,
      dodge.width = 0.8),
    size = 1.5,
    shape = 21,
    fill = "grey",
    color = "darkgrey",
    stroke = 0.5,
    alpha = 0.8) +
  stat_summary(
    aes(group = condition),
    fun = mean,
    geom = "point",
    position = position_dodge(width = 0.8),
    size = 3,
    color = "black") +
  facet_wrap(~AgeGroup) +
  labs(
    x = "Sublist",
    y = "Recognition accuracy",
    fill = "Condition") +
  scale_fill_manual(
    values = c(
      "phonological" = "#08306B",
      "semantic" = "#9ECAE1"),
    labels = c(
      "phonological" = "Phonological",
      "semantic" = "Semantic")) +
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::percent) +
  theme_classic() +
  theme(
    strip.text = element_text(
      size = 12,
      face = "bold"),
    legend.position = "top")

###############
## HIT RATE ##
###############

# Recognition accurary (hits only) 
old_trials <- TEST_data_final %>%
  filter(correct_response == "old")

hit_rm_data <- old_trials %>%
  group_by(participant, AgeGroup, condition, sublist) %>%
  summarise(
    hit_rate = mean(correct, na.rm = TRUE),
    .groups = "drop")

hit_rm_data

hit_rm_model <- aov_ez(
  id = "participant",
  dv = "hit_rate",
  between = "AgeGroup",
  within = c("condition", "sublist"),
  data = hit_rm_data,
  type = 3,
  anova_table = list(es = "pes")
)

hit_rm_model

# Main effect condition
emmeans(hit_rm_model, ~ condition)
pairs(emmeans(hit_rm_model, ~ condition), adjust = "holm")

# Main efefct sublist
emmeans(hit_rm_model, ~ sublist)
pairs(emmeans(hit_rm_model, ~ sublist), adjust = "holm")

###########################
## STUDY-TEST LAG - HITS ##
###########################

# Analysis PREREGISTRATION - lag analysis (only old trials)
old_trials <- TEST_data_final %>%
  filter(correct_response == "old")

old_trials <- old_trials %>%
  mutate(lag_z = as.numeric(scale(lag)) )

old_accuracy_lag_model <- glmer(correct ~ AgeGroup * condition * lag_z + (1 | participant),
  data = old_trials,
  family = binomial)

summary(old_accuracy_lag_model)

Anova(old_accuracy_lag_model, type = 3)

emmeans(old_accuracy_lag_model, ~ condition, type = "response")

pairs(emmeans(old_accuracy_lag_model, ~ condition, type = "response"))

# pLOT 
old_accuracy_lag_data <- old_trials %>%
  group_by(participant, AgeGroup, condition, lag) %>%
  summarise(
    accuracy = mean(correct, na.rm = TRUE),
    .groups = "drop")

ggplot(old_accuracy_lag_data,
       aes(x = lag, y = accuracy, color = condition, fill = condition)) +
  geom_point(
    aes(fill = condition),
    shape = 21,
    size = 1.2,
    stroke = 0.4,
    alpha = 0.15) +
  stat_summary(
    fun = mean,
    geom = "line",
    linewidth = 1) +
  stat_summary(
    fun.data = mean_cl_normal,
    geom = "ribbon",
    alpha = 0.20,
    color = NA) +
  facet_wrap(~AgeGroup) +
  labs(
    title = "Hit probability across study-test lag",
    x = "Study–test lag",
    y = "Hit probability",
    color = "List context",
    fill = "List context") +
  scale_color_manual(
    values = c(
      "phonological" = "#0072B2",
      "semantic" = "#E69F00"),
    labels = c(
      "phonological" = "Phonological",
      "semantic" = "Semantic")) +
  scale_fill_manual(
    values = c(
      "phonological" = "#0072B2",
      "semantic" = "#E69F00"),
    labels = c(
      "phonological" = "Phonological",
      "semantic" = "Semantic")) +
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::percent) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "bottom")

################
# FALSE ALARMS #
################

new_trials <- TEST_data_final %>%
  filter(correct_response == "new") %>%
  mutate(
    false_alarm = 1 - correct)

fa_rm_data <- new_trials %>%
  group_by(participant, AgeGroup, condition, sublist) %>%
  summarise(
    false_alarm_rate = mean(false_alarm, na.rm = TRUE),
    .groups = "drop")

fa_rm_data

fa_rm_model <- aov_ez(
  id = "participant",
  dv = "false_alarm_rate",
  between = "AgeGroup",
  within = c("condition", "sublist"),
  data = fa_rm_data,
  type = 3,
  anova_table = list(es = "pes"))

fa_rm_model

# Main effect condition
emmeans(fa_rm_model, ~ condition)

pairs(emmeans(fa_rm_model, ~ condition))

# Main effect sublist
emmeans(fa_rm_model, ~ sublist)

pairs(emmeans(fa_rm_model, ~ sublist), adjust = "holm")

# Condition x sublist interaction
emmeans(fa_rm_model, ~ condition * sublist)

pairs(emmeans(fa_rm_model, ~ condition | sublist), adjust = "holm")


# AVERAGE PER INDIVIDUAL LIST 
recognition_counts <- TEST_data_final %>%
  group_by(participant, AgeGroup, condition, list_id, sublist) %>%
  summarise(
    Hits = sum(
      correct_response == "old" & participant_response == "old",
      na.rm = TRUE),
    
    Misses = sum(
      correct_response == "old" & participant_response == "new",
      na.rm = TRUE),
    
    Correct_Rejections = sum(
      correct_response == "new" & participant_response == "new",
      na.rm = TRUE),
    
    False_Alarms = sum(
      correct_response == "new" & participant_response == "old",
      na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  
  group_by(AgeGroup, condition, sublist) %>%
  summarise(
    Hit_M = mean(Hits),
    Hit_SD = sd(Hits),
    
    Miss_M = mean(Misses),
    Miss_SD = sd(Misses),
    
    CR_M = mean(Correct_Rejections),
    CR_SD = sd(Correct_Rejections),
    
    FA_M = mean(False_Alarms),
    FA_SD = sd(False_Alarms),
    
    n = n(),
    .groups = "drop"
  ) %>%
  
  mutate(
    across(
      c(Hit_M, Hit_SD,
        Miss_M, Miss_SD,
        CR_M, CR_SD,
        FA_M, FA_SD),
      ~ round(.x, 2)
    ))

recognition_counts

############################
# HIT & FALSE ALARM RATES #
############################

# Calculate rates for each participant × condition × sublist
recognition_rates_subject <- TEST_data_final %>%
  group_by(participant, AgeGroup, condition, sublist) %>%
  summarise(
    
    # Hits and old trials
    Hits = sum(
      correct_response == "old" & participant_response == "old",
      na.rm = TRUE),
    
    Old_Trials = sum(
      correct_response == "old",
      na.rm = TRUE),
    
    # False alarms and new trials
    False_Alarms = sum(
      correct_response == "new" & participant_response == "old",
      na.rm = TRUE),
    
    New_Trials = sum(
      correct_response == "new",
      na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(
    hit_rate = Hits / Old_Trials,
    false_alarm_rate = False_Alarms / New_Trials
  )

# Mean and SD across participants
recognition_rates <- recognition_rates_subject %>%
  group_by(AgeGroup, condition, sublist) %>%
  summarise(
    Hit_M = mean(hit_rate, na.rm = TRUE),
    Hit_SD = sd(hit_rate, na.rm = TRUE),
    
    FA_M = mean(false_alarm_rate, na.rm = TRUE),
    FA_SD = sd(false_alarm_rate, na.rm = TRUE),
    
    n = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    across(
      c(Hit_M, Hit_SD, FA_M, FA_SD),
      ~ round(.x, 2)
    ))

recognition_rates

###############
# POST SURVEY #
##############
post_survey <- alldata_clean %>%
 dplyr::select(
    participant,
    AgeGroup,
    PS_Q1_debrief_response_box.text,
    PS_Q2_TechDiffs_response.text,
    PS_Q3_comments_response.text ) %>%
  filter(
    !is.na(PS_Q1_debrief_response_box.text) |
      !is.na(PS_Q2_TechDiffs_response.text) |
      !is.na(PS_Q3_comments_response.text))

post_survey


