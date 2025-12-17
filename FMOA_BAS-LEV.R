# Libraries 
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(stringdist)

# Read Excel (THIS IS WORDS FROM MANUSCRIPT)
file_path <- "/Users/laurigurguryan/Desktop/FMOA/StimFromPaper.xlsx"
df_new <- read_excel(file_path)

# Split the comma-separated words into lists
df_new <- df_new %>%
  mutate(
    Phonological_associates = str_split(`Phonological associates`, pattern = ",\\s*"),
    Semantic_associates = str_split(`Semantic associates`, pattern = ",\\s*")
  ) %>%
  select(Theme, Phonological_associates, Semantic_associates)

# Create 4 rows per theme by pairing the phonological and semantic associates
df_long <- df_new %>%
  mutate(
    paired = map2(Phonological_associates, Semantic_associates, ~ tibble(
      Phonological = .x,
      Semantic = .y
    ))
  ) %>%
  select(Theme, paired) %>%
  unnest(cols = c(paired))

# Check 
head(df_long)

# Sort alphabetically 
#df_long_sorted <- df_long %>%
#  arrange(Theme, Phonological, Semantic)

# Check
#head(df_long_sorted)

##############
# Path to file (FROM https://docs.google.com/spreadsheets/d/102Ptcxr1vMa_J23ya8_nB_DCH09HryKy/edit?usp=sharing&ouid=111521900047420809973&rtpof=true&sd=true)
file_path <- "/Users/laurigurguryan/Desktop/FMOA/DRMWords_NormativeDataForJohn_111411.xlsx"

# Read file 
df <- read_excel(file_path)

# Select only column to keep 
df_stimuli_clean <- df %>%
  select(
    Theme,
    `Sem Assoc`,
    BAS,
    MeanBAS,
    `Phono Assoc`, 
    `Levenshtein Distance`
  )

# Check 
head(df_stimuli_clean)

# Words
words <- c(
  "act","admission","adult","again","alone","anger","annoy","answer","argue",
  "army","atom","baby","back","bake","basement","basket","beautiful","bee",
  "black","blood","boom","boring","bread","broom","building","butterfly",
  "buy","cabbage","carpet","chain","chair","chaos","cheese","church","city",
  "clam","cold","corn","dance","data","decrease","dense","dirt","doctor","draw",
  "dry","fail","fate","fence","finish","flag","flash","focus","fog","forever",
  "fruit","funny","gas","girl","give","handle","hawk","heaven","height","high",
  "hint","hold","horse","hydrogen","impact","job","justice","king","lesson",
  "letter","lie","logic","long","lose","man","many","marry","math","mine","mix",
  "morning","movie","muscle","needle","nose","nut","opinion","pan","pen",
  "piano","pig","pitch","plain","plaster","present","process","rain","rent",
  "ring","river","rock","roof","rough","saloon","scale","shake","sheet",
  "shirt","shovel","sink","site","sleep","slow","smart","smell","snake",
  "sneeze","soap","speaker","square","stress","sweet","teeth","thief","train",
  "trash","trip","urge","vote","waiter","whiskey","whole","window"
)

# Filter df_small where Theme matches any word exactly
df_stimuli_filtered <- df_stimuli_clean %>%
  filter(trimws(tolower(Theme)) %in% tolower(words))

# View results
head(df_stimuli_filtered)

# Sort alphabetically 
#df_stimuli_sorted <- df_stimuli_filtered %>%
#  arrange(Theme, `Phono Assoc`, `Sem Assoc`)

# Check
#head(df_stimuli_sorted)

#######
# CHECK IF THEY CORRESPOND 
# check <- df_long_sorted %>%
#   select(Theme, Phonological, Semantic) %>%
#   mutate(
#     Phono_Assoc = df_stimuli_sorted$`Phono Assoc`,
#     Sem_Assoc = df_stimuli_sorted$`Sem Assoc`,
#     # Get rid of spaces and convert to lowercase 
#     check_phono = tolower(trimws(Phonological)) == tolower(trimws(Phono_Assoc)),
#     check_semantic = tolower(trimws(Semantic)) == tolower(trimws(Sem_Assoc)),
#     check = check_phono & check_semantic
#   ) %>%
#   select(Theme, Phonological, Phono_Assoc, Semantic, Sem_Assoc, check)
# 
# # Preview
# head(check)

# For df_long - semantic
df_long_sem <- df_long %>%
  group_by(Theme) %>%
  summarise(
    unique_id_sem = paste(
      sort(unique(str_to_lower(str_trim(Semantic)))),
      collapse = ", "
    ),
    .groups = "drop"
  )

# For df_long - phonological
df_long_phono <- df_long %>%
  group_by(Theme) %>%
  summarise(
    unique_id_phono = paste(
      sort(unique(str_to_lower(str_trim(Phonological)))),
      collapse = ", "
    ),
    .groups = "drop"
  )

# For df_stimuli_filtered - semantic
df_stimuli_sem <- df_stimuli_filtered %>%
  group_by(Theme) %>%
  summarise(
    unique_id_sem = paste(
      sort(unique(str_to_lower(str_trim(`Sem Assoc`)))),
      collapse = ", "
    ),
    .groups = "drop"
  )

# For df_stimuli_filtered - phonological
df_stimuli_phono <- df_stimuli_filtered %>%
  group_by(Theme) %>%
  summarise(
    unique_id_phono = paste(
      sort(unique(str_to_lower(str_trim(`Phono Assoc`)))),
      collapse = ", "
    ),
    .groups = "drop"
  )

# Check 
# Ignore case and spaces 
df_long_clean <- df_long %>%
  mutate(
    Theme_clean = str_to_lower(str_trim(Theme)),
    Semantic_clean = str_to_lower(str_trim(Semantic)),
    Phonological_clean = str_to_lower(str_trim(Phonological))
  )

# Create sorted pair name 
df_long_summary <- df_long_clean %>%
  group_by(Theme_clean) %>%
  summarise(
    unique_id_sem_long = paste(sort(unique(Semantic_clean)), collapse = ", "),
    unique_id_phono_long = paste(sort(unique(Phonological_clean)), collapse = ", "),
    .groups = "drop"
  )

# Ignore case and spaces 
df_stimuli_clean <- df_stimuli_filtered %>%
  mutate(
    Theme_clean = str_to_lower(str_trim(Theme)),
    SemAssoc_clean = str_to_lower(str_trim(`Sem Assoc`)),
    PhonoAssoc_clean = str_to_lower(str_trim(`Phono Assoc`))
  )

# Create sorted pair name 
df_stimuli_summary <- df_stimuli_clean %>%
  group_by(Theme_clean) %>%
  summarise(
    unique_id_sem_stim = paste(sort(unique(SemAssoc_clean)), collapse = ", "),
    unique_id_phono_stim = paste(sort(unique(PhonoAssoc_clean)), collapse = ", "),
    .groups = "drop"
  )

# Combine 
check <- df_long_summary %>%
  full_join(df_stimuli_summary, by = "Theme_clean") %>%
  mutate(
    sem_match = unique_id_sem_long == unique_id_sem_stim,
    phono_match = unique_id_phono_long == unique_id_phono_stim
  )

head(check)

# Filter only mismatches
check_mismatches <- check %>%
  filter(!sem_match | !phono_match)

###################################
###################################
# Find Lev values 
df_long$LevDis_lg <- stringdist(df_long$Theme,
                                df_long$Phonological,
                                method = "lv")

###################################
###################################

# Create df with the bas/lev values 
df_long <- df_long %>%
  mutate(
    Semantic = str_trim(Semantic),          # remove sapce
    Semantic = str_replace_all(Semantic, "[\r\n]", "")  # remove \r and \n
  )

# df_final <- df_long %>%
#   left_join(
#     df_stimuli_filtered %>% select(`Phono Assoc`, `Levenshtein Distance`),
#     by = c("Phonological" = "Phono Assoc")
#   ) %>%

df_final <- df_long %>% left_join(
    df_stimuli_filtered %>% select(`Sem Assoc`, BAS),
    by = c("Semantic" = "Sem Assoc")
  )

# # Rows with missing values 
# df_final %>%
#   summarise(
#     NA_Levenshtein = sum(is.na(`Levenshtein Distance`)),
#     NA_BAS = sum(is.na(BAS))
#   )
# 
# df_missing <- df_final %>%
#   filter(is.na(`Levenshtein Distance`) | is.na(BAS)) %>%
#   mutate(row_number = row_number()) %>%
#   select(Theme, Phonological, `Levenshtein Distance`, Semantic, BAS, row_number)
# 
# # Print the result
# df_missing

# Add missing info for BAS (ticket = 0.245)
df_final <- df_final %>%
  mutate(BAS = as.numeric(BAS)) %>%
  mutate(BAS = if_else(Semantic == "ticket", 0.245, BAS))

# SUMMARY TABLE WITH BAS + LEV

df_final <- df_final %>%
  mutate(
    BAS = as.numeric(BAS),
    `LevDis_lg` = as.numeric(`LevDis_lg`))

df_BASLEV <- df_final %>%
  group_by(Theme) %>%                          
  summarise(
    mean_Levenshtein = mean(`LevDis_lg`, na.rm = TRUE),  
    mean_BAS = mean(BAS, na.rm = TRUE)                               
  ) %>%
  arrange(Theme)  # optional: sort by Theme

df_BASLEV

# Save 
write.csv(df_BASLEV, "/Users/laurigurguryan/Desktop/FMOA/BAS-LEV.csv", row.names = FALSE)

