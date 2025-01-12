library(dplyr)
library(tidyr)

# Load data - The file you load will change when you get newer data
df_pilot <- readRDS("data/data_pilot_clean_janv_2025.rds")

df_ses <- df_pilot %>%
  select(ses_age_group, ses_dwelling, ses_education, ses_immigrant, ses_income, 
         ses_language, ses_status, ses_owner, ses_province, ses_religion, 
         ses_gender_factor) %>%
  # Convert all columns to character
  mutate(across(everything(), as.character)) %>%
  # Now pivot
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "value"
  ) %>%
  # Count occurrences
  count(variable, value, name = "population") %>%
  # Remove any NA combinations if you want
  filter(!is.na(value)) %>%
  mutate(location = "can")


