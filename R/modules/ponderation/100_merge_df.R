library(dplyr)
library(tidyr)

# Load data
df_merge_age <- readRDS("data/cancensus/age.rds")
df_merge_dwelling <- readRDS("data/cancensus/dwelling.rds")
df_merge_education <- readRDS("data/cancensus/education.rds")
df_merge_immigration <- readRDS("data/cancensus/immigration.rds")
df_merge_income <- readRDS("data/cancensus/income.rds")
df_merge_language <- readRDS("data/cancensus/language.rds")
df_merge_marital <- readRDS("data/cancensus/marital.rds")
df_merge_owner <- readRDS("data/cancensus/owner.rds")
df_merge_province <- readRDS("data/cancensus/province.rds")
df_merge_religion <- readRDS("data/cancensus/religion.rds")
df_merge_sex <- readRDS("data/cancensus/sex.rds")
df_merge_total_population <- readRDS("data/cancensus/total_population.rds")

# Merge data
df_merge <- bind_rows(
  df_merge_age,
  df_merge_dwelling,
  df_merge_education,
  df_merge_immigration,
  df_merge_income,
  df_merge_language,
  df_merge_marital,
  df_merge_owner,
  df_merge_province,
  df_merge_religion,
  df_merge_sex,
  df_merge_total_population
)

saveRDS(df_merge, "data/cancensus/population_data.rds")
