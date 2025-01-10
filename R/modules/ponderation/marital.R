library(cancensus)
library(dplyr)
library(tidyr)
library(stringr)


marital_variables <- c(
  "v_CA21_459","v_CA21_462","v_CA21_480","v_CA21_483","v_CA21_486","v_CA21_489"
)

# Returns a data frame with data only
df <- get_census(
  dataset = "CA21", regions = list(C = "1"),
  vectors = marital_variables,
  level = "C", use_cache = TRUE, geo_format = NA, quiet = TRUE
)

df_clean <- df %>%
  pivot_longer(cols = matches("v_CA21_"), names_to = "marital", values_to = "population") %>%
  select(marital, population) %>%
  mutate(
    marital = case_when(
      str_detect(marital, "Never married") ~ "Single",  # Never married
      str_detect(marital, "Married$") ~ "Married",  # Married
      str_detect(marital, "Living common-law") ~ "Common-law relationship",  # Living common-law
      str_detect(marital, "Widowed") ~ "Widower/widow",  # Widowed
      str_detect(marital, "Separated|Divorced") ~ "Divorced/separated",  # Regroup Separated and Divorced
      TRUE ~ "Other"  # Pour toute autre catégorie
    )
  ) %>%
  group_by(marital) %>%  # Regroupe par statut marital
  summarise(population = sum(population, na.rm = TRUE)) %>%  # Additionne les populations pour éviter les doublons
  mutate(country = "CA")
# Save the data
saveRDS(df_clean, "data/cancensus/marital.rds")
