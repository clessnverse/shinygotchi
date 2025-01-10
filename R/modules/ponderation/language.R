library(cancensus)
library(dplyr)
library(tidyr)

options(cancensus.api_key = "your_api_key")

language_variables <- c(
  "v_CA21_2209","v_CA21_2212","v_CA21_2215"
)

# Returns a data frame with data only
df <- get_census(
  dataset = "CA21", regions = list(C = "1"),
  vectors = language_variables,
  level = "C", use_cache = TRUE, geo_format = NA, quiet = TRUE
)

df_clean <- df %>%
  pivot_longer(cols = starts_with("v_"), names_to = "language", values_to = "population") %>%
  select(language, population) %>%
  mutate(language = sub(".*: ", "", language)) %>%  # Conserve les noms de langues
  mutate(language = case_when(
    language == "Non-official languages" ~ "Other",  # Remplace "Non-official languages" par "Other"
    TRUE ~ language  # Conserve les autres valeurs telles quelles
  )) %>%
  mutate(country = "CA")

# Save the data
saveRDS(df_clean, "data/cancensus/language.rds")

