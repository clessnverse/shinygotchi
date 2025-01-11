library(cancensus)
library(dplyr)
library(tidyr)

language_variables <- c(
  "v_CA21_2209","v_CA21_2212","v_CA21_2215"
)

# Returns a data frame with data only
df_language <- get_census(
  dataset = "CA21", regions = list(C = "1"),
  vectors = language_variables,
  level = "C", use_cache = TRUE, geo_format = NA, quiet = TRUE
)

df_language_clean <- df_language %>%
  pivot_longer(cols = starts_with("v_"), names_to = "value", values_to = "population") %>%
  select(value, population) %>%
  mutate(value = sub(".*: ", "", value), location = "can", variable = "language") %>%
  select(location, variable, value, population)

# Save the data
saveRDS(df_language_clean, "data/cancensus/language.rds")
