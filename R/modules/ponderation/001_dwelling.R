library(cancensus)
library(dplyr)
library(tidyr)

dwelling_variables <- c("v_CA21_436","v_CA21_435","v_CA21_437","v_CA21_438","v_CA21_439","v_CA21_440","v_CA21_441","v_CA21_442")

# Returns a data frame with data only
df_dwelling <- get_census(
  dataset = "CA21", regions = list(C = "1"),
  vectors = dwelling_variables,
  level = "C", use_cache = TRUE, geo_format = NA, quiet = TRUE
)

df_dwelling_clean <- df_dwelling %>%
  pivot_longer(cols = starts_with("v_"), names_to = "value", values_to = "population") %>%
  select(value, population) %>%
  mutate(value = sub(".*: ", "", value), location = "can", variable = "dwelling") %>%
  select(location, variable, value, population)

# Save the data
saveRDS(df_dwelling_clean, "data/cancensus/dwelling.rds")
