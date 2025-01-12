library(cancensus)
library(dplyr)
library(tidyr)
library(stringr)

marital_variables <- c(
  "v_CA21_459","v_CA21_462","v_CA21_480","v_CA21_483","v_CA21_486","v_CA21_489"
)

# Returns a data frame with data only
df_marital <- get_census(
  dataset = "CA21", regions = list(C = "1"),
  vectors = marital_variables,
  level = "C", use_cache = TRUE, geo_format = NA, quiet = TRUE
)

df_marital_clean <- df_marital %>%
  pivot_longer(cols = starts_with("v_"), names_to = "value", values_to = "population") %>%
  select(value, population) %>%
  mutate(value = sub(".*: ", "", value), location = "can", variable = "marital") %>%
  select(location, variable, value, population)

# Save the data
saveRDS(df_marital_clean, "data/cancensus/marital.rds")
