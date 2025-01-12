library(cancensus)
library(dplyr)
library(tidyr)

education_variables <- c("v_CA21_5820","v_CA21_5823","v_CA21_5835","v_CA21_5838","v_CA21_5841","v_CA21_5844","v_CA21_5850","v_CA21_5853","v_CA21_5856","v_CA21_5859","v_CA21_5862")

# Returns a data frame with data only
df_education <- get_census(
  dataset = "CA21", regions = list(C = "1"),
  vectors = education_variables,
  level = "C", use_cache = TRUE, geo_format = NA, quiet = TRUE
)

df_education_clean <- df_education %>%
  pivot_longer(cols = starts_with("v_"), names_to = "value", values_to = "population") %>%
  select(value, population) %>%
  mutate(value = sub(".*: ", "", value), location = "can", variable = "education") %>%
  select(location, variable, value, population)

# Save the data
saveRDS(df_education_clean, "data/cancensus/education.rds")
