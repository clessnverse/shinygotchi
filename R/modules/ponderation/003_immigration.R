library(cancensus)
library(dplyr)
library(tidyr)

immigration_variables <- c("v_CA21_4407", "v_CA21_4410")

# Returns a data frame with data only
df_immigration <- get_census(
  dataset = "CA21", regions = list(C = "1"),
  vectors = immigration_variables,
  level = "C", use_cache = TRUE, geo_format = NA, quiet = TRUE
)

df_immigration_clean <- df_immigration %>%
  pivot_longer(cols = starts_with("v_"), names_to = "value", values_to = "population") %>%
  select(value, population) %>%
  mutate(value = sub(".*: ", "", value), location = "can", variable = "immigration") %>%
  select(location, variable, value, population)

# Save the data
saveRDS(df_immigration_clean, "data/cancensus/immigration.rds")
