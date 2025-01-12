library(cancensus)
library(dplyr)
library(tidyr)

sex_variables <- c(
  "v_CA21_10","v_CA21_9"
)

# Returns a data frame with data only
df_sex <- get_census(
  dataset = "CA21", regions = list(C = "1"),
  vectors = sex_variables,
  level = "C", use_cache = TRUE, geo_format = NA, quiet = TRUE
)

df_sex_clean <- df_sex %>%
  pivot_longer(cols = starts_with("v_"), names_to = "value", values_to = "population") %>%
  select(value, population) %>%
  mutate(value = ifelse(grepl("^v_CA21_9", value), "male", "female"), location = "can", variable = "sex") %>%
  select(location, variable, value, population)

# Save the data
saveRDS(df_sex_clean, "data/cancensus/sex.rds")
