library(cancensus) 
library(dplyr)
library(tidyr)

owner_variables <- c("v_CA21_4238","v_CA21_4239","v_CA21_4240")

# Returns a data frame with data only
df_owner <- get_census(
  dataset = "CA21", regions = list(C = "1"),
  vectors = owner_variables,
  level = "C", use_cache = TRUE, geo_format = NA, quiet = TRUE
)

df_owner_clean <- df_owner %>%
  pivot_longer(cols = starts_with("v_"), names_to = "value", values_to = "population") %>%
  select(value, population) %>%
  mutate(value = sub(".*: ", "", value), location = "can", variable = "owner") %>%
  select(location, variable, value, population)

# Save the data
saveRDS(df_owner_clean, "data/cancensus/owner.rds")
