library(cancensus)
library(dplyr)
library(tidyr)

income_variables <- c("v_CA21_674","v_CA21_677","v_CA21_680","v_CA21_683","v_CA21_686","v_CA21_689","v_CA21_692","v_CA21_695","v_CA21_698","v_CA21_701","v_CA21_707","v_CA21_710")

# Returns a data frame with data only
df_income <- get_census(
  dataset = "CA21", regions = list(C = "1"),
  vectors = income_variables,
  level = "C", use_cache = TRUE, geo_format = NA, quiet = TRUE
)

df_income_clean <- df_income %>%
  pivot_longer(cols = starts_with("v_"), names_to = "value", values_to = "population") %>%
  select(value, population) %>%
  mutate(value = sub(".*: ", "", value), location = "can", variable = "income") %>%
  select(location, variable, value, population)

# Save the data
saveRDS(df_income_clean, "data/cancensus/income.rds")
