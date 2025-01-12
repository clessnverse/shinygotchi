library(cancensus)
library(dplyr)
library(tidyr)

religion_variables <- c("v_CA21_5673","v_CA21_5679","v_CA21_5682","v_CA21_5685","v_CA21_5688","v_CA21_5691","v_CA21_5694","v_CA21_5697","v_CA21_5700","v_CA21_5703","v_CA21_5706","v_CA21_5709","v_CA21_5712","v_CA21_5715","v_CA21_5718","v_CA21_5721","v_CA21_5724","v_CA21_5727","v_CA21_5730","v_CA21_5733","v_CA21_5736","v_CA21_5739","v_CA21_5742"
)

# Returns a data frame with data only
df_religion <- get_census(
  dataset = "CA21", regions = list(C = "1"),
  vectors = religion_variables,
  level = "C", use_cache = TRUE, geo_format = NA, quiet = TRUE
)

df_religion_clean <- df_religion %>%
  pivot_longer(cols = starts_with("v_"), names_to = "value", values_to = "population") %>%
  select(value, population) %>%
  mutate(value = sub(".*: ", "", value), location = "can", variable = "religion") %>%
  select(location, variable, value, population)

# Save the data
saveRDS(df_religion_clean, "data/cancensus/religion.rds")
