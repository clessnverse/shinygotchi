library(cancensus)
library(dplyr)
library(tidyr)

options(cancensus.api_key = "your_api_key")

sex_variables <- c(
  "v_CA21_10","v_CA21_9"
)

# Returns a data frame with data only
df <- get_census(
  dataset = "CA21", regions = list(C = "1"),
  vectors = sex_variables,
  level = "C", use_cache = FALSE, geo_format = NA, quiet = TRUE
)

df_clean <- df %>%
  pivot_longer(cols = starts_with("v_"), names_to = "sex", values_to = "population") %>%
  select(sex, population) %>%
  mutate(sex = case_when(
    sex == "v_CA21_10: Total - Age" ~ "female",
    sex == "v_CA21_9: Total - Age" ~ "male"
  )) %>%
  mutate(country = "CA")

# Save the data
saveRDS(df_clean, "data/cancensus/sex.rds")

