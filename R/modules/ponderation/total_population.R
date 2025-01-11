library(cancensus)
library(dplyr)
library(tidyr)

total_population_variable <- c("v_CA21_1")

# Returns a data frame with data only
df_total_population <- get_census(
  dataset = "CA21", regions = list(C = "1"),
  vectors = total_population_variable,
  level = "C", use_cache = TRUE, geo_format = NA, quiet = TRUE
)

df_total_population_clean <- df_total_population %>%
  mutate(location = "can", variable = "total_population", value = "total population" ,population = df_total_population$Population) %>%
  select(location, variable, value, population)

# Save the data
saveRDS(df_total_population_clean, "data/cancensus/total_population.rds")
