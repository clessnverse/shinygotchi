library(dplyr)
library(tidyr)

# Load the data
df <- readRDS("data/df.rds")

# Wrangle the data

# Extract column names as rows in a new dataframe
df_var <- tibble(variables = colnames(df))


