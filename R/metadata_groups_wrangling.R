library(dplyr)
library(tidyr)

# Load the data
df <- readRDS("data/df_canada.rds")

# "ses_age_group"                                
# "ses_province"                                 
# "ses_gender_factor"                            
# "ses_religion"                                 
# "ses_language"                                 
# "ses_income"                                   
# "ses_education_group"                          
# "ses_religiosity"                              
# "ses_owner"                                    
# "ses_ethnicity"                                
# "ses_immigrant"                                

# Wrangle the data

# Extract column names as rows in a new dataframe
df_var <- tibble(variables = colnames(df))


