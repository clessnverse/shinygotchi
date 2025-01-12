library(dplyr)

df_weights <- readRDS("data/cancensus/population_data.rds")

# 1. Age cleaning
df_weights_age <- df_weights %>%
  filter(variable == "age") %>%
  mutate(
    age_group = case_when(
      as.numeric(value) >= 18 & as.numeric(value) <= 24 ~ "18_24",
      as.numeric(value) >= 25 & as.numeric(value) <= 44 ~ "25_44",
      as.numeric(value) >= 45 & as.numeric(value) <= 64 ~ "45_64",
      as.numeric(value) >= 65 ~ "65_plus"
    )
  ) %>%
  filter(!is.na(age_group)) %>%
  group_by(location, variable, age_group) %>%
  summarise(population = sum(population), .groups = "drop") %>%
  rename(value = age_group) %>%
  mutate(variable = "ses_age_group")

# 2. Dwelling cleaning
df_weights_dwelling <- df_weights %>%
  filter(variable == "dwelling") %>%
  mutate(
    value = case_when(
      value %in% c("Single-detached house") ~ "stand_alone_house",
      value %in% c("Semi-detached house", "Row house", "Other single-attached house") ~ "townhouse",
      value %in% c("Apartment or flat in a duplex") ~ "duplex",
      value %in% c("Apartment in a building that has fewer than five storeys") ~ "apartment_complex",
      value %in% c("Apartment in a building that has five or more storeys") ~ "high_rise_apartment",
      value %in% c("Movable dwelling") ~ "mobile_home",
      TRUE ~ value
    ),
    variable = "ses_dwelling"
  ) %>%
  group_by(location, variable, value) %>%
  summarise(population = sum(population), .groups = "drop")

# 3. immigration cleaning#
df_weights_immigrant <- df_weights %>%
  filter(variable == "immigration") %>%
  mutate(
    value = case_when(
      value == "Non-immigrants" ~ "0",
      value == "Immigrants" ~ "1"
    ),
    variable = "ses_immigrant"
  ) %>%
  group_by(location, variable, value) %>%
  summarise(population = sum(population), .groups = "drop")

# 4. Income cleaning

  


# 9. Religion cleaning
df_weights_religion <- df_weights %>%
  filter(variable == "religion") %>%
  mutate(
    value = case_when(
      # Group all Christian denominations together
      value %in% c("Christian, n.o.s.", "Anabaptist", "Anglican", "Baptist", 
                   "Catholic", "Christian Orthodox", "Jehovah's Witness", 
                   "Latter Day Saints", "Lutheran", "Methodist and Wesleyan (Holiness)",
                   "Pentecostal and other Charismatic", "Presbyterian", "Reformed",
                   "United Church", "Other Christian and Christian-related traditions") ~ "christian",
      value == "Buddhist" ~ "buddhist",
      value == "Hindu" ~ "hindu",
      value == "Jewish" ~ "jew",
      value == "Muslim" ~ "muslim",
      value == "No religion and secular perspectives" ~ "agnostic/atheist",
      # Group remaining into other
      TRUE ~ "other"
    ),
    variable = "ses_religion_big_five"
  ) %>%
  group_by(location, variable, value) %>%
  summarise(population = sum(population), .groups = "drop")

# Combine all cleaned variables
df_weights_clean <- bind_rows(
  df_weights_age,
  df_weights_dwelling,
  df_weights_religion
)
