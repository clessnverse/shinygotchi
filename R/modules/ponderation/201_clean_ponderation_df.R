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
  rename(value = age_group)

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
    )
  ) %>%
  group_by(location, variable, value) %>%
  summarise(population = sum(population), .groups = "drop")

# 3. Religion cleaning
df_weights_religion <- df_weights %>%
  filter(variable == "religion") %>%
  mutate(
    value = case_when(
      value == "Catholic" ~ "catholic",
      value == "Christian Orthodox" ~ "orthodox_christian",
      value %in% c("Baptist", "Lutheran", "Presbyterian", "Reformed", "United Church", "Methodist and Wesleyan (Holiness)") ~ "protestant",
      value %in% c("Pentecostal and other Charismatic", "Other Christian and Christian-related traditions") ~ "evangelical",
      value == "Buddhist" ~ "buddhist",
      value == "Hindu" ~ "hindu",
      value == "Jewish" ~ "jew",
      value == "Muslim" ~ "muslim",
      value == "Sikh" ~ "sikh",
      value %in% c("Other religions and spiritual traditions", "Traditional (North American Indigenous) spirituality") ~ "other",
      value == "No religion and secular perspectives" ~ "atheist_or_agnostic",
      TRUE ~ value
    )
  ) %>%
  # Handle the atheist/agnostic split
  mutate(
    value = case_when(
      value == "atheist_or_agnostic" ~ "atheist",
      TRUE ~ value
    ),
    population = case_when(
      value == "atheist" ~ population * 0.5,
      TRUE ~ population
    )
  ) %>%
  bind_rows(
    # Add agnostic row
    . %>% 
      filter(value == "atheist") %>%
      mutate(value = "agnostic")
  ) %>%
  group_by(location, variable, value) %>%
  summarise(population = sum(population), .groups = "drop")

# 4. Combine all cleaned variables
df_weights_clean <- bind_rows(
  df_weights_age,
  df_weights_dwelling,
  df_weights_religion
)
