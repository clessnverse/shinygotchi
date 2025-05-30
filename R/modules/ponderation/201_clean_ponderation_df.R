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
      as.numeric(value) >= 65 ~ "65+"
    )
  ) %>%
  filter(!is.na(age_group)) %>%
  group_by(location, variable, age_group) %>%
  summarise(population = sum(population), .groups = "drop") %>%
  rename(value = age_group) %>%
  mutate(variable = "ses_age_4Cat")

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
    variable = "ses_dwelling_cat"
  ) %>%
  group_by(location, variable, value) %>%
  summarise(population = sum(population), .groups = "drop")

# 3. Immigration cleaning
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
df_weights_income <- df_weights %>%
  filter(variable == "income") %>%
  mutate(
    value = case_when(
      value == "Under $10,000 (including loss)" ~ "no_income",
      value %in% c("$10,000 to $19,999", "$20,000 to $29,999") ~ "1_to_30000",
      value %in% c("$30,000 to $39,999", "$40,000 to $49,999", "$50,000 to $59,999") ~ "30001_to_60000",
      value %in% c("$60,000 to $69,999", "$70,000 to $79,999", "$80,000 to $89,999") ~ "60001_to_90000",
      value == "$90,000 to $99,999" ~ "90001_to_110000",
      value == "$100,000 to $149,999" ~ "110001_to_150000",
      value == "$150,000 and over" ~ "more_than_150000",
      TRUE ~ NA_character_
    ),
    variable = "ses_incomeCensus"
  ) %>%
  mutate(
    value = factor(
      value,
      levels = c(
        "no_income",
        "1_to_30000",
        "30001_to_60000",
        "60001_to_90000",
        "90001_to_110000",
        "110001_to_150000",
        "more_than_150000"
      )
    )
  ) %>%
  group_by(location, variable, value) %>%
  summarise(population = sum(population), .groups = "drop")

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
    variable = "ses_religionBigFive"
  ) %>%
  group_by(location, variable, value) %>%
  summarise(population = sum(population), .groups = "drop")

# SECTION MODIFIÉE: Education avec les catégories de df_ses
df_weights_education <- df_weights %>%
  filter(variable == "education") %>%
  mutate(
    value = case_when(
      # educBUniv: Inférieur à l'université
      value %in% c("No certificate, diploma or degree", 
                   "High (secondary) school diploma or equivalency certificate") ~ "educBUniv",
      
      # educPostHS: Études post-secondaires non universitaires
      value %in% c("Non-apprenticeship trades certificate or diploma",
                  "Apprenticeship certificate",
                  "College, CEGEP or other non-university certificate or diploma",
                  "University certificate or diploma below bachelor level") ~ "educPostHS",
      
      # educUniv: Études universitaires
      value %in% c("Bachelor's degree", 
                   "University certificate or diploma above bachelor level",
                   "Master's degree", 
                   "Degree in medicine, dentistry, veterinary medicine or optometry",
                   "Earned doctorate") ~ "educUniv",
      
      TRUE ~ NA_character_
    ),
    variable = "ses_educ_3Cat"
  ) %>%
  group_by(location, variable, value) %>%
  summarise(population = sum(population), .groups = "drop")

# Immigrant - déjà fait plus haut, mais on garde pour la cohérence
df_weights_immigration <- df_weights %>%
  filter(variable == "immigration") %>%
  mutate(
    value = case_when(
      value == "Non-immigrants" ~ "0",
      value == "Immigrants" ~ "1",
      TRUE ~ NA_character_
    ),
    variable = "ses_immigrant"
  ) %>%
  mutate(
    value = factor(
      value,
      levels = c("0", "1")
    )
  ) %>%
  group_by(location, variable, value) %>%
  summarise(population = sum(population), .groups = "drop")

# Language
df_weights_language <- df_weights %>%
  filter(variable == "language") %>%
  mutate(
    value = case_when(
      value == "English" ~ "english",
      value == "French" ~ "french",
      value == "Non-official languages" ~ "other",
      TRUE ~ NA_character_
    ),
    variable = "ses_language"
  ) %>%
  mutate(
    value = factor(
      value,
      levels = c("english", "french", "other")
    )
  ) %>%
  group_by(location, variable, value) %>%
  summarise(population = sum(population), .groups = "drop")

# Province
df_weights_province <- df_weights %>%
  filter(variable == "province") %>%
  mutate(
    variable = "ses_province"
  ) %>%
  group_by(location, variable, value) %>%
  summarise(
    population = sum(population),
    .groups = "drop"
  )

# SECTION MODIFIÉE: Sexe/genre avec ajout des catégories manquantes
# D'abord créer le dataframe avec les valeurs existantes
df_weights_gender_base <- df_weights %>%
  filter(variable == "sex") %>%
  mutate(
    value = case_when(
      value == "female" ~ "female",
      value == "male" ~ "male",
      TRUE ~ NA_character_
    ),
    variable = "ses_gender"
  ) %>%
  group_by(location, variable, value) %>%
  summarise(
    population = sum(population),
    .groups = "drop"
  )

# Calculer la population totale pour estimer les proportions
total_pop <- sum(df_weights_gender_base$population)

# Ajouter les catégories manquantes avec des estimations
gender_additional <- tibble(
  location = "can",
  variable = "ses_gender",
  value = c("non_binary", "queer", "trans_man", "trans_woman", "agender"),
  # Estimations basées sur des proportions de la population
  population = c(
    round(total_pop * 0.0033),  # non_binary (0,33% de la population)
    round(total_pop * 0.001),   # queer (0,1% - estimation)
    round(total_pop * 0.0006),  # trans_man (0,06% - estimation)
    round(total_pop * 0.0004),  # trans_woman (0,04% - estimation)
    round(total_pop * 0.0002)   # agender (0,02% - estimation)
  )
)

# Combiner les catégories existantes et nouvelles
df_weights_gender <- bind_rows(df_weights_gender_base, gender_additional)

# Marital status
df_weights_marital <- df_weights %>%
  filter(variable == "marital") %>%
  mutate(
    value = case_when(
      value == "Married" ~ "married",
      value == "Living common-law" ~ "common_law_relationship",
      value == "Never married" ~ "single",
      value %in% c("Separated", "Divorced") ~ "divorced_separated",
      value == "Widowed" ~ "widower_widow",
      TRUE ~ NA_character_
    ),
    variable = "ses_matStatus"
  ) %>%
  mutate(
    value = factor(
      value,
      levels = c(
        "common_law_relationship",
        "divorced_separated",
        "married",
        "single",
        "widower_widow"
      )
    )
  ) %>%
  group_by(location, variable, value) %>%
  summarise(
    population = sum(population),
    .groups = "drop"
  )

# Owner
df_weights_owner <- df_weights %>%
  filter(variable == "owner") %>%
  mutate(
    value = case_when(
      value == "Owner" ~ "owner",
      value == "Renter" ~ "tenant",
      value == "Dwelling provided by the local government, First Nation or Indian band" ~ "neither",
      TRUE ~ NA_character_
    ),
    variable = "ses_owner"
  ) %>%
  mutate(
    value = factor(
      value,
      levels = c("neither", "owner", "tenant")
    )
  ) %>%
  group_by(location, variable, value) %>%
  summarise(
    population = sum(population),
    .groups = "drop"
  )

# Pop totale
df_weights_poptot <- df_weights %>%
  filter(variable == "total_population")

# Combine all cleaned variables
df_weights_clean <- bind_rows(
  df_weights_age,
  df_weights_dwelling,
  df_weights_religion,
  df_weights_education,  # Version modifiée
  df_weights_gender,     # Version modifiée
  df_weights_language,
  df_weights_immigration,
  df_weights_income,
  df_weights_province,
  df_weights_marital,
  df_weights_owner,
  df_weights_poptot
)

saveRDS(df_weights_clean, "data/cancensus/df_weights_clean.rds")
