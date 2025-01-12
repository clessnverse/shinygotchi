library(cancensus)
library(dplyr)

# Assurez-vous d'avoir défini votre clé API au préalable
# set_cancensus_api_key("<votre_api_key>", install = TRUE)

population_par_province <- get_census(
  dataset = "CA21",              # Recensement 2021
  regions = list(C = "01"),      # "01" = Canada au complet
  vectors = c("v_CA21_1"),       # v_CA21_1 = Population, 2021
  level = "PR",                  # PR = Provinces/Territoires
  geo_format = NA,               # Pas de géométrie, juste un df
  use_cache = TRUE               # On peut utiliser le cache
)

df_pop_clean <- population_par_province %>%
  select(`Region Name`, `v_CA21_1: Population, 2021`) %>%  # Garder uniquement les colonnes nécessaires
  rename(
    province = `Region Name`,              # Renommer "Region Name" en "province"
    population_2021 = `v_CA21_1: Population, 2021`  # Renommer "v_CA21_1: Population, 2021" en "population_2021"
  ) %>%
  mutate(
    province = case_when(
      province == "Alberta (Alta.)" ~ "AB",
      province == "British Columbia (B.C.)" ~ "BC",
      province == "Manitoba (Man.)" ~ "MB",
      province == "New Brunswick (N.B.)" ~ "NB",
      province == "Newfoundland and Labrador (N.L.)" ~ "NL",
      province == "Northwest Territories (N.W.T.)" ~ "NT",
      province == "Nova Scotia (N.S.)" ~ "NS",
      province == "Nunavut (Nvt.)" ~ "NU",
      province == "Ontario (Ont.)" ~ "ON",
      province == "Prince Edward Island (P.E.I.)" ~ "PE",
      province == "Quebec (Que.)" ~ "QC",
      province == "Saskatchewan (Sask.)" ~ "SK",
      province == "Yukon (Y.T.)" ~ "YT",
      TRUE ~ province  # Par défaut, garder la valeur originale si non correspondante (pour éviter des erreurs)
    ),
    country = "can"  # Ajouter la colonne "country"
  ) %>%
  mutate(location = country, variable = "province", value = province, population = population_2021) %>%
  select(location, variable, value, population)

# Save the data
saveRDS(df_pop_clean, "data/cancensus/province.rds")
