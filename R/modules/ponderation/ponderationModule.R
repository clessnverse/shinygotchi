library(cancensus)
library(dplyr)

ages_variables <- c(
  "v_CA21_20", # 1 yo
  "v_CA21_23", # 2 yo
  "v_CA21_26", # 3 yo
  "v_CA21_29", # 4 yo
  "v_CA21_35", # 5 yo
  "v_CA21_38", # 6 yo
  "v_CA21_41", # 7 yo
  "v_CA21_44", # 8 yo
  "v_CA21_47", # 9 yo
  "v_CA21_53", # 10 yo
  "v_CA21_56", # 11 yo
  "v_CA21_59", # 12 yo
  "v_CA21_62", # 13 yo
  "v_CA21_65", # 14 yo
  "v_CA21_74", # 15 yo
  "v_CA21_77", # 16 yo
  "v_CA21_80", # 17 yo
  "v_CA21_83", # 18 yo
  "v_CA21_86", # 19 yo
  "v_CA21_92", # 20 yo
  "v_CA21_95", # 21 yo
  "v_CA21_98", # 22 yo
  "v_CA21_101", # 23 yo
  "v_CA21_104", # 24 yo
  "v_CA21_110", # 25 yo
  "v_CA21_113", # 26 yo
  "v_CA21_116", # 27 yo
  "v_CA21_119", # 28 yo
  "v_CA21_122", # 29 yo
  "v_CA21_128", # 30 yo
  "v_CA21_131", # 31 yo
  "v_CA21_134", # 32 yo
  "v_CA21_137", # 33 yo
  "v_CA21_140", # 34 yo
  "v_CA21_146", # 35 yo
  "v_CA21_149", # 36 yo
  "v_CA21_152", # 37 yo
  "v_CA21_155", # 38 yo
  "v_CA21_158", # 39 yo
  "v_CA21_164", # 40 yo
  "v_CA21_167", # 41 yo
  "v_CA21_170", # 42 yo
  "v_CA21_173", # 43 yo
  "v_CA21_176", # 44 yo
  "v_CA21_182", # 45 yo
  "v_CA21_185", # 46 yo
  "v_CA21_188", # 47 yo
  "v_CA21_191", # 48 yo
  "v_CA21_194", # 49 yo
  "v_CA21_200", # 50 yo
  "v_CA21_203", # 51 yo
  "v_CA21_206", # 52 yo
  "v_CA21_209", # 53 yo
  "v_CA21_212", # 54 yo
  "v_CA21_218", # 55 yo
  "v_CA21_221", # 56 yo
  "v_CA21_224", # 57 yo
  "v_CA21_227", # 58 yo
  "v_CA21_230", # 59 yo
  "v_CA21_236", # 60 yo
  "v_CA21_239", # 61 yo
  "v_CA21_242", # 62 yo
  "v_CA21_245", # 63 yo
  "v_CA21_248", # 64 yo
  "v_CA21_257", # 65 yo
  "v_CA21_260", # 66 yo
  "v_CA21_263", # 67 yo
  "v_CA21_266", # 68 yo
  "v_CA21_279", # 69 yo
  "v_CA21_275", # 70 yo
  "v_CA21_278", # 71 yo
  "v_CA21_281", # 72 yo
  "v_CA21_284", # 73 yo
  "v_CA21_287", # 74 yo
  "v_CA21_293", # 75 yo
  "v_CA21_296", # 76 yo
  "v_CA21_299", # 77 yo
  "v_CA21_302", # 78 yo
  "v_CA21_305", # 79 yo
  "v_CA21_311", # 80 yo
  "v_CA21_314", # 81 yo
  "v_CA21_317", # 82 yo
  "v_CA21_320", # 83 yo
  "v_CA21_323", # 84 yo
  "v_CA21_332", # 85 yo
  "v_CA21_335", # 86 yo
  "v_CA21_338", # 87 yo
  "v_CA21_341", # 88 yo
  "v_CA21_344", # 89 yo
  "v_CA21_350", # 90 yo
  "v_CA21_353", # 91 yo
  "v_CA21_356", # 92 yo
  "v_CA21_359", # 93 yo
  "v_CA21_362", # 94 yo
  "v_CA21_368", # 95 yo
  "v_CA21_371", # 96 yo
  "v_CA21_374", # 97 yo
  "v_CA21_377", # 98 yo
  "v_CA21_380" # 99 yo
)

# Returns a data frame with data only
df <- get_census(
  dataset = "CA21", regions = list(C = "1"),
  vectors = ages_variables,
  level = "C", use_cache = TRUE, geo_format = NA, quiet = TRUE
)

df_clean <- df %>%
  pivot_longer(cols = starts_with("v_"), names_to = "age", values_to = "population") %>%
  select(age, population) %>%
  mutate(age = sub(".*: ", "", age)) %>%
  mutate(country = "CA")

# Save the data
saveRDS(df_clean, "data/cancensus/ages.rds")
