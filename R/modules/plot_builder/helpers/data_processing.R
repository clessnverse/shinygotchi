# R/modules/plot_builder/helpers/data_processing.R
library(dplyr)
library(foreign)

read_data_file <- function(file) {
  ext <- tools::file_ext(file$name)
  
  df <- switch(ext,
         csv = read.csv(file$datapath),
         rds = readRDS(file$datapath),
         sav = read.spss(file$datapath, to.data.frame = TRUE),
         stop("Invalid file type")
  )
  
  # Convert character columns to factors for consistency
  df %>% mutate(across(where(is.character), as.factor))
}

prepare_plot_data <- function(df, x_var, y_var, fill_var = NULL, operation = "mean") {
  # Your data wrangling logic here
  # This will depend on plot type and other parameters
}
