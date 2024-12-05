# List of required packages
my_packages = c(
  "remotes",
  "ggplot2",
  "dplyr",
  "tidyr",
  "scales",
  "shiny",
  "shinydashboard",
  "shinyjs",
  "waiter",
  "shinycssloaders",
  "fresh",
  "bslib",
  "fontawesome"
)

# Function to install missing packages
install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

# Install base packages
invisible(sapply(my_packages, install_if_missing))

# Install clessnverse from GitHub
tryCatch({
  if (!require(clessnverse)) {
    remotes::install_github(
      "clessnverse/clessnverse",
      ref = "main",
      upgrade = "never",
      dependencies = TRUE,
      auth_token = Sys.getenv("GITHUB_PAT")
    )
  }
}, error = function(e) {
  message("Error installing clessnverse: ", e$message)
  message("Trying alternative installation method...")
  
  # Alternative installation method
  devtools::install_github(
    "clessnverse/clessnverse",
    ref = "main",
    force = TRUE
  )
})

# Load the package
library(clessnverse)
