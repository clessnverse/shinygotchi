# plotBuilderModule.R

library(shiny)
library(ggplot2)
library(dplyr)

# Module UI
plotBuilderUI <- function(id) {
  ns <- NS(id)
  
  sidebarLayout(
    sidebarPanel(
        ),
    
    mainPanel(

    )
  )
}

# Module Server
plotBuilderServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Load necessary packages
  library(foreign)
  library(png)
  library(grid)
  library(cowplot)

  })
}
