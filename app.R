# app.R
library(shiny)
library(ggplot2)
library(dplyr)

# Source all helper functions
source("R/utils/theme_utils.R")

# Source module files
source("R/modules/plot_builder/ui.R")
source("R/modules/plot_builder/server.R")

# Define the UI
ui <- navbarPage(
  "Datagotchi Data Explorer",
  
  # First tab: Plot Builder
  tabPanel("Plot Builder",
           plotBuilderUI("plot_builder")
  ),
  
  # Second tab: Display Geyser Gif
  tabPanel("Geyser Gif",
           div(
             img(src = "https://upload.wikimedia.org/wikipedia/commons/7/7a/Old_Faithful_Geyser_at_Yellowstone_National_Park.gif",
                 alt = "Old Faithful Geyser", 
                 style = "max-width:100%; height:auto;")
           )
  )
)

# Define the server logic
server <- function(input, output, session) {
  plotBuilderServer("plot_builder")
}

# Run the app
shinyApp(ui = ui, server = server)
