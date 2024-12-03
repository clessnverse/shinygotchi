library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(tidyr)
library(slickR)

# Source the module
source("R/modules/plot_builder/plotBuilderModule.R")
source("R/modules/groups/groupsModule.R")

# Define the UI
ui <- navbarPage(
  title = "Datagotchi Data Explorer",
    
  # Inclure la police dans l'UI
  header = tagList(
    tags$head(
      tags$style(HTML("
        @import url('https://fonts.googleapis.com/css2?family=VT323&display=swap');

/* Application de la police VT323 à l'ensemble du contenu */
body, label, input, button, select, textarea, h1, h2, h3, h4, h5, h6 {
  font-family: 'VT323', cursive, sans-serif;
  font-size: 150%;
        }
      "))
    )
  ),

  tabPanel("Groups",
    socialGroupsUI("social_explorer")
  ), 
  tabPanel("Issues",
  ),
    tabPanel("Lifestyles",
  ),
  tabPanel("Parties",
  ),
  tabPanel("Ridings",
  ),

  # First tab: Plot Builder
  tabPanel("Plot Builder",
           plotBuilderUI("plot_builder")
  ),

)

# Define the server logic
server <- function(input, output, session) {
  plotBuilderServer("plot_builder")
  socialGroupsServer("social_explorer", data)
}

# Run the app
shinyApp(ui = ui, server = server)
