library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(tidyr)

# Source the module
source("R/modules/plot_builder/plotBuilderModule.R")
source("R/modules/groups/groupsModule.R")

# Define the UI
ui <- navbarPage(
  "Datagotchi Data Explorer",
   
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
