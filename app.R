# app.R
# Load required packages
library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(tidyr)
library(shinydashboard)
library(shinyjs)
library(waiter)
library(shinycssloaders)
library(fresh)
library(bslib)
library(fontawesome)

# Source modules
source("R/modules/plot_builder/plotBuilderModule.R")
source("R/modules/groups/groupsModule.R")

# Custom theme
my_theme <- bs_theme(
  version = 5,
  bootswatch = "pulse",
  primary = "#2C3E50",
  secondary = "#95A5A6",
  success = "#18BC9C",
  info = "#3498DB",
  warning = "#F39C12",
  danger = "#E74C3C"
) %>%
  bs_add_rules(sass::sass_file("www/custom.scss"))

# Custom CSS
custom_css <- "
.navbar {
  box-shadow: 0 2px 4px rgba(0,0,0,0.1);
}

.nav-tabs .nav-link.active {
  border-bottom: 3px solid #2C3E50;
  font-weight: bold;
}

.card {
  border-radius: 15px;
  box-shadow: 0 4px 6px rgba(0,0,0,0.1);
  transition: transform 0.2s;
}

.card:hover {
  transform: translateY(-5px);
}

.btn-primary {
  border-radius: 25px;
  padding: 8px 20px;
  text-transform: uppercase;
  letter-spacing: 1px;
  font-weight: 600;
}

#loading-screen {
  position: fixed;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
  background-color: rgba(255,255,255,0.9);
  display: flex;
  justify-content: center;
  align-items: center;
  z-index: 9999;
}

.title-box {
  background: white;
  color: black;
  padding: 20px;
  border-radius: 10px;
  margin-bottom: 20px;
  border: 1px solid black;
}

.animate-fade-in {
  animation: fadeIn 0.5s ease-in;
}

@keyframes fadeIn {
  from { opacity: 0; }
  to { opacity: 1; }
}
"

# UI Definition
ui <- tagList(
  useWaiter(),  # Loading screen
  useShinyjs(),
  tags$head(
    tags$style(custom_css),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Poppins:wght@300;400;500;600&display=swap")
  ),
  
navbarPage(
  title = tagList(
    tags$img(src = "https://raw.githubusercontent.com/clessnverse/shinygotchi/refs/heads/main/www/datagotchi.png", height = "30px"),
    span("Datagotchi Data Explorer")
  ),
    theme = my_theme,
    id = "nav",
    
    # Groups Tab
    tabPanel(
      "Groups",
      icon = icon("users"),
      div(
        class = "animate-fade-in",
        fluidRow(
          column(
            width = 12,
            div(
              class = "title-box",
              h2("Groupes sociaux"),
              p("Explore demographic and behavioral patterns across different social groups")
            )
          )
        ),
        socialGroupsUI("social_explorer")
      )
    ),
    
    # Issues Tab
    tabPanel(
      "Issues",
      icon = icon("comments"),
      div(class = "animate-fade-in",
        # Add your Issues content here
      )
    ),
    
    # Lifestyles Tab
    tabPanel(
      "Lifestyles",
      icon = icon("heart"),
      div(class = "animate-fade-in",
        # Add your Lifestyles content here
      )
    ),
    
    # Parties Tab
    tabPanel(
      "Parties",
      icon = icon("flag"),
      div(class = "animate-fade-in",
        # Add your Parties content here
      )
    ),
    
    # Ridings Tab
    tabPanel(
      "Ridings",
      icon = icon("map-marker-alt"),
      div(class = "animate-fade-in",
        # Add your Ridings content here
      )
    ),
    
    # Plot Builder Tab
    tabPanel(
      "Plot Builder",
      icon = icon("chart-bar"),
      div(
        class = "animate-fade-in",
        fluidRow(
          column(
            width = 12,
            div(
              class = "title-box",
              h2("Custom Plot Builder"),
              p("Create and customize your own data visualizations")
            )
          )
        ),
        plotBuilderUI("plot_builder")
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  # Show loading screen
  waiter_show(
    html = tagList(
      spin_flower(),
      h3("Loading Datagotchi Explorer...", style = "color: #2C3E50;")
    ),
    color = "#ffffff"
  )
  
  # Initialize modules
  plotBuilderServer("plot_builder")
  socialGroupsServer("social_explorer", data)
  
  # Hide loading screen after delay
  Sys.sleep(1)  # Simulate loading time
  waiter_hide()
  
  # Add tab switching animation
  observeEvent(input$nav, {
    shinyjs::runjs('
      $(".tab-pane").fadeOut(0);
      $(".tab-pane.active").fadeIn(500);
    ')
  })

  output$logo_image <- renderUI({
    tags$img(src = "datagotchi.png", height = "30px")
  })
}


# Run the application
shinyApp(ui = ui, server = server)
