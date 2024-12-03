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
library(sass)
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
  background: linear-gradient(135deg, #2C3E50, #3498DB);
  color: white;
  padding: 20px;
  border-radius: 10px;
  margin-bottom: 20px;
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
    title = div(
      tags$img(src = "datagotchi.png", height = "30px", style = "margin-right: 10px; vertical-align: middle;"),
      "Datagotchi Data Explorer",
      class = "animate-fade-in"
    ),
    
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
              h2("Social Groups Analysis"),
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
}

# Create custom SCSS file (save as www/custom.scss)
writeLines('
// Custom SCSS styles
$font-family-base: "Poppins", sans-serif;

// Card styles
.card {
  backdrop-filter: blur(10px);
  background: rgba(255, 255, 255, 0.9);
}

// Custom scrollbar
::-webkit-scrollbar {
  width: 8px;
}

::-webkit-scrollbar-track {
  background: #f1f1f1;
  border-radius: 4px;
}

::-webkit-scrollbar-thumb {
  background: #2C3E50;
  border-radius: 4px;
}

// Tooltip customization
.tooltip {
  font-family: $font-family-base;
  font-size: 0.9rem;
}

// Form control styling
.form-control {
  border-radius: 8px;
  border: 1px solid #e0e0e0;
  transition: all 0.3s ease;
  
  &:focus {
    box-shadow: 0 0 0 0.2rem rgba(44, 62, 80, 0.25);
    border-color: #2C3E50;
  }
}

// Progress bar animation
.progress-bar {
  transition: width 0.5s ease-in-out;
}
', "www/custom.scss")

# Run the application
shinyApp(ui = ui, server = server)
