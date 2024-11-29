# R/modules/plot_builder/ui.R
library(shiny)
library(colourpicker)  # Add this for the new color feature

plotBuilderUI <- function(id) {
  ns <- NS(id)
  
  sidebarLayout(
    sidebarPanel(
      fileInput(ns("datafile"), "Upload Data File",
                accept = c(".csv", ".rds", ".sav")),
      
      # Plot type selection
      radioButtons(
        ns("plotType"),
        "Choose Plot Type:",
        choices = c("Bar Plot" = "bar", "Scatter Plot" = "scatter"),
        selected = "bar"
      ),
      
      # Variable selection inputs
      selectInput(ns("xVar"), "X Variable:", choices = NULL),
      selectInput(ns("yVar"), "Y Variable:", choices = NULL),
      selectInput(ns("fillVar"), "Fill Variable (optional):", 
                 choices = c("None", NULL), selected = "None"),
      
      # Color selector UI (new feature)
      conditionalPanel(
        condition = sprintf("input['%s'] != 'None'", ns("fillVar")),
        uiOutput(ns("colorSelector"))
      ),
      
      selectInput(ns("facetVar"), "Facet Wrap Variable (optional):", 
                 choices = c("None", NULL), selected = "None"),
      
      # Rest of your UI components...
    ),
    mainPanel(
      plotOutput(ns("mainPlot")),
      conditionalPanel(
        condition = sprintf("input['%s']", ns("showCode")),
        verbatimTextOutput(ns("plotCode"))
      )
    )
  )
}
