# plotBuilderUI.R

library(shiny)

# Module UI
plotBuilderUI <- function(id) {
  ns <- NS(id)
  
  sidebarLayout(
    sidebarPanel(
      fileInput(ns("datafile"), "Upload Data File",
                accept = c(".csv", ".rds", ".sav")),
      radioButtons(
        ns("plotType"),
        "Choose Plot Type:",
        choices = c("Bar Plot" = "bar", "Scatter Plot" = "scatter"),
        selected = "bar"
      ),
      selectInput(ns("xVar"), "X Variable:", choices = NULL),
      selectInput(ns("yVar"), "Y Variable:", choices = NULL),
      selectInput(ns("fillVar"), "Fill Variable (optional):", choices = NULL, selected = "None"),
      selectInput(ns("facetVar"), "Facet Wrap Variable (optional):", choices = NULL, selected = "None"),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'bar'", ns("plotType")),
        selectInput(ns("operation"), "Operation on Y Variable:",
                    choices = c("Mean" = "mean", "Proportion" = "prop")),
        textOutput(ns("operationInfo")),
        uiOutput(ns("factorUI"))
      ),
      checkboxInput(ns("showCode"), "Show ggplot code", value = FALSE),
      hr(),
      h4("Download Plot"),
      numericInput(ns("plotWidth"), "Plot Width (in inches):", value = 10, min = 1, max = 100),
      numericInput(ns("plotHeight"), "Plot Height (in inches):", value = 6, min = 1, max = 100),
      downloadButton(ns("downloadPlot"), "Download Plot"),
      numericInput(ns("baseSize"), "Base Text Size (points):", value = 12, min = 6, max = 100)
    ),
    mainPanel(
      plotOutput(ns("mainPlot")),
      conditionalPanel(
        condition = sprintf("input['%s']", ns("showCode")),
        tags$div(
          style = "background-color: #f5f5f5; padding: 15px; border-radius: 5px; margin-top: 20px;",
          tags$p("R code to reproduce this plot:"),
          verbatimTextOutput(ns("plotCode"))
        )
      )
    )
  )
}
