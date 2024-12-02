# plotBuilderServer.R

library(shiny)
library(ggplot2)
library(dplyr)
library(foreign)
library(png)
library(grid)
library(cowplot)

# Source helper functions
source("R/modules/plot_builder/helpers/helpers.R")

# Module Server
plotBuilderServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive expression to read the uploaded data
    dataInput <- reactive({
      req(input$datafile)
      read_uploaded_file(input$datafile)
    })

    # Update variable selections when data is loaded
    observeEvent(dataInput(), {
      df <- dataInput()
      updateSelectInput(session, "xVar", choices = names(df))
      updateSelectInput(session, "yVar", choices = names(df))
      updateSelectInput(session, "fillVar", choices = c("None", names(df)), selected = "None")
      updateSelectInput(session, "facetVar", choices = c("None", names(df)), selected = "None")
    })

    # Determine if Y variable is a factor
    is_factor <- reactive({
      req(input$yVar)
      df <- dataInput()
      is.factor(df[[input$yVar]])
    })

    # Dynamic UI for factor handling
    output$factorUI <- renderUI({
      req(input$yVar)
      df <- dataInput()
      if (is_factor()) {
        generate_factor_ui(ns, df[[input$yVar]])
      }
    })

    # Display operation info
    output$operationInfo <- renderText({
      display_operation_info(input$operation)
    })

    # Reactive expression for plot and code
    plot_and_code <- reactive({
      req(dataInput())
      generate_plot_and_code(dataInput(), input)
    })

    # Render the plot
    output$mainPlot <- renderPlot({
      plot_obj <- plot_and_code()$plot
      if (!is.null(input$baseSize)) {
        plot_obj <- plot_obj + theme(text = element_text(size = input$baseSize))
      }
      plot_obj
    })

    # Render the code
    output$plotCode <- renderText({
      plot_and_code()$code
    })

    # Download handler for the plot
    output$downloadPlot <- downloadHandler(
      filename = function() {
        paste("plot-", Sys.Date(), ".png", sep="")
      },
      content = function(file) {
        ggsave(
          filename = file,
          plot = export_plot(plot_and_code()$plot, input$baseSize),
          width = input$plotWidth,
          height = input$plotHeight,
          units = "in",
          dpi = 300
        )
      }
    )
  })
}
