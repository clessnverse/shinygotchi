# R/modules/plot_builder/server.R
library(shiny)
library(ggplot2)
library(dplyr)
library(foreign)

# Source helper functions
source("R/modules/plot_builder/helpers/data_processing.R")
source("R/modules/plot_builder/helpers/plot_generation.R")
source("R/modules/plot_builder/helpers/color_management.R")

plotBuilderServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Data Input Handling ----------------------------------------
    dataInput <- reactive({
      req(input$datafile)
      read_data_file(input$datafile)
    })

    # Update UI based on loaded data ----------------------------
    observeEvent(dataInput(), {
      df <- dataInput()
      updateSelectInput(session, "xVar", choices = names(df))
      updateSelectInput(session, "yVar", choices = names(df))
      updateSelectInput(session, "fillVar", 
                       choices = c("None", names(df)), 
                       selected = "None")
      updateSelectInput(session, "facetVar", 
                       choices = c("None", names(df)), 
                       selected = "None")
    })

    # Factor Variable Handling ----------------------------------
    is_factor <- reactive({
      req(input$yVar)
      df <- dataInput()
      req(df)
      is.factor(df[[input$yVar]])
    })

    output$factorUI <- renderUI({
      req(input$yVar)
      df <- dataInput()
      req(df)
      
      if(is_factor()) {
        levels_without_na <- levels(df[[input$yVar]])
        levels_without_na <- levels_without_na[!is.na(levels_without_na)]
        
        tagList(
          radioButtons(ns("factorHandling"), 
                      "How to handle factor variable:",
                      choices = c("Binarize" = "binary",
                                "Scale Numerically" = "scale")),
          
          conditionalPanel(
            condition = sprintf("input['%s'] == 'binary'", ns("factorHandling")),
            selectizeInput(
              ns("levelsSelected"),
              "Select levels to consider as '1':",
              choices = levels_without_na,
              multiple = TRUE
            )
          ),
          
          conditionalPanel(
            condition = sprintf("input['%s'] == 'scale'", ns("factorHandling")),
            tagList(
              lapply(seq_along(levels_without_na), function(i) {
                level <- levels_without_na[i]
                n_choices <- length(levels_without_na)
                values <- seq(0, 1, length.out = n_choices)
                selectInput(
                  inputId = ns(paste0("scale_", i)),
                  label = paste("Select value for", level, ":"),
                  choices = values,
                  selected = values[i]
                )
              })
            )
          )
        )
      }
    })

    # Color Management -----------------------------------------
    output$colorSelector <- renderUI({
      req(input$fillVar != "None")
      df <- dataInput()
      create_color_inputs(ns, input$fillVar, df)
    })

    # Plot Generation -----------------------------------------
    plot_and_code <- reactive({
      req(dataInput())
      df <- dataInput()
      req(input$xVar, input$yVar)
      
      # Process data based on plot type
      processed_data <- prepare_plot_data(
        df = df,
        plot_type = input$plotType,
        x_var = input$xVar,
        y_var = input$yVar,
        fill_var = if(input$fillVar != "None") input$fillVar else NULL,
        operation = input$operation
      )
      
      # Get color values if fill variable is selected
      color_values <- if(input$fillVar != "None") {
        fill_values <- unique(df[[input$fillVar]])
        get_color_values(input, fill_values)
      } else NULL
      
      # Create plot based on type
      if(input$plotType == "bar") {
        p <- create_bar_plot(
          data = processed_data,
          x_var = input$xVar,
          y_var = "wrangled_y",
          fill_var = input$fillVar,
          facet_var = if(input$facetVar != "None") input$facetVar else NULL,
          color_values = color_values
        )
      } else {
        p <- create_scatter_plot(
          data = processed_data,
          x_var = input$xVar,
          y_var = input$yVar,
          color_var = if(input$fillVar != "None") input$fillVar else NULL,
          facet_var = if(input$facetVar != "None") input$facetVar else NULL,
          color_values = color_values
        )
      }
      
      # Generate code
      code <- generate_plot_code(
        plot_type = input$plotType,
        processed_data = processed_data,
        x_var = input$xVar,
        y_var = input$yVar,
        fill_var = input$fillVar,
        facet_var = input$facetVar,
        color_values = color_values
      )
      
      list(plot = p, code = code)
    })

    # Outputs ------------------------------------------------
    output$mainPlot <- renderPlot({
      plot_obj <- plot_and_code()$plot
      
      if (!is.null(input$baseSize)) {
        plot_obj + theme(text = element_text(size = input$baseSize))
      } else {
        plot_obj
      }
    })

    output$plotCode <- renderText({
      plot_and_code()$code
    })

    # Download Handler ---------------------------------------
    output$downloadPlot <- downloadHandler(
      filename = function() {
        paste("plot-", Sys.Date(), ".png", sep="")
      },
      content = function(file) {
        plot_obj <- plot_and_code()$plot
        
        if (!is.null(input$baseSize)) {
          plot_obj <- plot_obj + 
            theme(text = element_text(size = input$baseSize * 6))
        }
        
        ggsave(
          filename = file,
          plot = plot_obj,
          width = input$plotWidth,
          height = input$plotHeight,
          units = "in",
          dpi = 300
        )
      }
    )
  })
}
