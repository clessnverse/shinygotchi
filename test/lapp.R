library(shiny)
library(ggplot2)
library(dplyr)



# Define the UI
library(shiny)
library(ggplot2)
library(dplyr)

# Define the UI
ui <- navbarPage(
  "Datagotchi Data Explorer",
  
  # First tab: Plot Selector
  tabPanel("Plot Builder",
           sidebarLayout(
             sidebarPanel(
               # Add the file input control here
               fileInput("datafile", "Upload Data File",
                         accept = c(".csv", ".rds", ".sav")),
               
               # Select plot type
               radioButtons(
                 "plotType",
                 "Choose Plot Type:",
                 choices = c("Bar Plot" = "bar", "Scatter Plot" = "scatter"),
                 selected = "bar"
               ),
               
               # Variable selection inputs
               selectInput("xVar", "X Variable:", choices = NULL),
               selectInput("yVar", "Y Variable:", choices = NULL),
               selectInput("fillVar", "Fill Variable (optional):", choices = c("None"), selected = "None"),
               selectInput("facetVar", "Facet Wrap Variable (optional):", choices = c("None"), selected = "None"),
               
               # Data wrangling options for bar plot
               conditionalPanel(
                 condition = "input.plotType == 'bar'",
                 selectInput("operation", "Operation on Y Variable:",
                             choices = c("Mean" = "mean", "Proportion" = "prop")),
                 textOutput("operationInfo"),
                 
                 # Conditional panel for factor variables
                 uiOutput("factorUI")
               ),
               
               # Add show code checkbox
               checkboxInput("showCode", "Show ggplot code", value = FALSE)
             ),
             
             mainPanel(
               plotOutput("mainPlot"),
               # Add verbatim text output for the code
               conditionalPanel(
                 condition = "input.showCode == true",
                 tags$div(
                   style = "background-color: #f5f5f5; padding: 15px; border-radius: 5px; margin-top: 20px;",
                   tags$p("R code to reproduce this plot:"),
                   verbatimTextOutput("plotCode")
                 )
               )
             )
           )
  ),
  
  # Second tab: Display Geyser Gif
  tabPanel("Geyser Gif",
           div(
             img(src = "https://upload.wikimedia.org/wikipedia/commons/7/7a/Old_Faithful_Geyser_at_Yellowstone_National_Park.gif",
                 alt = "Old Faithful Geyser", style = "max-width:100%; height:auto;")
           )
  )
)

# Define the server logic
server <- function(input, output, session) {
  # Load the 'foreign' package for reading .sav files
  library(foreign)
  
  # Reactive expression to read the uploaded data
  dataInput <- reactive({
    req(input$datafile)
    ext <- tools::file_ext(input$datafile$name)
    switch(ext,
           csv = read.csv(input$datafile$datapath),
           rds = readRDS(input$datafile$datapath),
           sav = read.spss(input$datafile$datapath, to.data.frame = TRUE),
           {
             showNotification("Invalid file; Please upload a .csv, .rds, or .sav file", type = "error")
             validate("Invalid file; Please upload a .csv, .rds, or .sav file")
           }
    )
  })
  
  # Observe when dataInput() changes and update selectInput choices
  observeEvent(dataInput(), {
    df <- dataInput()
    # Update variable selection inputs
    updateSelectInput(session, "xVar", choices = names(df))
    updateSelectInput(session, "yVar", choices = names(df))
    updateSelectInput(session, "fillVar", choices = c("None", names(df)), selected = "None")
    updateSelectInput(session, "facetVar", choices = c("None", names(df)), selected = "None")
  })
  
  # Reactive value to track if selected Y variable is a factor
  is_factor <- reactive({
    req(input$yVar)
    df <- dataInput()
    req(df)
    is.factor(df[[input$yVar]])
  })
  
  # Dynamic UI for factor handling
  output$factorUI <- renderUI({
    req(input$yVar)
    df <- dataInput()
    req(df)
    if(is_factor()) {
      levels_without_na <- levels(df[[input$yVar]])
      levels_without_na <- levels_without_na[!is.na(levels_without_na)]
      
      tagList(
        radioButtons("factorHandling", "How to handle factor variable:",
                     choices = c("Binarize" = "binary",
                                 "Scale Numerically" = "scale")),
        
        # UI elements for binarization
        conditionalPanel(
          condition = "input.factorHandling == 'binary'",
          selectizeInput(
            "levelsSelected",
            "Select levels to consider as '1':",
            choices = levels_without_na,
            multiple = TRUE
          )
        ),
        
        # UI elements for numerical scaling
        conditionalPanel(
          condition = "input.factorHandling == 'scale'",
          tagList(
            lapply(seq_along(levels_without_na), function(i) {
              level <- levels_without_na[i]
              n_choices <- length(levels_without_na)
              values <- seq(0, 1, length.out = n_choices)
              selectInput(
                inputId = paste0("scale_", i),
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
  
  # Function to create numerical scaling for factors
  create_numerical_scale <- function(factor_values, value_mapping) {
    # Convert factor to numeric using mapping
    numeric_values <- value_mapping[as.character(factor_values)]
    numeric_values[is.na(factor_values)] <- NA
    return(numeric_values)
  }
  
  # Dynamically display information about the selected operation
  output$operationInfo <- renderText({
    if (input$operation == "mean") {
      "The bar plot will display the mean of the Y variable for each group."
    } else if (input$operation == "prop") {
      "The bar plot will display the proportion of the Y variable for each group."
    } else {
      ""
    }
  })
  
  # Create a reactive expression for the plot and its code
  plot_and_code <- reactive({
    req(dataInput())
    df <- dataInput()
    req(input$xVar, input$yVar)
    data_to_use <- df
    
    # Step 2: Handle factor Y variable
    if (input$plotType == "bar" && is_factor()) {
      if (!is.null(input$factorHandling)) {
        if (input$factorHandling == "binary") {
          req(input$levelsSelected)
          
          # Create the data transformation code
          data_transform_code <- sprintf(
            'data_to_use <- data_to_use %%>%%\n  mutate(processed_y = if_else(%s %%in%% c("%s"), 1, 0, missing = NA))',
            input$yVar,
            paste(input$levelsSelected, collapse = '", "')
          )
          
          # Execute the transformation
          data_to_use <- data_to_use %>%
            mutate(
              processed_y = if_else(
                .data[[input$yVar]] %in% input$levelsSelected,
                1, 0,
                missing = NA
              )
            )
          
          y_var_for_calculation <- "processed_y"
        } else if (input$factorHandling == "scale") {
          # Get levels and mapping for numerical scaling
          levels_without_na <- levels(df[[input$yVar]])
          levels_without_na <- levels_without_na[!is.na(levels_without_na)]
          
          value_mapping <- sapply(seq_along(levels_without_na), function(i) {
            as.numeric(input[[paste0("scale_", i)]])
          })
          names(value_mapping) <- levels_without_na
          
          # Create the data transformation code
          mapping_code <- paste(
            sprintf('c("%s" = %s', names(value_mapping), value_mapping),
            collapse = ", "
          )
          mapping_code <- paste0(mapping_code, ")")
          
          data_transform_code <- sprintf(
            'value_mapping <- %s\ndata_to_use <- data_to_use %%>%%\n  mutate(processed_y = value_mapping[as.character(%s)])',
            mapping_code,
            input$yVar
          )
          
          # Execute the transformation
          data_to_use <- data_to_use %>%
            mutate(
              processed_y = create_numerical_scale(
                .data[[input$yVar]], 
                value_mapping
              )
            )
          
          y_var_for_calculation <- "processed_y"
        }
      } else {
        y_var_for_calculation <- input$yVar
        data_transform_code <- NULL
      }
    } else {
      y_var_for_calculation <- input$yVar
      data_transform_code <- NULL
    }
    
    if (input$plotType == "bar") {
      # Prepare grouping variables
      group_vars <- list(sym(input$xVar))
      if (input$fillVar != "None") {
        group_vars <- append(group_vars, sym(input$fillVar))
      }
      
      # Create grouping variables string for code
      group_vars_code <- input$xVar
      if (input$fillVar != "None") {
        group_vars_code <- paste(group_vars_code, input$fillVar, sep = ", ")
      }
      
      # Data wrangling code and execution
      wrangle_code <- sprintf(
        'wrangled_data <- data_to_use %%>%%\n  filter(!is.na(%s)) %%>%%\n  filter(!is.na(%s))',
        input$xVar, y_var_for_calculation
      )
      
      if (input$fillVar != "None") {
        wrangle_code <- paste0(wrangle_code, sprintf(' %%>%%\n  filter(!is.na(%s))', input$fillVar))
      }
      
      wrangle_code <- paste0(
        wrangle_code,
        sprintf(
          ' %%>%%\n  group_by(%s) %%>%%\n  summarise(wrangled_y = %s(%s, na.rm = TRUE), .groups = "drop")',
          group_vars_code,
          if(input$operation == "mean") "mean" else "function(x) sum(x, na.rm = TRUE) / n()",
          y_var_for_calculation
        )
      )
      
      # Execute the wrangling
      wrangled_data <- eval(parse(text = wrangle_code))
      
      # Create plot and code
      p <- ggplot(wrangled_data) +
        aes(x = .data[[input$xVar]], 
            y = wrangled_y,
            fill = if (input$fillVar != "None") .data[[input$fillVar]] else NULL,
            group = if (input$fillVar != "None") .data[[input$fillVar]] else NULL) +
        geom_bar(
          stat = "identity",
          position = position_dodge(preserve = "single", width = 0.9),
          width = 0.8
        ) +
        theme_minimal() +
        labs(x = input$xVar, 
             y = if (input$operation == "mean") paste("Mean", input$yVar) else paste("Proportion", input$yVar),
             fill = input$fillVar)
      
      plot_code <- sprintf(
        'ggplot(wrangled_data) +\n  aes(x = %s, y = wrangled_y%s) +\n  geom_bar(stat = "identity", position = position_dodge(preserve = "single", width = 0.9), width = 0.8) +\n  theme_minimal() +\n  labs(x = "%s", y = "%s"%s)',
        input$xVar,
        if(input$fillVar != "None") sprintf(', fill = %s, group = %s', input$fillVar, input$fillVar) else "",
        input$xVar,
        if(input$operation == "mean") paste("Mean", input$yVar) else paste("Proportion", input$yVar),
        if(input$fillVar != "None") sprintf(', fill = "%s"', input$fillVar) else ""
      )
      
      if (input$facetVar != "None") {
        p <- p + facet_wrap(vars(.data[[input$facetVar]]))
        plot_code <- paste0(plot_code, sprintf(' +\n  facet_wrap(~%s)', input$facetVar))
      }
      
    } else if (input$plotType == "scatter") {
      # Create scatter plot and code
      p <- ggplot(data_to_use, aes(x = .data[[input$xVar]], y = .data[[input$yVar]])) +
        geom_point()
      
      plot_code <- sprintf(
        'ggplot(data_to_use, aes(x = %s, y = %s%s)) +\n  geom_point() +\n  theme_minimal() +\n  labs(x = "%s", y = "%s"%s)',
        input$xVar,
        input$yVar,
        if(input$fillVar != "None") sprintf(', color = %s', input$fillVar) else "",
        input$xVar,
        input$yVar,
        if(input$fillVar != "None") sprintf(', color = "%s"', input$fillVar) else ""
      )
      
      if (input$fillVar != "None") {
        p <- p + aes(color = .data[[input$fillVar]]) +
          labs(color = input$fillVar)
      }
      
      p <- p + theme_minimal() +
        labs(x = input$xVar, y = input$yVar)
      
      if (input$facetVar != "None") {
        p <- p + facet_wrap(vars(.data[[input$facetVar]]))
        plot_code <- paste0(plot_code, sprintf(' +\n  facet_wrap(~%s)', input$facetVar))
      }
    }
    
    # Combine all code pieces
    full_code <- c(
      "# Load required libraries",
      "library(dplyr)",
      "library(ggplot2)",
      "",
      if(!is.null(data_transform_code)) c("# Transform data", data_transform_code, ""),
      if(input$plotType == "bar") c("# Wrangle data", wrangle_code, ""),
      "# Create plot",
      plot_code
    )
    
    list(
      plot = p,
      code = paste(full_code, collapse = "\n")
    )
  })
  
  # Render the plot
  output$mainPlot <- renderPlot({
    plot_and_code()$plot
  })
  
  # Render the code
  output$plotCode <- renderText({
    plot_and_code()$code
  })
}


# Run the app
shinyApp(ui = ui, server = server)
