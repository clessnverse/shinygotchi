library(shiny)
library(ggplot2)
library(dplyr)

df <- readRDS("df.rds")  # Load your dataset

# Define the UI
ui <- navbarPage(
  "Datagotchi Data Explorer",
  
  # First tab: Plot Selector
  tabPanel("Plots",
           sidebarLayout(
             sidebarPanel(
               # Select plot type
               radioButtons(
                 "plotType",
                 "Choose Plot Type:",
                 choices = c("Bar Plot" = "bar", "Scatter Plot" = "scatter"),
                 selected = "bar"
               ),
               
               # Variable selection inputs
               selectInput("xVar", "X Variable:", choices = names(df)),
               selectInput("yVar", "Y Variable:", choices = names(df)),
               selectInput("fillVar", "Fill Variable (optional):", choices = c("None", names(df)), selected = "None"),
               selectInput("facetVar", "Facet Wrap Variable (optional):", choices = c("None", names(df)), selected = "None"),
               
               # Data wrangling options for bar plot
               conditionalPanel(
                 condition = "input.plotType == 'bar'",
                 selectInput("operation", "Operation on Y Variable:",
                             choices = c("Mean" = "mean", "Proportion" = "prop")),
                 textOutput("operationInfo"),
                 
                 # Conditional panel to binarize factor variables
                 uiOutput("binarizeUI")
               )
             ),
             
             mainPanel(
               plotOutput("mainPlot")  # Render the main plot
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
  
  # Dynamically generate UI for binarizing factor variables
  output$binarizeUI <- renderUI({
    if (is.factor(df[[input$yVar]])) {
      selectizeInput(
        "levelsSelected",
        paste("Select levels of", input$yVar, "to consider as '1':"),
        choices = levels(df[[input$yVar]]),
        multiple = TRUE
      )
    }
  })
  
  # Render the main plot
  output$mainPlot <- renderPlot({
    req(input$xVar, input$yVar)  # Ensure variables are selected
    
    # Step 1: Prepare data
    data_to_use <- df  # Start with the original data
    
    # Step 2: Handle binarization of factor Y variable
    if (input$plotType == "bar" && is.factor(df[[input$yVar]])) {
      req(input$levelsSelected)  # Ensure levels are selected for binarization
      data_to_use <- data_to_use %>%
        mutate(
          binarized_y = if_else(
            df[[input$yVar]] %in% input$levelsSelected,
            1, 0,
            missing = 0  # Default missing values to 0
          )
        )
      y_var_for_calculation <- "binarized_y"
    } else {
      y_var_for_calculation <- input$yVar
    }
    
    # Step 3: Wrangle data if Bar Plot is selected
    if (input$plotType == "bar") {
      # Prepare grouping variables
      group_vars <- list(sym(input$xVar))
      if (input$fillVar != "None") {
        group_vars <- append(group_vars, sym(input$fillVar))
      }
      if (input$facetVar != "None") {
        group_vars <- append(group_vars, sym(input$facetVar))
      }
      
      # Summarize data
      wrangled_data <- data_to_use %>%
        group_by(!!!group_vars) %>%
        summarise(
          wrangled_y = if (input$operation == "mean") {
            mean(.data[[y_var_for_calculation]], na.rm = TRUE)
          } else if (input$operation == "prop") {
            sum(.data[[y_var_for_calculation]], na.rm = TRUE) / n()
          },
          .groups = "drop"  # Avoid nested grouping
        )
      
      # Create bar plot with explicit position dodge
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
      
      # Add faceting if selected
      if (input$facetVar != "None") {
        p <- p + facet_wrap(vars(.data[[input$facetVar]]))
      }
      
    } else if (input$plotType == "scatter") {
      # Create scatter plot
      p <- ggplot(data_to_use, aes(x = .data[[input$xVar]], y = .data[[input$yVar]])) +
        geom_point()
      
      # Add color aesthetic if fill variable is selected
      if (input$fillVar != "None") {
        p <- p + aes(color = .data[[input$fillVar]]) +
          labs(color = input$fillVar)
      }
      
      p <- p + theme_minimal() +
        labs(x = input$xVar, y = input$yVar)
      
      # Add faceting if selected
      if (input$facetVar != "None") {
        p <- p + facet_wrap(vars(.data[[input$facetVar]]))
      }
    }
    
    # Step 5: Print the plot
    print(p)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
