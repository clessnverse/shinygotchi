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
                 
                 # Conditional panel for factor variables
                 uiOutput("factorUI")
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
  
  # Reactive value to track if selected Y variable is a factor
  is_factor <- reactive({
    req(input$yVar)
    is.factor(df[[input$yVar]])
  })
  
  # Dynamic UI for factor handling
  output$factorUI <- renderUI({
    req(input$yVar)
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
          selectizeInput(
            "zeroLevel",
            "Select level for 0:",
            choices = levels_without_na
          ),
          selectizeInput(
            "oneLevel",
            "Select level for 1:",
            choices = levels_without_na
          ),
          radioButtons(
            "middleHandling",
            "How to handle middle values:",
            choices = c(
              "Equal spacing" = "equal",
              "Closer to 0" = "zero_weighted",
              "Closer to 1" = "one_weighted"
            )
          )
        )
      )
    }
  })
  
  # Function to create numerical scaling for factors
  create_numerical_scale <- function(factor_values, zero_level, one_level, middle_handling) {
    levels_without_na <- levels(factor_values)
    levels_without_na <- levels_without_na[!is.na(levels_without_na)]
    n_levels <- length(levels_without_na)
    
    # Create sequence based on middle handling preference
    middle_values <- switch(middle_handling,
                          "equal" = seq(0, 1, length.out = n_levels),
                          "zero_weighted" = {
                            x <- seq(0, 1, length.out = n_levels)
                            x^2  # Weight towards zero
                          },
                          "one_weighted" = {
                            x <- seq(0, 1, length.out = n_levels)
                            sqrt(x)  # Weight towards one
                          })
    
    # Create named vector for mapping
    zero_idx <- which(levels_without_na == zero_level)
    one_idx <- which(levels_without_na == one_level)
    
    # Reorder values to ensure 0 and 1 are in correct positions
    values <- middle_values
    values[zero_idx] <- 0
    values[one_idx] <- 1
    
    # Create mapping
    mapping <- setNames(values, levels_without_na)
    
    # Convert factor to numeric using mapping
    numeric_values <- mapping[as.character(factor_values)]
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
  
  # Render the main plot
  output$mainPlot <- renderPlot({
    req(input$xVar, input$yVar)  # Ensure variables are selected
    
    # Step 1: Prepare data
    data_to_use <- df  # Start with the original data
    
    # Step 2: Handle factor Y variable
    if (input$plotType == "bar" && is_factor()) {
      if (!is.null(input$factorHandling)) {  # Check if factorHandling exists
        if (input$factorHandling == "binary") {
          req(input$levelsSelected)  # Ensure levels are selected for binarization
          data_to_use <- data_to_use %>%
            mutate(
              processed_y = if_else(
                .data[[input$yVar]] %in% input$levelsSelected,
                1, 0,
                missing = NA  # Set missing values to NA
              )
            )
        } else if (input$factorHandling == "scale") {
          req(input$zeroLevel, input$oneLevel, input$middleHandling)
          data_to_use <- data_to_use %>%
            mutate(
              processed_y = create_numerical_scale(
                .data[[input$yVar]], 
                input$zeroLevel,
                input$oneLevel,
                input$middleHandling
              )
            )
        }
        y_var_for_calculation <- "processed_y"
      } else {
        y_var_for_calculation <- input$yVar
      }
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
      
      # Remove NA values and summarize data
      wrangled_data <- data_to_use %>%
        filter(!is.na(.data[[input$xVar]])) %>%  # Remove NAs from x variable
        filter(!is.na(.data[[y_var_for_calculation]])) %>%  # Remove NAs from y variable
        {if (input$fillVar != "None") filter(., !is.na(.data[[input$fillVar]])) else .} %>%  # Remove NAs from fill variable if present
        {if (input$facetVar != "None") filter(., !is.na(.data[[input$facetVar]])) else .} %>%  # Remove NAs from facet variable if present
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
    
    # Print the plot
    print(p)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
