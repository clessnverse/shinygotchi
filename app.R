# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(rlang)

# Define UI
ui <- fluidPage(
  titlePanel("Data Visualization App"),

  sidebarLayout(
    sidebarPanel(
      # File input to load data
      fileInput('datafile', 'Load data file (RDS)', accept = c('.rds')),

      # Dynamic selectors for variables
      uiOutput('xvar_select'),
      uiOutput('yvar_select'),
      uiOutput('fillvar_select'),

      # Checkbox for coord_flip()
      checkboxInput('coord_flip', 'Apply coord_flip()', value = FALSE),

      hr(),
      h4("Data Wrangling"),

      # Selectors for grouping variables and summarise function
      uiOutput('group_vars_ui'),
      selectInput('summary_func', 'Summarise Function', choices = c('Mean' = 'mean', 'Sum' = 'sum', 'Count' = 'count')),

      # Button to generate the plot
      actionButton('generate_plot', 'Generate Plot')
    ),

    mainPanel(
      # Output plot
      plotOutput('plot')
    )
  )
)

# Define Server
server <- function(input, output, session) {

  # Reactive value to store data
  data <- reactiveVal(NULL)

  # Observe when a file is uploaded
  observeEvent(input$datafile, {
    req(input$datafile)
    infile <- input$datafile
    ext <- tools::file_ext(infile$name)
    if (ext == "rds") {
      df <- readRDS(infile$datapath)
      data(df)
      showNotification("Data loaded successfully.", type = "message")
    } else {
      showNotification("Please upload an RDS file.", type = "error")
      data(NULL)
    }
  })

  # Generate dynamic UI inputs based on the data
  observeEvent(data(), {
    req(data())
    cols <- names(data())

    output$xvar_select <- renderUI({
      selectInput('xvar', 'X Variable', choices = cols)
    })

    output$yvar_select <- renderUI({
      selectInput('yvar', 'Y Variable', choices = cols)
    })

    output$fillvar_select <- renderUI({
      selectInput('fillvar', 'Fill Variable', choices = c('None', cols))
    })

    output$group_vars_ui <- renderUI({
      selectInput('group_vars', 'Group By Variables', choices = cols, multiple = TRUE)
    })
  })

  # Generate the plot based on user input
  observeEvent(input$generate_plot, {
    req(data())
    req(input$xvar)
    req(input$yvar)

    df <- data()

    # Convert input strings to symbols for tidy evaluation
    xvar_sym <- sym(input$xvar)
    yvar_sym <- sym(input$yvar)
    fillvar_sym <- if (input$fillvar == 'None') NULL else sym(input$fillvar)

    # Handle grouping variables
    group_vars <- input$group_vars
    group_syms <- syms(group_vars)

    # Data wrangling: grouping and summarizing
    df_wrangle <- df

    if (length(group_syms) > 0) {
      df_wrangle <- df_wrangle %>%
        group_by(!!!group_syms)
    }

    # Summarise based on the selected function
    if (input$summary_func == 'mean') {
      df_wrangle <- df_wrangle %>%
        summarise(y_value = mean(!!yvar_sym, na.rm = TRUE))
    } else if (input$summary_func == 'sum') {
      df_wrangle <- df_wrangle %>%
        summarise(y_value = sum(!!yvar_sym, na.rm = TRUE))
    } else if (input$summary_func == 'count') {
      df_wrangle <- df_wrangle %>%
        summarise(y_value = n())
    } else {
      showNotification("Invalid summarise function", type = "error")
      return()
    }

    # Ungroup the data after summarising
    df_wrangle <- df_wrangle %>% ungroup()

    # Generate the plot
    output$plot <- renderPlot({
      p <- ggplot(df_wrangle, aes(x = !!xvar_sym, y = y_value))

      if (!is.null(fillvar_sym)) {
        p <- p + aes(fill = !!fillvar_sym)
      }

      p <- p + geom_bar(stat = 'identity', position = 'dodge')

      if (input$coord_flip) {
        p <- p + coord_flip()
      }

      p + theme_minimal() +
        labs(x = input$xvar, y = input$yvar)
    })
  })
}

# Run the Shiny App
shinyApp(ui = ui, server = server)

