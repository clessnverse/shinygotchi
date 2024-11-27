# server.R

# Load the necessary libraries for server-side processing
library(shiny)
library(ggplot2)
library(dplyr)
library(rlang)

# Define the server logic required to manipulate data and generate visualizations
server <- function(input, output, session) {
  
  # Reactive value to store the uploaded data
  data <- reactiveVal(NULL)
  
  # Observe when a new file is uploaded
  observeEvent(input$datafile, {
    req(input$datafile)  # Ensure a file has been uploaded
    infile <- input$datafile  # Get file input details
    ext <- tools::file_ext(infile$name)  # Get file extension
    if (ext == "rds") {
      # Read the RDS file and store it in reactive value 'data'
      df <- readRDS(infile$datapath)
      data(df)
      showNotification("Données importées avec succès.", type = "message")
    } else {
      # Show error if the file is not an RDS file
      showNotification("Veuillez importer un fichier RDS.", type = "error")
      data(NULL)
    }
  })
  
  # Reactive value to store data after mutations (new variables creation)
  mutated_data <- reactiveVal(NULL)
  
  # Observe when the 'update_vars' button is clicked to update variables
  observeEvent(input$update_vars, {
    req(data())  # Ensure data is available
    df <- data()
    
    # Apply manipulation expressions if provided
    if (!is.null(input$mutate_expr) && input$mutate_expr != "") {
      tryCatch({
        # Parse the input expressions
        exprs <- parse(text = input$mutate_expr)
        # Convert expressions to list and use mutate with unquote-splicing
        df <- df %>% mutate(!!!as.list(exprs))
        showNotification("Variables mises à jour avec succès.", type = "message")
      }, error = function(e) {
        # Show error notification if parsing fails
        showNotification(paste("Erreur dans l'expression de manipulation :", e$message), type = "error")
      })
    } else {
      # Show warning if no expressions are provided
      showNotification("Aucune expression de manipulation fournie.", type = "warning")
    }
    
    # Update the 'mutated_data' reactive value with the new data
    mutated_data(df)
  })
  
  # Reactive value to store filtered data
  filtered_data <- reactiveVal(NULL)
  
  # Observe when the 'update_filter' button is clicked to filter data
  observeEvent(input$update_filter, {
    req(mutated_data())  # Ensure mutated data is available
    df <- mutated_data()
    
    # Filter data if a filtering expression is provided
    if (!is.null(input$filter_expr) && input$filter_expr != "") {
      tryCatch({
        # Use the filtering expression to filter the data
        df <- df %>% filter(!!parse(text = input$filter_expr))
        showNotification("Données filtrées avec succès.", type = "message")
      }, error = function(e) {
        # Show error notification if parsing fails
        showNotification(paste("Erreur dans l'expression de filtrage :", e$message), type = "error")
      })
    } else {
      # Show warning if no filtering expression is provided
      showNotification("Aucune expression de filtrage fournie.", type = "warning")
    }
    
    # Update the 'filtered_data' reactive value with the filtered data
    filtered_data(df)
  })
  
  # Observe when the 'export_data' button is clicked to export data
  observeEvent(input$export_data, {
    req(filtered_data())  # Ensure filtered data is available
    export_path <- input$export_path  # Get the export path from user input
    tryCatch({
      # Save the filtered data as an RDS file to the specified path
      saveRDS(filtered_data(), export_path)
      showNotification(paste("Données exportées avec succès vers", export_path), type = "message")
    }, error = function(e) {
      # Show error notification if saving fails
      showNotification(paste("Erreur lors de l'exportation :", e$message), type = "error")
    })
  })
  
  # Render a preview of the manipulated data in the UI
  output$data_preview_manipulation <- renderPrint({
    req(filtered_data())  # Ensure filtered data is available
    head(filtered_data())  # Display the first few rows
  })
  
  # --- Visualization Tab ---
  
  # Reactive expression to get the data for visualization
  visual_data <- reactive({
    req(filtered_data())  # Ensure filtered data is available
    df <- filtered_data()
    return(df)
  })
  
  # Reactive values to store the choices for variables in select inputs
  group_vars_choices <- reactiveVal()
  summary_var_choices <- reactiveVal()
  weight_var_choices <- reactiveVal()
  
  # Observe changes in visual_data to update the choices in select inputs
  observeEvent(visual_data(), {
    req(visual_data())
    cols <- names(visual_data())  # Get column names of the data
    
    # Update the reactive values with the column names
    group_vars_choices(cols)
    summary_var_choices(cols)
    weight_var_choices(cols)
  })
  
  # Render UI for selecting grouping variables
  output$group_vars_ui <- renderUI({
    selectInput('group_vars', 'Variables de Regroupement', group_vars_choices(), multiple = TRUE)
  })
  
  # Render UI for selecting the variable to summarize
  output$summary_var_ui <- renderUI({
    selectInput('summary_var', 'Variable à Résumer', summary_var_choices())
  })
  
  # Render UI for selecting the weighting variable if weighted mean is selected
  output$weight_var_ui <- renderUI({
    if (input$summary_func == 'weighted.mean') {
      selectInput('weight_var', 'Variable de Pondération', weight_var_choices())
    }
  })
  
  # Reactive expression to get the summarized data for visualization
  summarized_data <- eventReactive(input$generate_plot, {
    df <- visual_data()  # Get the data for visualization
    
    req(input$summary_var)  # Ensure a summary variable is selected
    
    # Create a symbol for the summary variable (for tidy evaluation)
    summary_var_sym <- sym(input$summary_var)
    
    # Check if weighting variable is needed
    if (input$summary_func == 'weighted.mean') {
      req(input$weight_var)
      weight_var_sym <- sym(input$weight_var)
    }
    
    # Group the data if grouping variables are selected
    if (!is.null(input$group_vars) && length(input$group_vars) > 0) {
      df_grouped <- df %>% group_by(across(all_of(input$group_vars)))
    } else {
      df_grouped <- df
    }
    
    # Apply the selected summary function
    if (input$summary_func == 'mean') {
      # Calculate mean of the summary variable
      df_summary <- df_grouped %>%
        summarise(value = mean(as.numeric(!!summary_var_sym), na.rm = TRUE)) %>%
        ungroup()
    } else if (input$summary_func == 'sum') {
      # Calculate sum of the summary variable
      df_summary <- df_grouped %>%
        summarise(value = sum(as.numeric(!!summary_var_sym), na.rm = TRUE)) %>%
        ungroup()
    } else if (input$summary_func == 'n') {
      # Count the number of observations
      df_summary <- df_grouped %>%
        summarise(value = n()) %>%
        ungroup()
    } else if (input$summary_func == 'proportion') {
      # Calculate the proportion of a specific category
      
      # Ask the user to specify the category value
      showModal(modalDialog(
        title = "Valeur de la Catégorie",
        textInput("category_value", "Entrez la valeur de la catégorie pour calculer la proportion :", value = ""),
        easyClose = TRUE,
        footer = tagList(
          modalButton("Annuler"),
          actionButton("ok_proportion", "OK")
        )
      ))
      
      # Observe when the user provides the category value
      observeEvent(input$ok_proportion, {
        removeModal()  # Close the modal dialog
        category_value <- input$category_value  # Get the category value from user input
        
        # Calculate the proportion where the summary variable equals the category value
        df_summary <- df_grouped %>%
          summarise(value = mean(!!summary_var_sym == category_value, na.rm = TRUE)) %>%
          ungroup()
        
        # Update the 'summarized_data' reactive expression with the new summary data
        summarized_data <<- reactive({ df_summary })
        
        # Update the UI for selecting Y variable in the plot
        output$yvar_select <- renderUI({
          selectInput('yvar', 'Variable pour l\'axe Y', names(df_summary))
        })
        
        # Render the plot with the new summarized data
        output$plot <- renderPlot({
          req(input$xvar, input$yvar)
          plot_data <- df_summary
          req(plot_data)
          
          # Create the ggplot object with selected variables
          p <- ggplot(plot_data, aes(x = .data[[input$xvar]], y = .data[[input$yvar]]))
          
          # Add the appropriate geom based on selected plot type
