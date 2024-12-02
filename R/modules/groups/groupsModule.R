# UI Module
socialGroupsUI <- function(id) {
  ns <- NS(id)
  
  sidebarLayout(
    sidebarPanel( 
        selectInput(ns("social_var"), "Select Social Group Variable:",
          choices = c(
            "Age Groups" = "ses_age_group",
            "Gender" = "ses_gender_factor",
            "Geographic Location" = "ses_province",
            "Language" = "ses_language",
            "Socioeconomic Status" = "ses_income",
            "Education" = "ses_education_group",
            "Ethnicity" = "ses_ethnicity",
            "Religious Affiliation" = "ses_religiosity",
            "Housing Status" = "ses_owner",
            "Religious Groups" = "ses_religion",
            "Sexual Orientation" = "ses_orientation_factor"
          )
        )
    ),
    
    mainPanel(
      plotOutput(ns("plot_vote_choice")),
      plotOutput(ns("plot_turnout")) 
    )
  )
}

# Server Module
socialGroupsServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    # Debug print when module starts
    cat("Module server initialized\n")
    
    # Read data with error checking
    df_social_groups <- tryCatch({
      data <- readRDS("data/df_canada.rds")
      cat("Data loaded successfully. Dimensions:", dim(data), "\n")
      data
    }, error = function(e) {
      cat("Error loading data:", e$message, "\n")
      NULL
    })
    
    # Print the first few rows and column names for debugging
    cat("First few rows of data:\n")
    print(head(df_social_groups))
    cat("\nColumn names:\n")
    print(names(df_social_groups))
    
    lifestyle_vars <- list(
      "Vote choice" = "dv_vote_choice",
      "Left vs Right" = "dv_attitude_leftvsright",
      "Turnout" = "dv_turnout",
      "Hunting" = "lifestyle_hunting_freq_numeric",
      "Manual Tasks" = "lifestyle_manual_tasks_freq_numeric",
      "Art" = "lifestyle_performing_arts_freq_numeric",
      "Transport" = "lifestyle_choice_transport_clean"
    )
    
    social_vars <- list(
      "Age Groups" = "ses_age_group",
      "Gender" = "ses_gender_factor",
      "Geographic Location" = "ses_region",
      "Language" = "ses_language",
      "Socioeconomic Status" = "ses_income",
      "Education" = "ses_education_group",
      "Ethnicity" = "ses_ethnicity",
      "Religious Affiliation" = "ses_religiosity",
      "Housing Status" = "ses_owner",
      "Religious Groups" = "ses_religion",
      "Sexual Orientation" = "ses_orientation_factor"
    )
    
    # Add reactive observer for input changes
    observeEvent(input$social_var, {
      cat("Selected social variable:", input$social_var, "\n")
    })
   
    output$plot_vote_choice <- renderPlot({
      # Validate input
      req(input$social_var)
      req(df_social_groups)
      
      cat("Starting to create plot for:", input$social_var, "\n")
      
      # Calculate proportions with error handling
      plot_data <- tryCatch({
        data <- df_social_groups %>%
          group_by(!!sym(input$social_var), dv_vote_choice) %>%
          summarise(count = n(), .groups = 'drop') %>%
          group_by(!!sym(input$social_var)) %>%
          mutate(proportion = count / sum(count)) %>%
          ungroup()
        
        cat("Plot data created. Dimensions:", dim(data), "\n")
        data
      }, error = function(e) {
        cat("Error creating plot data:", e$message, "\n")
        NULL
      })
      
      req(plot_data)
      
      # Create and print plot
      p <- ggplot(data = plot_data, 
             aes(x = !!sym(input$social_var), 
                 y = proportion, 
                 fill = dv_vote_choice)) +
        geom_bar(stat = "identity", position = "dodge") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = "Vote Choice by Social Group",
             x = "Social Group",
             y = "Proportion",
             fill = "Vote Choice") +
        scale_y_continuous(labels = scales::percent)
      
      cat("Plot created successfully\n")
      print(p)
    })
     # Ajouter le renderPlot pour le taux de participation
     output$plot_turnout <- renderPlot({
      # Validate input
      req(input$social_var)
      req(df_social_groups)
      
      cat("Starting to create turnout plot for:", input$social_var, "\n")
      
      # Convertir dv_turnout en variable binaire si nécessaire
      if (!"dv_turnout_binary" %in% names(df_social_groups)) {
        df_social_groups <- df_social_groups %>%
          mutate(dv_turnout_binary = ifelse(dv_turnout >= 0.5, "Yes", "No"))
      }
      
      # Calculate turnout rate by social group
      plot_data_turnout <- tryCatch({
        data <- df_social_groups %>%
          filter(!is.na(!!sym(input$social_var)), !is.na(dv_turnout_binary)) %>%
          group_by(!!sym(input$social_var)) %>%
          summarise(
            turnout_rate = mean(dv_turnout_binary == "Yes", na.rm = TRUE),
            .groups = 'drop'
          )
        
        cat("Turnout plot data created. Dimensions:", dim(data), "\n")
        data
      }, error = function(e) {
        cat("Error creating turnout plot data:", e$message, "\n")
        NULL
      })
      
      req(plot_data_turnout)
      
      # Create and print turnout dot plot
      p_turnout <- ggplot(data = plot_data_turnout, 
                          aes(x = reorder(!!sym(input$social_var), turnout_rate), 
                              y = turnout_rate)) +
        geom_point(size = 3, color = "blue") +
        coord_flip() +
        theme_minimal() +
        labs(title = "Turnout by Social Group",
             x = "Social Group",
             y = "Turnout Rate") +
        scale_y_continuous(labels = scales::percent)
      
      cat("Turnout plot created successfully\n")
      print(p_turnout)
    })
  })
}
