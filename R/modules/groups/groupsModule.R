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
      plotOutput(ns("plot_turnout")),
      plotOutput(ns("plot_left_vs_right")) 
    )
  )
}

# Server Module
socialGroupsServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    # Read data with error checking
    df_social_groups <- readRDS("data/df_canada.rds")

    canadian_party_colors <- c(
      "lpc" = "#d71920",
      "cpc" = "#182c54",
      "ndp" = "#F58220",
      "bq" = "#00aeef",
      "gpc" = "#3D9B35",
      "other" = "#a6a6a6"
    )
    
    lifestyle_vars <- list(
      "Vote choice" = "dv_vote_choice", # Barplot LO
      "Left vs Right" = "dv_attitude_leftvsright", # Dotplot avec coord_flip Etienne
      "Turnout" = "dv_turnout", # Dotplot avec coord_flip Etienne
      "Hunting" = "lifestyle_hunting_freq_numeric", # Barplot LO
      "Manual Tasks" = "lifestyle_manual_tasks_freq_numeric", # Barplot combiné avec Art Etienne
      "Art" = "lifestyle_performing_arts_freq_numeric", # Barplot combiné avec Manual Etiebbe
      "Transport" = "lifestyle_choice_transport_clean" # Meme que vote choice LO
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

      # Valid parties
      valid_parties <- (names(canadian_party_colors))
      df_social_groups$dv_vote_choice[!(df_social_groups$dv_vote_choice %in% valid_parties)] <- "other"

      # Calculate proportions with error handling
      data <- df_social_groups %>%
          filter(!is.na(!!sym(input$social_var)), !is.na(dv_vote_choice)) %>%
          group_by(!!sym(input$social_var), dv_vote_choice) %>%
          summarise(count = n(), .groups = 'drop') %>%
          group_by(!!sym(input$social_var)) %>%
          mutate(proportion = count / sum(count)) %>%
          ungroup()

      # Create and print plot
      p <- ggplot(data = data, 
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
        scale_y_continuous(labels = scales::percent) +
        scale_fill_manual(values = canadian_party_colors)
      
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

    output$plot_left_vs_right <- renderPlot({
      # Validate input
      req(input$social_var)
      req(df_social_groups)
      
      cat("Starting to create Left vs Right plot for:", input$social_var, "\n")
      
      # Vérifier que dv_attitude_leftvsright est disponible et numérique
      if (!"dv_attitude_leftvsright" %in% names(df_social_groups)) {
        cat("Variable dv_attitude_leftvsright not found in data.\n")
        return(NULL)
      }
      
      if (!is.numeric(df_social_groups$dv_attitude_leftvsright)) {
        cat("Variable dv_attitude_leftvsright is not numeric.\n")
        return(NULL)
      }
      
      # Calculer la moyenne de dv_attitude_leftvsright par groupe social
      plot_data_left_right <- tryCatch({
        data <- df_social_groups %>%
          filter(!is.na(!!sym(input$social_var)), !is.na(dv_attitude_leftvsright)) %>%
          group_by(!!sym(input$social_var)) %>%
          summarise(
            mean_left_right = mean(dv_attitude_leftvsright, na.rm = TRUE),
            .groups = 'drop'
          )
        
        cat("Left vs Right plot data created. Dimensions:", dim(data), "\n")
        data
      }, error = function(e) {
        cat("Error creating Left vs Right plot data:", e$message, "\n")
        NULL
      })
      
      req(plot_data_left_right)
      
      # Créer le dot plot avec coord_flip
      p_left_right <- ggplot(data = plot_data_left_right, 
                             aes(x = reorder(!!sym(input$social_var), mean_left_right), 
                                 y = mean_left_right)) +
        geom_point(size = 3, color = "darkgreen") +
        coord_flip() +
        theme_minimal() +
        labs(title = "Left vs Right by Social Group",
             x = "Social Group",
             y = "Mean Left-Right Attitude (0 = Left, 1 = Right)") +
        scale_y_continuous(limits = c(0, 1))
      
      cat("Left vs Right plot created successfully\n")
      print(p_left_right)
    })
  }) 
}
