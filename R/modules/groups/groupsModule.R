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
      plotOutput(ns("plot_vote_choice")) 
    )
  )
}

# Server Module
socialGroupsServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    # Read data with error checking
    df_social_groups <- readRDS("data/df_canada.rds")
    
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
      # Calculate proportions with error handling
      data <- df_social_groups %>%
          filter(!is.na(!!sym(input$social_var)), !is.na(dv_vote_choice)) %>%
          group_by(!!sym(input$social_var), dv_vote_choice) %>%
          summarise(count = n(), .groups = 'drop') %>%
          group_by(!!sym(input$social_var)) %>%
          mutate(proportion = count / sum(count)) %>%
          drop_na() %>%
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
        scale_y_continuous(labels = scales::percent)
      
      print(p)
    })
  })
}
