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
      
    )
  )
}


socialGroupsServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    lifestyle_vars <- list(
      # Activities
      "Vote choice" = "dv_vote_choice", # Barplot x = social_var, y = summarise(n = n() / nrow(data)), fill = dv_vote_choice. Ne pas oublier de mettre les couleurs officielles. LO
      "Left vs Right" = "dv_attitude_leftvsright", # Dotplot avec coordflip Etienne
      "Turnout" = "dv_turnout", # dotplot avec coordflip Etienne
      "Hunting" = "lifestyle_hunting_freq_numeric", # barplot LO
      "Manual Tasks" = "lifestyle_manual_tasks_freq_numeric", # barplot avec art Etienne 
      "Art" = "lifestyle_performing_arts_freq_numeric", # barplot avec manual Etienne
      "Transport" = "lifestyle_choice_transport_clean", # Même que vote choice LO 
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
    
    create_plot <- function(social_var, lifestyle_var, plot_type) {
      plot_data <- data %>%
        select(all_of(c(social_var, lifestyle_var))) %>%
        drop_na()
      
      p <- ggplot(plot_data) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      switch(plot_type,
        "bar" = {
          p + 
            geom_bar(aes(x = .data[[social_var]], 
                        fill = .data[[lifestyle_var]]), 
                    position = "fill") +
            labs(y = "Proportion", 
                 x = names(which(choices == social_var)),
                 title = names(which(lifestyle_vars == lifestyle_var)))
        },
        "box" = {
          p +
            geom_boxplot(aes(x = .data[[social_var]], 
                            y = .data[[lifestyle_var]])) +
            coord_flip() +
            labs(title = names(which(lifestyle_vars == lifestyle_var)))
        },
        "violin" = {
          p +
            geom_violin(aes(x = .data[[social_var]], 
                           y = .data[[lifestyle_var]])) +
            coord_flip() +
            labs(title = names(which(lifestyle_vars == lifestyle_var)))
        },
        "histogram" = {
          p +
            geom_histogram(aes(x = .data[[lifestyle_var]])) +
            facet_wrap(vars(.data[[social_var]]), scales = "free_y") +
            labs(title = names(which(lifestyle_vars == lifestyle_var)))
        }
      )
    }
    
    output$plot_container <- renderUI({
      plot_output_list <- lapply(seq_along(lifestyle_vars), function(i) {
        plotname <- paste0("plot", i)
        plotOutput(session$ns(plotname), height = "400px")
      })
      
      do.call(tagList, plot_output_list)
    })
    
    observe({
      for (i in seq_along(lifestyle_vars)) {
        local({
          local_i <- i
          plotname <- paste0("plot", local_i)
          
          output[[plotname]] <- renderPlot({
            create_plot(input$social_var, 
                       lifestyle_vars[[local_i]], 
                       input$plot_type)
          })
        })
      }
    })
  })
}

# Example usage in app.R:
# ui <- fluidPage(
#   socialGroupsUI("social_explorer")
# )
# 
# server <- function(input, output, session) {
#   socialGroupsServer("social_explorer", your_data)
# }
# 
# shinyApp(ui, server)
# Module UI



