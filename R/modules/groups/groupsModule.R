socialGroupsUI <- function(id) {
  ns <- NS(id)
  
  sidebarLayout(
    sidebarPanel( 
        selectInput(ns("social_var"), "Select Social Group Variable:",
          choices = c(
            "Age Groups" = "ses_age_group",
            "Gender" = "ses_female",
            "Geographic Location" = "ses_state",
            "Language" = "ses_language_english",
            "Socioeconomic Status" = "ses_income_large_groups",
            "Education" = "ses_education",
            "Ethnicity" = "ses_ethnicity",
            "Religious Affiliation" = "ses_religiosity_importance",
            "Sexual Orientation" = "ses_sexual_orientation_heterosexual",
            "Housing Status" = "ses_ownership",
            "Religious Groups" = "ses_religion_factor"
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
      "Yoga" = "lifestyle_yoga_freq_factor",
      "Fishing" = "lifestyle_fishing_freq_factor",
      "Hunting" = "lifestyle_hunting_freq_factor",
      "Non-motorized Activities" = "lifestyle_nonmotorized_freq_factor",
      "Motorized Activities" = "lifestyle_motorized_freq_factor",
      "Social Volunteering" = "lifestyle_volunteeringsoc_freq_factor",
      "Video Gaming" = "lifestyle_videogame_freq_factor",
      # Transportation
      "Transport Type" = "lifestyle_transport",
      "Vehicle Preference" = "lifestyle_prius_pickup",
      # Food & Drink
      "Meat Consumption" = "lifestyle_meat_consumption_freq_factor",
      "Coffee Habits" = "lifestyle_coffee",
      "Meal Preferences" = "lifestyle_classic_meal",
      "Alcohol Preference" = "lifestyle_alcohol_favorite_grouped",
      # Entertainment
      "Barbie/Oppenheimer" = "lifestyle_movie_barbenheimer",
      "Baseball Watching" = "lifestyle_freq_watch_baseball",
      "Soccer Watching" = "lifestyle_freq_watch_soccer",
      # Other
      "Clothing Style" = "lifestyle_clothing_style_grouped",
      "Gun Ownership" = "lifestyle_guns_number_factor"
    )

    social_vars <- list(
      "Age Groups" = "ses_age_group",
      "Gender" = "ses_female",
      "Geographic Location" = "ses_state",
      "Language" = "ses_language_english",
      "Socioeconomic Status" = "ses_income_large_groups",
      "Education" = "ses_education",
      "Ethnicity" = "ses_ethnicity",
      "Religious Affiliation" = "ses_religiosity_importance",
      "Sexual Orientation" = "ses_sexual_orientation_heterosexual",
      "Housing Status" = "ses_ownership",
      "Religious Groups" = "ses_religion_factor"
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



