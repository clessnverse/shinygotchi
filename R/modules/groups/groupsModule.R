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
            "Housing Status" = "ses_owner",
            "Religious Groups" = "ses_religion_big_five",
            "Sexual Orientation" = "ses_orientation_factor"
          )
        ),
        hr(),
        h4("Adjust Text Size"),
        actionButton(ns("decrease_text_size"), "-"),
        actionButton(ns("increase_text_size"), "+")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Vote Choice",
                 plotOutput(ns("plot_vote_choice"), height = "600px"),
                 downloadButton(ns("download_plot_vote_choice"), "Download Plot")
        ),
        tabPanel("Turnout",
                 plotOutput(ns("plot_turnout"), height = "600px"),
                 downloadButton(ns("download_plot_turnout"), "Download Plot")
        ),
        tabPanel("Left vs Right",
                 plotOutput(ns("plot_left_vs_right"), height = "600px"),
                 downloadButton(ns("download_plot_left_vs_right"), "Download Plot")
        ),
        tabPanel("Manual Tasks vs Art",
                 plotOutput(ns("plot_manual_vs_art"), height = "600px"),
                 downloadButton(ns("download_plot_manual_vs_art"), "Download Plot")
        ),
        tabPanel("Hunting",
                 plotOutput(ns("plot_hunting"), height = "600px"),
                 downloadButton(ns("download_plot_hunting"), "Download Plot")
        ),
        tabPanel("Transport",
                 plotOutput(ns("plot_transport"), height = "600px"),
                 downloadButton(ns("download_plot_transport"), "Download Plot")
        )
      )
    )
    
  )
}

# Server Module
socialGroupsServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive value for text size
    text_size <- reactiveVal(24)  # Default text size
    
    # Observers for text size adjustment
    observeEvent(input$increase_text_size, {
      text_size(text_size() + 1)
    })
    
    observeEvent(input$decrease_text_size, {
      new_size <- text_size() - 1
      if (new_size >= 8) {  # Minimum text size
        text_size(new_size)
      }
    })
    
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

    sns_colors <- c(
      "Facebook" = "#5064ac",
      "Instagram" = "#fd0183",
      "LinkedIn" = "#0077b5",
      "Pinterest" = "#ec0c2d",
      "Snapchat" = "#f6f600",
      "TikTok" = "#121212",
      "Twitter / X" = "#209cf4",
      "YouTube" = "#ff0033",
      "Autre (veuillez préciser)" = "#a6a6a6"
    )
    
    lifestyle_vars <- list(
      "Vote choice" = "dv_vote_choice",
      "Left vs Right" = "dv_attitude_leftvsright",
      "Turnout" = "dv_turnout",
      "Hunting" = "lifestyle_hunting_freq_numeric",
      "Manual Tasks" = "lifestyle_manual_task_freq_numeric",
      "Art" = "lifestyle_performing_arts_freq_numeric",
      "Transport" = "lifestyle_medsociaux_plus_frequent"
    )
    
    social_vars <- list(
      "Age Groups" = "ses_age_group",
      "Gender" = "ses_gender_factor",
      "Geographic Location" = "ses_region",
      "Language" = "ses_language",
      "Socioeconomic Status" = "ses_income",
      "Education" = "ses_education_group",
      "Ethnicity" = "ses_ethnicity",
      "Housing Status" = "ses_owner",
      "Religious Groups" = "ses_religion_big_five",
      "Sexual Orientation" = "ses_orientation_factor"
    )
    
    # Add reactive observer for input changes
    observeEvent(input$social_var, {
      cat("Selected social variable:", input$social_var, "\n")
    })
    
    # Function to apply the custom theme with dynamic text size
    custom_theme <- reactive({
      clessnize::theme_datagotchi_light(base_size = text_size())
    })

    # Définir les mappings pour chaque variable
    age_group_labels <- c(
      "18_24" = "18-24 ans",
      "25_44" = "25-44 ans",
      "45_64" = "45-64 ans",
      "65+"   = "65 ans et plus"
    )
    
    gender_labels <- c(
      "male"         = "Homme",
      "female"       = "Femme",
      "non_binary"   = "Non-binaire",
      "agender"      = "Agenre",
      "trans_man"    = "Homme trans",
      "trans_woman"  = "Femme trans",
      "queer"        = "Queer"
    )
    
    province_labels <- c(
      "NL" = "Terre-Neuve-et-Labrador",
      "PE" = "Île-du-Prince-Édouard",
      "NS" = "Nouvelle-Écosse",
      "NB" = "Nouveau-Brunswick",
      "QC" = "Québec",
      "ON" = "Ontario",
      "MB" = "Manitoba",
      "SK" = "Saskatchewan",
      "AB" = "Alberta",
      "BC" = "Colombie-Britannique",
      "YT" = "Yukon",
      "NT" = "Territoires du Nord-Ouest",
      "NU" = "Nunavut"
    )
    
    language_labels <- c(
      "english" = "Anglais",
      "french"  = "Français",
      "other"   = "Autre"
    )
    
    income_labels <- c(
      "no_income"          = "Aucun revenu",
      "1_to_30000"         = "1 à 30 000 $",
      "30001_to_60000"     = "30 001 à 60 000 $",
      "60001_to_90000"     = "60 001 à 90 000 $",
      "90001_to_110000"    = "90 001 à 110 000 $",
      "110001_to_150000"   = "110 001 à 150 000 $",
      "150001_to_200000"   = "150 001 à 200 000 $",
      "more_than_200000"   = "Plus de 200 000 $"
    )
    
    education_labels <- c(
      "educBHS"       = "Sans diplôme",
      "educHS"        = "DES",
      "educPostHS"    = "Postsecondaire",
      "educUnivBac"   = "Baccalauréat",
      "educUnivSup"   = "Universitaire supérieur"
    )
    
    ethnicity_labels <- c(
      "white"      = "Blanc",
      "black"      = "Noir",
      "indigenous" = "Autochtone",
      "asian"      = "Asiatique",
      "hispanic"   = "Hispanique",
      "arab"       = "Arabe",
      "other"      = "Autre"
    )
    
    owner_labels <- c(
      "owner"    = "Propriétaire",
      "tenant"   = "Locataire",
      "neither"  = "Ni l'un ni l'autre"
    )
    
    religion_labels <- c(
      "christian" = "Chrétien",
      "muslim"    = "Musulman",
      "hindu"     = "Hindou",
      "buddhist"  = "Bouddhiste",
      "jew"       = "Juif"
    )
    
    orientation_labels <- c(
      "heterosexual" = "Hétérosexuel",
      "gay"          = "Gai/Lesbienne",
      "bisexual"     = "Bisexuel",
      "other"        = "Autre"
    )
    
    # Créer une liste de mappings pour un accès facile
    variable_mappings <- list(
      "ses_age_group" = age_group_labels,
      "ses_gender_factor" = gender_labels,
      "ses_province" = province_labels,
      "ses_language" = language_labels,
      "ses_income" = income_labels,
      "ses_education_group" = education_labels,
      "ses_ethnicity" = ethnicity_labels,
      "ses_owner" = owner_labels,
      "ses_religion_big_five" = religion_labels,
      "ses_orientation_factor" = orientation_labels
    )

    # Fonction pour appliquer le mapping à une variable donnée
    apply_mapping <- function(data, variable_name) {
      if (variable_name %in% names(variable_mappings)) {
        mapping <- variable_mappings[[variable_name]]
        data[[variable_name]] <- recode(data[[variable_name]], !!!mapping)
      }
      return(data)
    }

    
    ### Plot: Vote Choice ###
    output$plot_vote_choice <- renderPlot({
      # Valid parties
      valid_parties <- names(canadian_party_colors)
      df_social_groups$dv_vote_choice[!(df_social_groups$dv_vote_choice %in% valid_parties)] <- "other"
 # Appliquer le mapping à la variable sociale sélectionnée
 df_social_groups <- apply_mapping(df_social_groups, input$social_var)
      # Calculate proportions
      data <- df_social_groups %>%
        filter(!is.na(.data[[input$social_var]]), !is.na(dv_vote_choice)) %>%
        group_by(!!sym(input$social_var), dv_vote_choice) %>%
        summarise(count = n(), .groups = 'drop') %>%
        group_by(!!sym(input$social_var)) %>%
        mutate(proportion = count / sum(count)) %>%
        ungroup()

      # Create plot
      p <- ggplot(data = data, 
                  aes(x = !!sym(input$social_var), 
                      y = proportion, 
                      fill = dv_vote_choice)) +
        geom_bar(stat = "identity", position = "dodge") +
        custom_theme() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = text_size()),
              axis.text.y = element_text(size = text_size()),
              legend.text = element_text(size = text_size()),
              legend.title = element_text(size = text_size()),
              plot.title = element_text(size = text_size() + 2),
              axis.title = element_text(size = text_size())) +
        labs(title = "Vote Choice by Social Group",
             x = "Social Group",
             y = "Proportion",
             fill = "Vote Choice") +
        scale_y_continuous(labels = scales::percent) +
        scale_fill_manual(values = canadian_party_colors)
      print(p)
    })
    
    output$download_plot_vote_choice <- downloadHandler(
      filename = function() {
        paste("plot_vote_choice", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        # Duplicate the plotting code here
        valid_parties <- names(canadian_party_colors)
        df_social_groups$dv_vote_choice[!(df_social_groups$dv_vote_choice %in% valid_parties)] <- "other"

         # Appliquer le mapping à la variable sociale sélectionnée
 df_social_groups <- apply_mapping(df_social_groups, input$social_var)

        data <- df_social_groups %>%
          filter(!is.na(.data[[input$social_var]]), !is.na(dv_vote_choice)) %>%
          group_by(!!sym(input$social_var), dv_vote_choice) %>%
          summarise(count = n(), .groups = 'drop') %>%
          group_by(!!sym(input$social_var)) %>%
          mutate(proportion = count / sum(count)) %>%
          ungroup()

        p <- ggplot(data = data, 
                    aes(x = !!sym(input$social_var), 
                        y = proportion, 
                        fill = dv_vote_choice)) +
          geom_bar(stat = "identity", position = "dodge") +
          clessnize::theme_datagotchi_light(base_size = text_size()) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, size = text_size()),
                axis.text.y = element_text(size = text_size()),
                legend.text = element_text(size = text_size()),
                legend.title = element_text(size = text_size()),
                plot.title = element_text(size = text_size() + 2),
                axis.title = element_text(size = text_size())) +
          labs(title = "Vote Choice by Social Group",
               x = "Social Group",
               y = "Proportion",
               fill = "Vote Choice") +
          scale_y_continuous(labels = scales::percent) +
          scale_fill_manual(values = canadian_party_colors)
        ggsave(file, plot = p, width = 16, height = 9, units = "in")
      }
    )
    
    ### Plot: Turnout ###
output$plot_turnout <- renderPlot({
  # Validate input
  req(input$social_var)
  req(df_social_groups)
  
           # Appliquer le mapping à la variable sociale sélectionnée
 df_social_groups <- apply_mapping(df_social_groups, input$social_var)
  # Convert dv_turnout to binary variable if necessary
  if (!"dv_turnout_binary" %in% names(df_social_groups)) {
    df_social_groups <- df_social_groups %>%
      mutate(dv_turnout_binary = ifelse(dv_turnout >= 0.5, "Yes", "No"))
  }
  
  # Calculate turnout rate by social group
  plot_data_turnout <- df_social_groups %>%
    filter(!is.na(.data[[input$social_var]]), !is.na(dv_turnout_binary)) %>%
    group_by(!!sym(input$social_var)) %>%
    summarise(
      turnout_rate = mean(dv_turnout_binary == "Yes", na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Create turnout bar plot with thin horizontal bars
  p_turnout <- ggplot(data = plot_data_turnout, 
                      aes(x = reorder(!!sym(input$social_var), turnout_rate), 
                          y = turnout_rate)) +
    geom_bar(stat = "identity", width = 0.4, fill = "tomato") +
    coord_flip() +
    custom_theme() +
    theme(axis.text.x = element_text(size = text_size()),
          axis.text.y = element_text(size = text_size()),
          plot.title = element_text(size = text_size() + 2),
          axis.title = element_text(size = text_size()),
          axis.ticks = element_blank()) +  # Optionnel : supprimer les graduations
    labs(title = "Turnout by Social Group",
         x = NULL,  # Supprimer le label de l'axe x car les groupes sociaux sont déjà indiqués
         y = "Turnout Rate (%)") +
    scale_y_continuous(labels = scales::percent)
  
  print(p_turnout)
})

    
output$download_plot_turnout <- downloadHandler(
  filename = function() {
    paste("plot_turnout", Sys.Date(), ".png", sep = "")
  },
  content = function(file) {
    # Duplicate the plotting code here
# Appliquer le mapping à la variable sociale sélectionnée
df_social_groups <- apply_mapping(df_social_groups, input$social_var)

    if (!"dv_turnout_binary" %in% names(df_social_groups)) {
      df_social_groups <- df_social_groups %>%
        mutate(dv_turnout_binary = ifelse(dv_turnout >= 0.5, "Yes", "No"))
    }
    
    plot_data_turnout <- df_social_groups %>%
      filter(!is.na(.data[[input$social_var]]), !is.na(dv_turnout_binary)) %>%
      group_by(!!sym(input$social_var)) %>%
      summarise(
        turnout_rate = mean(dv_turnout_binary == "Yes", na.rm = TRUE),
        .groups = 'drop'
      )
    
    p_turnout <- ggplot(data = plot_data_turnout, 
                        aes(x = reorder(!!sym(input$social_var), turnout_rate), 
                            y = turnout_rate)) +
      geom_bar(stat = "identity", width = 0.5, fill = "tomato") +
      coord_flip() +
      clessnize::theme_datagotchi_light(base_size = text_size()) +
      theme(axis.text.x = element_text(size = text_size()),
            axis.text.y = element_text(size = text_size()),
            plot.title = element_text(size = text_size() + 2),
            axis.title = element_text(size = text_size()),
            axis.ticks = element_blank()) +  # Optionnel : supprimer les graduations
      labs(title = "Turnout by Social Group",
           x = NULL,
           y = "Turnout Rate (%)") +
      scale_y_continuous(labels = scales::percent)
    
    ggsave(file, plot = p_turnout, width = 16, height = 9, units = "in")
  }
)

    
    ### Plot: Left vs Right ###
output$plot_left_vs_right <- renderPlot({
  req(input$social_var)
  req(df_social_groups)
           # Appliquer le mapping à la variable sociale sélectionnée
 df_social_groups <- apply_mapping(df_social_groups, input$social_var)
  
  if (!"dv_attitude_leftvsright" %in% names(df_social_groups)) {
    return(NULL)
  }
  
  if (!is.numeric(df_social_groups$dv_attitude_leftvsright)) {
    return(NULL)
  }
  
  plot_data_left_right <- df_social_groups %>%
    filter(!is.na(.data[[input$social_var]]), !is.na(dv_attitude_leftvsright)) %>%
    group_by(!!sym(input$social_var)) %>%
    summarise(
      mean_left_right = mean(dv_attitude_leftvsright, na.rm = TRUE),
      .groups = 'drop'
    )
  
  p_left_right <- ggplot(data = plot_data_left_right, 
                         aes(x = reorder(!!sym(input$social_var), mean_left_right), 
                             y = mean_left_right)) +
    geom_point(size = 3, color = "black") +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
    coord_flip() +
    custom_theme() +
    theme(axis.text.x = element_text(size = text_size()),
          axis.text.y = element_text(size = text_size()),
          plot.title = element_text(size = text_size() + 2),
          axis.title = element_text(size = text_size())) +
    labs(title = "Positionnement Gauche-Droite par Groupe Social",
         x = NULL,
         y = NULL) +
    scale_y_continuous(limits = c(0, 1),
                       breaks = c(0.25, 0.5, 0.75),
                       labels = c("Gauche", "Centre", "Droite"))
  print(p_left_right)
})

    
output$download_plot_left_vs_right <- downloadHandler(
  filename = function() {
    paste("plot_left_vs_right", Sys.Date(), ".png", sep = "")
  },
  content = function(file) {
    # Duplicate the plotting code here
    req(input$social_var)
    req(df_social_groups)

            # Appliquer le mapping à la variable sociale sélectionnée
 df_social_groups <- apply_mapping(df_social_groups, input$social_var)
    
    if (!"dv_attitude_leftvsright" %in% names(df_social_groups)) {
      return(NULL)
    }
    
    if (!is.numeric(df_social_groups$dv_attitude_leftvsright)) {
      return(NULL)
    }
    
    plot_data_left_right <- df_social_groups %>%
      filter(!is.na(.data[[input$social_var]]), !is.na(dv_attitude_leftvsright)) %>%
      group_by(!!sym(input$social_var)) %>%
      summarise(
        mean_left_right = mean(dv_attitude_leftvsright, na.rm = TRUE),
        .groups = 'drop'
      )
    
    p_left_right <- ggplot(data = plot_data_left_right, 
                           aes(x = reorder(!!sym(input$social_var), mean_left_right), 
                               y = mean_left_right)) +
      geom_point(size = 3, color = "black") +
      geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
      coord_flip() +
      clessnize::theme_datagotchi_light(base_size = text_size()) +
      theme(axis.text.x = element_text(size = text_size()),
            axis.text.y = element_text(size = text_size()),
            plot.title = element_text(size = text_size() + 2),
            axis.title = element_text(size = text_size())) +
      labs(title = "Positionnement Gauche-Droite par Groupe Social",
           x = NULL,
           y = NULL) +
      scale_y_continuous(limits = c(0, 1),
                         breaks = c(0.25, 0.5, 0.75),
                         labels = c("Gauche", "Centre", "Droite"))
    ggsave(file, plot = p_left_right, width = 16, height = 9, units = "in")
  }
)

    
    ### Plot: Manual Tasks vs Art ###
    output$plot_manual_vs_art <- renderPlot({
      # Validate input
      req(input$social_var)
      req(df_social_groups)
      
              # Appliquer le mapping à la variable sociale sélectionnée
 df_social_groups <- apply_mapping(df_social_groups, input$social_var)
      
      cat("Starting to create Manual Tasks vs Art plot for:", input$social_var, "\n")
      
      # Verify that the variables exist and are numeric
      if (!all(c("lifestyle_manual_task_freq_numeric", "lifestyle_performing_arts_freq_numeric") %in% names(df_social_groups))) {
        cat("Required variables not found in data.\n")
        return(NULL)
      }
      
      # Convert to numeric if necessary
      df_social_groups$lifestyle_manual_task_freq_numeric <- as.numeric(df_social_groups$lifestyle_manual_task_freq_numeric)
      df_social_groups$lifestyle_performing_arts_freq_numeric <- as.numeric(df_social_groups$lifestyle_performing_arts_freq_numeric)
      
      # Calculate means by social group
      plot_data_manual_art <- tryCatch({
        data <- df_social_groups %>%
          filter(!is.na(.data[[input$social_var]])) %>%
          group_by(!!sym(input$social_var)) %>%
          summarise(
            Manual_Tasks = mean(lifestyle_manual_task_freq_numeric, na.rm = TRUE),
            Art = mean(lifestyle_performing_arts_freq_numeric, na.rm = TRUE),
            .groups = 'drop'
          ) %>%
          mutate(
            Art = -Art  # Make Art values negative
          ) %>%
          pivot_longer(cols = c("Manual_Tasks", "Art"), names_to = "Activity", values_to = "Mean_Frequency")
        
        cat("Manual vs Art plot data created. Dimensions:", dim(data), "\n")
        data
      }, error = function(e) {
        cat("Error creating Manual vs Art plot data:", e$message, "\n")
        NULL
      })
      
      req(plot_data_manual_art)
      
      # Create the bar plot with zero in the middle
      p_manual_art <- ggplot(data = plot_data_manual_art, 
                             aes(x = reorder(!!sym(input$social_var), Mean_Frequency), 
                                 y = Mean_Frequency, 
                                 fill = Activity)) +
        geom_bar(stat = "identity", position = "identity") +
        geom_hline(yintercept = 0, color = "black") +
        custom_theme() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = text_size()),
              axis.text.y = element_text(size = text_size()),
              legend.text = element_text(size = text_size()),
              legend.title = element_text(size = text_size()),
              plot.title = element_text(size = text_size() + 2),
              axis.title = element_text(size = text_size())) +
        labs(title = "Average Frequency of Manual Tasks and Art by Social Group",
             x = "Social Group",
             y = "Average Frequency (%)") +
        scale_y_continuous(labels = function(x) scales::percent(abs(x)), 
                           breaks = pretty(plot_data_manual_art$Mean_Frequency),
                           expand = expansion(mult = c(0.1, 0.1))) +
        scale_fill_manual(values = c("Manual_Tasks" = "steelblue", "Art" = "tomato")) +
        coord_flip()
      
      cat("Manual vs Art plot created successfully\n")
      print(p_manual_art)
    })
    
    output$download_plot_manual_vs_art <- downloadHandler(
      filename = function() {
        paste("plot_manual_vs_art", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        # Duplicate the plotting code here
        req(input$social_var)
        req(df_social_groups)
        
                     # Appliquer le mapping à la variable sociale sélectionnée
 df_social_groups <- apply_mapping(df_social_groups, input$social_var)
      
 cat("Starting to create Manual Tasks vs Art plot for:", input$social_var, "\n")
 
 # Verify that the variables exist and are numeric
 if (!all(c("lifestyle_manual_task_freq_numeric", "lifestyle_performing_arts_freq_numeric") %in% names(df_social_groups))) {
   cat("Required variables not found in data.\n")
   return(NULL)
 }
 
 # Convert to numeric if necessary
 df_social_groups$lifestyle_manual_task_freq_numeric <- as.numeric(df_social_groups$lifestyle_manual_task_freq_numeric)
 df_social_groups$lifestyle_performing_arts_freq_numeric <- as.numeric(df_social_groups$lifestyle_performing_arts_freq_numeric)
 
 # Calculate means by social group
 plot_data_manual_art <- tryCatch({
   data <- df_social_groups %>%
     filter(!is.na(.data[[input$social_var]])) %>%
     group_by(!!sym(input$social_var)) %>%
     summarise(
       Manual_Tasks = mean(lifestyle_manual_task_freq_numeric, na.rm = TRUE),
       Art = mean(lifestyle_performing_arts_freq_numeric, na.rm = TRUE),
       .groups = 'drop'
     ) %>%
     mutate(
       Art = -Art  # Make Art values negative
     ) %>%
     pivot_longer(cols = c("Manual_Tasks", "Art"), names_to = "Activity", values_to = "Mean_Frequency")
   
   cat("Manual vs Art plot data created. Dimensions:", dim(data), "\n")
   data
 }, error = function(e) {
   cat("Error creating Manual vs Art plot data:", e$message, "\n")
   NULL
 })
 
 req(plot_data_manual_art)
 
 # Create the bar plot with zero in the middle
 p_manual_art <- ggplot(data = plot_data_manual_art, 
                        aes(x = reorder(!!sym(input$social_var), Mean_Frequency), 
                            y = Mean_Frequency, 
                            fill = Activity)) +
   geom_bar(stat = "identity", position = "identity") +
   geom_hline(yintercept = 0, color = "black") +
   custom_theme() +
   theme(axis.text.x = element_text(angle = 45, hjust = 1, size = text_size()),
         axis.text.y = element_text(size = text_size()),
         legend.text = element_text(size = text_size()),
         legend.title = element_text(size = text_size()),
         plot.title = element_text(size = text_size() + 2),
         axis.title = element_text(size = text_size())) +
   labs(title = "Average Frequency of Manual Tasks and Art by Social Group",
        x = "Social Group",
        y = "Average Frequency (%)") +
   scale_y_continuous(labels = function(x) scales::percent(abs(x)), 
                      breaks = pretty(plot_data_manual_art$Mean_Frequency),
                      expand = expansion(mult = c(0.1, 0.1))) +
   scale_fill_manual(values = c("Manual_Tasks" = "steelblue", "Art" = "tomato")) +
   coord_flip()
        
        ggsave(file, plot = p_manual_art, width = 16, height = 9, units = "in")
      }
    )
    
    ### Plot: Hunting ###
    output$plot_hunting <- renderPlot({
                           # Appliquer le mapping à la variable sociale sélectionnée
 df_social_groups <- apply_mapping(df_social_groups, input$social_var)
      df_hunting <- df_social_groups %>%
        filter(!is.na(.data[[input$social_var]]), !is.na(lifestyle_hunting_freq_numeric)) %>%
        group_by(!!sym(input$social_var)) %>%
        summarise(mean_hunting_freq = mean(lifestyle_hunting_freq_numeric, na.rm = TRUE))
  
      p <- ggplot(data = df_hunting, aes(x = !!sym(input$social_var), y = mean_hunting_freq)) +
        geom_bar(stat = "identity", fill = "darkorange") +
        custom_theme() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = text_size()),
              axis.text.y = element_text(size = text_size()),
              plot.title = element_text(size = text_size() + 2),
              axis.title = element_text(size = text_size())) +
        labs(title = "Average Hunting Frequency by Social Group",
             x = "Social Group",
             y = "Average Hunting Frequency")
      print(p)
    })
    
    output$download_plot_hunting <- downloadHandler(
      filename = function() {
        paste("plot_hunting", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
                                   # Appliquer le mapping à la variable sociale sélectionnée
 df_social_groups <- apply_mapping(df_social_groups, input$social_var)
 df_hunting <- df_social_groups %>%
   filter(!is.na(.data[[input$social_var]]), !is.na(lifestyle_hunting_freq_numeric)) %>%
   group_by(!!sym(input$social_var)) %>%
   summarise(mean_hunting_freq = mean(lifestyle_hunting_freq_numeric, na.rm = TRUE))

 p <- ggplot(data = df_hunting, aes(x = !!sym(input$social_var), y = mean_hunting_freq)) +
   geom_bar(stat = "identity", fill = "darkorange") +
   custom_theme() +
   theme(axis.text.x = element_text(angle = 45, hjust = 1, size = text_size()),
         axis.text.y = element_text(size = text_size()),
         plot.title = element_text(size = text_size() + 2),
         axis.title = element_text(size = text_size())) +
   labs(title = "Average Hunting Frequency by Social Group",
        x = "Social Group",
        y = "Average Hunting Frequency")
        ggsave(file, plot = p, width = 16, height = 9, units = "in")
      }
    )
    
    ### Plot: Transport ###
    output$plot_transport <- renderPlot({
      # Appliquer le mapping à la variable sociale sélectionnée
 df_social_groups <- apply_mapping(df_social_groups, input$social_var)
      data <- df_social_groups %>%
        filter(!is.na(.data[[input$social_var]]), !is.na(lifestyle_medsociaux_plus_frequent)) %>%
        group_by(!!sym(input$social_var), lifestyle_medsociaux_plus_frequent) %>%
        summarise(count = n(), .groups = 'drop') %>%
        group_by(!!sym(input$social_var)) %>%
        mutate(proportion = count / sum(count)) %>%
        ungroup()

      p <- ggplot(data = data, 
                  aes(x = !!sym(input$social_var), 
                      y = proportion, 
                      fill = lifestyle_medsociaux_plus_frequent)) +
        geom_bar(stat = "identity", position = "dodge") +
        custom_theme() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = text_size()),
              axis.text.y = element_text(size = text_size()),
              legend.text = element_text(size = text_size()),
              legend.title = element_text(size = text_size()),
              plot.title = element_text(size = text_size() + 2),
              axis.title = element_text(size = text_size())) +
        labs(title = "Most Frequent Social Media by Social Group",
             x = "Social Group",
             y = "Proportion",
             fill = "Social Media") +
        scale_y_continuous(labels = scales::percent) +
        scale_fill_manual(values = sns_colors)
      print(p)
    })
    
    output$download_plot_transport <- downloadHandler(
      filename = function() {
        paste("plot_transport", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        df_social_groups <- apply_mapping(df_social_groups, input$social_var)
      data <- df_social_groups %>%
        filter(!is.na(.data[[input$social_var]]), !is.na(lifestyle_medsociaux_plus_frequent)) %>%
        group_by(!!sym(input$social_var), lifestyle_medsociaux_plus_frequent) %>%
        summarise(count = n(), .groups = 'drop') %>%
        group_by(!!sym(input$social_var)) %>%
        mutate(proportion = count / sum(count)) %>%
        ungroup()

      p <- ggplot(data = data, 
                  aes(x = !!sym(input$social_var), 
                      y = proportion, 
                      fill = lifestyle_medsociaux_plus_frequent)) +
        geom_bar(stat = "identity", position = "dodge") +
        custom_theme() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = text_size()),
              axis.text.y = element_text(size = text_size()),
              legend.text = element_text(size = text_size()),
              legend.title = element_text(size = text_size()),
              plot.title = element_text(size = text_size() + 2),
              axis.title = element_text(size = text_size())) +
        labs(title = "Most Frequent Social Media by Social Group",
             x = "Social Group",
             y = "Proportion",
             fill = "Social Media") +
        scale_y_continuous(labels = scales::percent) +
        scale_fill_manual(values = sns_colors)
        ggsave(file, plot = p, width = 16, height = 9, units = "in")
      }
    )
    
  }) 
}

