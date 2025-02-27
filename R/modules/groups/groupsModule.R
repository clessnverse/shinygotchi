source("R/utils/viz.R")

# UI Module (Mise à jour des choix dans le selectInput)
socialGroupsUI <- function(id) {
  ns <- NS(id)
  
  div(
    class = "groups-module-container",
    sidebarLayout(
      sidebarPanel(
        div(
          class = "sidebar-content",
          selectInput(ns("social_var"), "Sélectionnez la variable du groupe social :",
            choices = c(
              "Groupes d'âges" = "ses_age_4Cat",
              "Genre" = "ses_gender",
              "Province" = "ses_province",
              "Langue" = "ses_language",
              "Groupes de revenues" = "ses_income",
              "Éducation" = "ses_educ_5Cat",
              "Ethnicité" = "ses_ethnicity",
              "Statut de logement" = "ses_owner",
              "Groupes religieux" = "ses_religionBigFive",
              "Orientation sexuelle" = "ses_sexOrientation"
            )
          ),
          hr(),
          h4("Ajuster la taille du texte"),
          actionButton(ns("decrease_text_size"), "-"),
          actionButton(ns("increase_text_size"), "+"),
          hr(),
          numericInput(ns("export_width"), "Largeur (en pouces) pour l'export PNG:", value = 12, min = 4, max = 30),
          numericInput(ns("export_height"), "Hauteur (en pouces) pour l'export PNG:", value = 7, min = 4, max = 30),
          numericInput(ns("export_text_size"), "Taille de texte (points) pour l'export PNG:", value = 60, min = 8, max = 150)
        )
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Choix de vote",
                   plotOutput(ns("plot_vote_choice"), height = "600px"),
                   downloadButton(ns("download_plot_vote_choice"), "Télécharger le graphique")
          ),
          tabPanel("Participation électorale",
                   plotOutput(ns("plot_turnout"), height = "600px"),
                   downloadButton(ns("download_plot_turnout"), "Télécharger le graphique")
          ),
          tabPanel("Gauche vs Droite",
                   plotOutput(ns("plot_left_vs_right"), height = "600px"),
                   downloadButton(ns("download_plot_left_vs_right"), "Télécharger le graphique")
          ),
          tabPanel("Tâches manuelles vs Art",
                   plotOutput(ns("plot_manual_vs_art"), height = "600px"),
                   downloadButton(ns("download_plot_manual_vs_art"), "Télécharger le graphique")
          ),
          tabPanel("Chasse",
                   plotOutput(ns("plot_hunting"), height = "600px"),
                   downloadButton(ns("download_plot_hunting"), "Télécharger le graphique")
          ),
          tabPanel("Réseaux sociaux",
                   plotOutput(ns("plot_reseaux"), height = "600px"),
                   downloadButton(ns("download_plot_reseaux"), "Télécharger le graphique")
          )
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
    df_social_groups <- readRDS("data/data_pilot_clean_janv_2025.rds")
    
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
      "Vote choice" = "dv_voteChoice",
      "Left vs Right" = "dv_attitudeLeftvsRight",
      "Turnout" = "dv_turnout",
      "Hunting" = "lifestyle_goHuntingFreq_numeric",
      "Manual Tasks" = "lifestyle_manualTaskFreq_numeric",
      "Art" = "lifestyle_performingArtsFreq_numeric",
      "Reseaux" = "lifestyle_mostFreqSocialMedia"
    )
    
    social_vars <- list(
      "Age Groups" = "ses_age_4Cat",
      "Gender" = "ses_gender",
      "Geographic Location" = "ses_region",
      "Language" = "ses_language",
      "Socioeconomic Status" = "ses_income",
      "Education" = "ses_educ_5Cat",
      "Ethnicity" = "ses_ethnicity",
      "Housing Status" = "ses_owner",
      "Religious Groups" = "ses_religionBigFive",
      "Sexual Orientation" = "ses_sexOrientation"
    )
    
    # Add reactive observer for input changes
    observeEvent(input$social_var, {
      cat("Selected social variable:", input$social_var, "\n")
    })
    
    # Function to apply the custom theme with dynamic text size
    custom_theme <- reactive({
      theme_datagotchi_light(base_size = text_size())
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
      "jew"       = "Juif",
      "agnostic/atheist" = "Agnostique/Athée", # Ajouté car présent dans dataframe
      "other"     = "Autre",                   # Ajouté car possiblement présent
      "none"      = "Aucune religion"          # Ajouté car possiblement présent
    )
    
    orientation_labels <- c(
      "heterosexual" = "Hétérosexuel",
      "gay"          = "Gai/Lesbienne",
      "bisexual"     = "Bisexuel",
      "other"        = "Autre"
    )
    
    # Créer une liste de mappings pour un accès facile
    variable_mappings <- list(
      "ses_age_4Cat" = age_group_labels,
      "ses_gender" = gender_labels,
      "ses_province" = province_labels,
      "ses_language" = language_labels,
      "ses_income" = income_labels,
      "ses_educ_5Cat" = education_labels,
      "ses_ethnicity" = ethnicity_labels,
      "ses_owner" = owner_labels,
      "ses_religionBigFive" = religion_labels,
      "ses_sexOrientation" = orientation_labels
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
### Plot: Vote Choice ###
output$plot_vote_choice <- renderPlot({
  # Valid parties - exclusion explicite de "other"
  valid_parties <- setdiff(names(canadian_party_colors), "other")
  
  # Appliquer le mapping à la variable sociale sélectionnée
  df_social_groups <- apply_mapping(df_social_groups, input$social_var)
  
  # Calculate proportions - en filtrant pour n'inclure que les partis valides (sans "other")
  data <- df_social_groups %>%
    filter(!is.na(.data[[input$social_var]]), 
           !is.na(dv_voteChoice),
           dv_voteChoice %in% valid_parties) %>%  # Filtrer pour n'inclure que les partis valides
    group_by(!!sym(input$social_var), dv_voteChoice) %>%
    summarise(count = n(), .groups = 'drop') %>%
    group_by(!!sym(input$social_var)) %>%
    mutate(proportion = count / sum(count)) %>%
    ungroup()

  # Create plot
  p <- ggplot(data = data, 
              aes(x = !!sym(input$social_var), 
                  y = proportion, 
                  fill = dv_voteChoice)) +
    geom_bar(stat = "identity", position = "dodge") +
    custom_theme() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = text_size()),
          axis.title.y = element_text(
            margin = margin(r = 30)  # éloigne le titre de l'axe
          ),
          axis.text.y = element_text(
                    size = text_size(),
                    margin = margin(r = 0)),
          legend.text = element_text(size = text_size()),
          legend.title = element_text(size = text_size()),
          plot.title = element_text(size = text_size() + 2),
          axis.title = element_text(size = text_size())) +
    labs(title = "Choix de vote par groupe social",
         x = "Groupe social",
         y = "Proportion",
         fill = "Choix de vote") +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = canadian_party_colors[valid_parties])  # Utiliser seulement les couleurs des partis valides
  print(p)
})

output$download_plot_vote_choice <- downloadHandler(
  filename = function() {
    paste("plot_vote_choice", Sys.Date(), ".png", sep = "")
  },
  content = function(file) {
    
    # --------------------------------------------------------------------
    # (1) Créer et sauvegarder le ggplot dans un FICHIER TEMPORAIRE
    # --------------------------------------------------------------------
    # 1.1 Construire votre ggplot comme d'habitude
    valid_parties <- setdiff(names(canadian_party_colors), "other")
    df_social_groups <- apply_mapping(df_social_groups, input$social_var)
    
    data <- df_social_groups %>%
      filter(!is.na(.data[[input$social_var]]), 
             !is.na(dv_voteChoice),
             dv_voteChoice %in% valid_parties) %>%  # Filtrer pour n'inclure que les partis valides
      group_by(!!sym(input$social_var), dv_voteChoice) %>%
      summarise(count = n(), .groups = 'drop') %>%
      group_by(!!sym(input$social_var)) %>%
      mutate(proportion = count / sum(count)) %>%
      ungroup()
    
    export_plot <- ggplot(data, aes(x = !!sym(input$social_var), y = proportion, fill = dv_voteChoice)) +
      geom_bar(stat = "identity", position = "dodge") +
      # Ici, vous pouvez ajuster la taille du texte "à l'export", ou le faire plus haut
      theme_datagotchi_light(base_size = input$export_text_size) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = input$export_text_size),
        axis.title.y = element_text(
          margin = margin(r = 20)  # éloigne le titre de l'axe
        ),
        axis.text.y = element_text(size = input$export_text_size),
        legend.text = element_text(size = input$export_text_size),
        legend.title = element_text(size = input$export_text_size),
        plot.title = element_text(size = input$export_text_size + 2),
        axis.title = element_text(size = input$export_text_size)
      ) +
      labs(
        title = "Choix de vote par groupe social",
        x = "Groupe social",
        y = "Proportion",
        fill = "Choix de vote"
      ) +
      scale_y_continuous(labels = scales::percent) +
      scale_fill_manual(values = canadian_party_colors[valid_parties])  # Utiliser seulement les couleurs des partis valides
    
    
    # 1.2 Sauvegarder ce ggplot dans un fichier temporaire
    tmp_file <- tempfile(fileext = ".png")
    ggsave(
      filename = tmp_file,
      plot = export_plot,
      width = input$export_width,   # inch
      height = input$export_height, # inch
      units = "in",
      dpi = 300
    )
    
    # --------------------------------------------------------------------
    # (2) Charger l'image principale ET le logo
    # --------------------------------------------------------------------
    main_img <- readPNG(tmp_file)
    logo_img <- readPNG("www/Logo.PNG")  # à adapter si nécessaire
    
    # --------------------------------------------------------------------
    # (3) Créer l'appareil graphique final
    # --------------------------------------------------------------------
    # On va écrire directement dans 'file' (celui du downloadHandler)
    png(filename = file,
        width = input$export_width * 100,    # en pixels
        height = input$export_height * 100,  # en pixels
        res = 100  # ou 300 si vous voulez plus de résolution
    )
    
    # --------------------------------------------------------------------
    # (4) Afficher le graphique principal
    # --------------------------------------------------------------------
    grid.raster(main_img, width = unit(1, "npc"), height = unit(1, "npc"))
    # => Remplit tout le device
    
    # --------------------------------------------------------------------
    # (5) Superposer le logo
    # --------------------------------------------------------------------
    # Adaptez x, y pour la position, et width, height pour la taille du logo
    # x=0.92, y=0.06 => coin inférieur droit (approx)
    grid.raster(
      logo_img, 
      x = 0.95,             # position horizontale [0 = gauche, 1 = droite]
      y = 0.05,             # position verticale   [0 = bas,    1 = haut]
      width = unit(0.87, "inches"),  # taille du logo en inches
      height= unit(0.4, "inches")   # ajustez selon ratio
    )
    
    # --------------------------------------------------------------------
    # (6) Fermer l'appareil graphique
    # --------------------------------------------------------------------
    dev.off()
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
  # La variable dv_turnout semble déjà être une valeur entre 0 et 1
  # Mais nous créons quand même une version binaire pour la cohérence avec le code original
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
                      aes(x = !!sym(input$social_var), y = turnout_rate)) +
    geom_bar(stat = "identity", width = 0.4, fill = "tomato") +
    coord_flip() +
    custom_theme() +
    theme(axis.text.x = element_text(size = text_size()),
          axis.title.x = element_text(
            margin = margin(t = 20)  # éloigne "Taux de participation électorale" de l'axe
          ),
          axis.text.y = element_text(size = text_size()),
          plot.title = element_text(size = text_size() + 2),
          axis.title = element_text(size = text_size()),
          axis.ticks = element_blank()) +  # Optionnel : supprimer les graduations
    labs(title = "Participation électorale par groupe social",
         x = NULL,  # Supprimer le label de l'axe x car les groupes sociaux sont déjà indiqués
         y = "Taux de participation électorale (%)") +
    scale_y_continuous(labels = scales::percent)
  
  print(p_turnout)
})

output$download_plot_turnout <- downloadHandler(
  filename = function() {
    paste("plot_turnout", Sys.Date(), ".png", sep = "")
  },
  content = function(file) {
    
    # --------------------------------------------------------------------
    # (1) Créer et sauvegarder le ggplot dans un fichier temporaire
    # --------------------------------------------------------------------
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
    
    export_plot <- ggplot(plot_data_turnout, aes(x = !!sym(input$social_var), y = turnout_rate)) +
      geom_bar(stat = "identity", width = 0.4, fill = "tomato") +
      coord_flip() +
      theme_datagotchi_light(base_size = input$export_text_size) +
      theme(
        axis.text.x = element_text(size = input$export_text_size),
        axis.title.x = element_text(
          margin = margin(t = 30)  # éloigne "Taux de participation électorale" de l'axe
        ),
        axis.text.y = element_text(size = input$export_text_size),
        plot.title  = element_text(size = input$export_text_size + 2),
        axis.title  = element_text(size = input$export_text_size),
        axis.ticks  = element_blank()
      ) +
      labs(
        title = "Participation électorale par groupe social",
        x = NULL,
        y = "Taux de participation électorale (%)"
      ) +
      scale_y_continuous(labels = scales::percent)
    
    # 1.2 Sauvegarde temporaire
    tmp_file <- tempfile(fileext = ".png")
    ggsave(
      filename = tmp_file,
      plot = export_plot,
      width  = input$export_width,
      height = input$export_height,
      units  = "in",
      dpi    = 300
    )
    
    # --------------------------------------------------------------------
    # (2) Charger l'image principale ET le logo
    # --------------------------------------------------------------------
    main_img <- readPNG(tmp_file)
    logo_img <- readPNG("www/Logo.PNG")
    
    # --------------------------------------------------------------------
    # (3) Créer l'appareil graphique final
    # --------------------------------------------------------------------
    png(filename = file,
        width  = input$export_width * 100,
        height = input$export_height * 100,
        res    = 100
    )
    
    # --------------------------------------------------------------------
    # (4) Afficher le graphique principal
    # --------------------------------------------------------------------
    grid.raster(main_img, width = unit(1, "npc"), height = unit(1, "npc"))
    
    # --------------------------------------------------------------------
    # (5) Superposer le logo
    # --------------------------------------------------------------------
    grid.raster(
      logo_img, 
      x = 0.95,  
      y = 0.05,  
      width  = unit(0.87, "inches"), 
      height = unit(0.4, "inches")
    )
    
    # --------------------------------------------------------------------
    # (6) Fermer l'appareil graphique
    # --------------------------------------------------------------------
    dev.off()
  }
)

    
 ### Plot: Left vs Right ###
output$plot_left_vs_right <- renderPlot({
  req(input$social_var)
  req(df_social_groups)
  
  # Appliquer le mapping à la variable sociale sélectionnée
  df_social_groups <- apply_mapping(df_social_groups, input$social_var)
  
  if (!"dv_attitudeLeftvsRight" %in% names(df_social_groups)) {
    return(NULL)
  }
  
  if (!is.numeric(df_social_groups$dv_attitudeLeftvsRight)) {
    return(NULL)
  }
  
  plot_data_left_right <- df_social_groups %>%
    filter(!is.na(.data[[input$social_var]]), !is.na(dv_attitudeLeftvsRight)) %>%
    group_by(!!sym(input$social_var)) %>%
    summarise(
      mean_left_right = mean(dv_attitudeLeftvsRight, na.rm = TRUE),
      .groups = 'drop'
    )
  
  p_left_right <- ggplot(data = plot_data_left_right, 
                         aes(x = !!sym(input$social_var), y = mean_left_right)) +
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
    
    # --------------------------------------------------------------------
    # (1) Créer et sauvegarder le ggplot dans un fichier temporaire
    # --------------------------------------------------------------------
    df_social_groups <- apply_mapping(df_social_groups, input$social_var)
    
    if (!"dv_attitudeLeftvsRight" %in% names(df_social_groups)) {
      return(NULL)
    }
    if (!is.numeric(df_social_groups$dv_attitudeLeftvsRight)) {
      return(NULL)
    }
    
    plot_data_left_right <- df_social_groups %>%
      filter(!is.na(.data[[input$social_var]]), !is.na(dv_attitudeLeftvsRight)) %>%
      group_by(!!sym(input$social_var)) %>%
      summarise(
        mean_left_right = mean(dv_attitudeLeftvsRight, na.rm = TRUE),
        .groups = 'drop'
      )
    
    export_plot <- ggplot(plot_data_left_right, aes(x = !!sym(input$social_var), y = mean_left_right)) +
      geom_point(size = 3, color = "black") +
      geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
      coord_flip() +
      theme_datagotchi_light(base_size = input$export_text_size) +
      theme(
        axis.text.x = element_text(size = input$export_text_size),
        axis.text.y = element_text(size = input$export_text_size),
        plot.title  = element_text(size = input$export_text_size + 2),
        axis.title  = element_text(size = input$export_text_size)
      ) +
      labs(
        title = "Positionnement Gauche-Droite par Groupe Social",
        x = NULL,
        y = NULL
      ) +
      scale_y_continuous(
        limits = c(0, 1),
        breaks = c(0.25, 0.5, 0.75),
        labels = c("Gauche", "Centre", "Droite")
      )
    
    # 1.2 Sauvegarde temporaire
    tmp_file <- tempfile(fileext = ".png")
    ggsave(
      filename = tmp_file,
      plot = export_plot,
      width  = input$export_width,
      height = input$export_height,
      units  = "in",
      dpi    = 300
    )
    
    # --------------------------------------------------------------------
    # (2) Charger l'image principale + logo
    # --------------------------------------------------------------------
    main_img <- readPNG(tmp_file)
    logo_img <- readPNG("www/Logo.PNG")
    
    # --------------------------------------------------------------------
    # (3) Créer l'appareil graphique final
    # --------------------------------------------------------------------
    png(filename = file,
        width  = input$export_width * 100,
        height = input$export_height * 100,
        res    = 100
    )
    
    # --------------------------------------------------------------------
    # (4) Afficher le graphique principal
    # --------------------------------------------------------------------
    grid.raster(main_img, width = unit(1, "npc"), height = unit(1, "npc"))
    
    # --------------------------------------------------------------------
    # (5) Superposer le logo
    # --------------------------------------------------------------------
    grid.raster(
      logo_img, 
      x = 0.95,
      y = 0.05,
      width  = unit(0.87, "inches"), 
      height = unit(0.4, "inches")
    )
    
    # --------------------------------------------------------------------
    # (6) Fermer l'appareil graphique
    # --------------------------------------------------------------------
    dev.off()
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
  if (!all(c("lifestyle_manualTaskFreq_numeric", "lifestyle_performingArtsFreq_numeric") %in% names(df_social_groups))) {
    cat("Required variables not found in data.\n")
    return(NULL)
  }
  
  # Convert to numeric if necessary
  df_social_groups$lifestyle_manualTaskFreq_numeric <- as.numeric(df_social_groups$lifestyle_manualTaskFreq_numeric)
  df_social_groups$lifestyle_performingArtsFreq_numeric <- as.numeric(df_social_groups$lifestyle_performingArtsFreq_numeric)
  
  # Calculate means by social group
  plot_data_manual_art <- tryCatch({
    data <- df_social_groups %>%
      filter(!is.na(.data[[input$social_var]])) %>%
      group_by(!!sym(input$social_var)) %>%
      summarise(
        Manual_Tasks = mean(lifestyle_manualTaskFreq_numeric, na.rm = TRUE),
        Art = mean(lifestyle_performingArtsFreq_numeric, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      mutate(
        Art = -Art  # Make Art values negative
      ) %>%
      pivot_longer(cols = c("Manual_Tasks", "Art"), names_to = "Activité", values_to = "Mean_Frequency")
    
    cat("Manual vs Art plot data created. Dimensions:", dim(data), "\n")
    data
  }, error = function(e) {
    cat("Error creating Manual vs Art plot data:", e$message, "\n")
    NULL
  })
  
  req(plot_data_manual_art)
  
  # Create the bar plot with zero in the middle
  p_manual_art <- ggplot(data = plot_data_manual_art, 
                          aes(x = !!sym(input$social_var), 
                              y = Mean_Frequency, 
                              fill = Activité)) +
    geom_bar(stat = "identity", position = "identity") +
    geom_hline(yintercept = 0, color = "black") +
    custom_theme() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = text_size()),
          axis.title.x = element_text(
            margin = margin(t = 20)  
          ),
          axis.text.y = element_text(size = text_size()),
          legend.text = element_text(size = text_size()),
          legend.title = element_text(size = text_size()),
          plot.title = element_text(size = text_size() + 2),
          axis.title = element_text(size = text_size())) +
    labs(title = "Fréquence moyenne des tâches manuelles et de l'art par groupe social",
         x = "Groupe social",
         y = "Fréquence moyenne (%)") +
    scale_y_continuous(labels = function(x) scales::percent(abs(x)), 
                       breaks = pretty(plot_data_manual_art$Mean_Frequency),
                       expand = expansion(mult = c(0.1, 0.1))) +
    scale_fill_manual(values = c("Manual_Tasks" = "steelblue", "Art" = "tomato"),
                      labels = c("Manual_Tasks" = "Tâches manuelles", "Art" = "Activité artistique")) +
    coord_flip()
  
  cat("Manual vs Art plot created successfully\n")
  print(p_manual_art)
})

output$download_plot_manual_vs_art <- downloadHandler(
  filename = function() {
    paste("plot_manual_vs_art", Sys.Date(), ".png", sep = "")
  },
  content = function(file) {
    
    # --------------------------------------------------------------------
    # (1) Créer et sauvegarder le ggplot dans un fichier temporaire
    # --------------------------------------------------------------------
    df_social_groups <- apply_mapping(df_social_groups, input$social_var)
    
    if (!all(c("lifestyle_manualTaskFreq_numeric", "lifestyle_performingArtsFreq_numeric") %in% names(df_social_groups))) {
      return(NULL)
    }
    
    df_social_groups$lifestyle_manualTaskFreq_numeric <-
      as.numeric(df_social_groups$lifestyle_manualTaskFreq_numeric)
    df_social_groups$lifestyle_performingArtsFreq_numeric <-
      as.numeric(df_social_groups$lifestyle_performingArtsFreq_numeric)
    
    plot_data_manual_art <- df_social_groups %>%
      filter(!is.na(.data[[input$social_var]])) %>%
      group_by(!!sym(input$social_var)) %>%
      summarise(
        Manual_Tasks = mean(lifestyle_manualTaskFreq_numeric, na.rm = TRUE),
        Art = mean(lifestyle_performingArtsFreq_numeric, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      mutate(
        Art = -Art  # Inverse la valeur pour Art
      ) %>%
      tidyr::pivot_longer(
        cols = c("Manual_Tasks", "Art"),
        names_to = "Activité",
        values_to = "Mean_Frequency"
      )
    
    export_plot <- ggplot(plot_data_manual_art, aes(x = !!sym(input$social_var), y = Mean_Frequency, fill = Activité)) +
      geom_bar(stat = "identity", position = "identity") +
      geom_hline(yintercept = 0, color = "black") +
      theme_datagotchi_light(base_size = input$export_text_size) +
      theme(
        axis.text.x   = element_text(angle = 45, hjust = 1, size = input$export_text_size),
        axis.title.x = element_text(
          margin = margin(t = 20)  
        ),
        axis.text.y   = element_text(size = input$export_text_size),
        legend.text   = element_text(size = input$export_text_size),
        legend.title  = element_text(size = input$export_text_size),
        legend.position = "bottom",         # Place la légende en bas
        legend.justification = "center",    # Centre la légende horizontalement
        legend.box.just = "center", 
        plot.title    = element_text(size = input$export_text_size + 2),
        axis.title    = element_text(size = input$export_text_size)
      ) +
      labs(
        title = "Fréquence moyenne des tâches manuelles et de l'art par groupe social",
        x = "Groupe social",
        y = "Fréquence moyenne (%)"
      ) +
      scale_y_continuous(
        labels = function(x) scales::percent(abs(x)), 
        breaks = scales::pretty_breaks(),
        expand = expansion(mult = c(0.1, 0.1))
      ) +
      scale_fill_manual(values = c("Manual_Tasks" = "steelblue", "Art" = "tomato"),
                        labels = c("Manual_Tasks" = "Tâches manuelles", "Art" = "Activité artistique")) +
      coord_flip()
    
    # 1.2 Sauvegarde temporaire
    tmp_file <- tempfile(fileext = ".png")
    ggsave(
      filename = tmp_file,
      plot = export_plot,
      width  = input$export_width,
      height = input$export_height,
      units  = "in",
      dpi    = 300
    )
    
    # --------------------------------------------------------------------
    # (2) Charger l'image principale + logo
    # --------------------------------------------------------------------
    main_img <- readPNG(tmp_file)
    logo_img <- readPNG("www/Logo.PNG")
    
    # --------------------------------------------------------------------
    # (3) Créer l'appareil graphique final
    # --------------------------------------------------------------------
    png(filename = file,
        width  = input$export_width * 100,
        height = input$export_height * 100,
        res    = 100
    )
    
    # --------------------------------------------------------------------
    # (4) Afficher le graphique principal
    # --------------------------------------------------------------------
    grid.raster(main_img, width = unit(1, "npc"), height = unit(1, "npc"))
    
    # --------------------------------------------------------------------
    # (5) Superposer le logo
    # --------------------------------------------------------------------
    grid.raster(
      logo_img, 
      x = 0.95,
      y = 0.05,
      width  = unit(0.87, "inches"), 
      height = unit(0.4, "inches")
    )
    
    # --------------------------------------------------------------------
    # (6) Fermer l'appareil graphique
    # --------------------------------------------------------------------
    dev.off()
  }
)
    
    
    
   ### Plot: Hunting ###
output$plot_hunting <- renderPlot({
  # Appliquer le mapping à la variable sociale sélectionnée
  df_social_groups <- apply_mapping(df_social_groups, input$social_var)
  
  df_hunting <- df_social_groups %>%
    filter(!is.na(.data[[input$social_var]]), !is.na(lifestyle_goHuntingFreq_numeric)) %>%
    group_by(!!sym(input$social_var)) %>%
    summarise(mean_hunting_freq = mean(lifestyle_goHuntingFreq_numeric, na.rm = TRUE))
  
  p <- ggplot(data = df_hunting, aes(x = !!sym(input$social_var), y = mean_hunting_freq)) +
    geom_bar(stat = "identity", fill = "darkorange") +
    custom_theme() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = text_size()),
          axis.title.y = element_text(
            margin = margin(r = 20)  # éloigne le titre de l'axe
          ),
          axis.text.y = element_text(size = text_size()),
          plot.title = element_text(size = text_size() + 2),
          axis.title = element_text(size = text_size())) +
    labs(title = "Fréquence moyenne de la chasse par groupe social",
         x = "Groupe social",
         y = "Fréquence moyenne de la chasse")
  print(p)
})

output$download_plot_hunting <- downloadHandler(
  filename = function() {
    paste("plot_hunting", Sys.Date(), ".png", sep = "")
  },
  content = function(file) {
    
    # --------------------------------------------------------------------
    # (1) Créer et sauvegarder le ggplot dans un fichier temporaire
    # --------------------------------------------------------------------
    df_social_groups <- apply_mapping(df_social_groups, input$social_var)
    df_hunting <- df_social_groups %>%
      filter(!is.na(.data[[input$social_var]]), !is.na(lifestyle_goHuntingFreq_numeric)) %>%
      group_by(!!sym(input$social_var)) %>%
      summarise(mean_hunting_freq = mean(lifestyle_goHuntingFreq_numeric, na.rm = TRUE))
    
    export_plot <- ggplot(df_hunting, aes(x = !!sym(input$social_var), y = mean_hunting_freq)) +
      geom_bar(stat = "identity", fill = "darkorange") +
      theme_datagotchi_light(base_size = input$export_text_size) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = input$export_text_size),
        axis.title.y = element_text(
          margin = margin(r = 20)  # éloigne le titre de l'axe
        ),
        axis.text.y = element_text(size = input$export_text_size),
        plot.title  = element_text(size = input$export_text_size + 2),
        axis.title  = element_text(size = input$export_text_size)
      ) +
      labs(
        title = "Fréquence moyenne de la chasse par groupe social",
        x = "Groupe social",
        y = "Fréquence moyenne de la chasse"
      )
    
    # 1.2 Sauvegarde temporaire
    tmp_file <- tempfile(fileext = ".png")
    ggsave(
      filename = tmp_file,
      plot = export_plot,
      width  = input$export_width,
      height = input$export_height,
      units  = "in",
      dpi    = 300
    )
    
    # --------------------------------------------------------------------
    # (2) Charger l'image principale + logo
    # --------------------------------------------------------------------
    main_img <- readPNG(tmp_file)
    logo_img <- readPNG("www/Logo.PNG")
    
    # --------------------------------------------------------------------
    # (3) Créer l'appareil graphique final
    # --------------------------------------------------------------------
    png(filename = file,
        width  = input$export_width * 100,
        height = input$export_height * 100,
        res    = 100
    )
    
    # --------------------------------------------------------------------
    # (4) Afficher le graphique principal
    # --------------------------------------------------------------------
    grid.raster(main_img, width = unit(1, "npc"), height = unit(1, "npc"))
    
    # --------------------------------------------------------------------
    # (5) Superposer le logo
    # --------------------------------------------------------------------
    grid.raster(
      logo_img, 
      x = 0.95,
      y = 0.05,
      width  = unit(0.87, "inches"), 
      height = unit(0.4, "inches")
    )
    
    # --------------------------------------------------------------------
    # (6) Fermer l'appareil graphique
    # --------------------------------------------------------------------
    dev.off()
  }
)
    
    
    
   ### Plot: Réseaux sociaux ###
output$plot_reseaux <- renderPlot({
  # Appliquer le mapping à la variable sociale sélectionnée
  df_social_groups <- apply_mapping(df_social_groups, input$social_var)
  
  data <- df_social_groups %>%
    filter(!is.na(.data[[input$social_var]]), !is.na(lifestyle_mostFreqSocialMedia)) %>%
    group_by(!!sym(input$social_var), lifestyle_mostFreqSocialMedia) %>%
    summarise(count = n(), .groups = 'drop') %>%
    group_by(!!sym(input$social_var)) %>%
    mutate(proportion = count / sum(count)) %>%
    ungroup()

  p <- ggplot(data = data, 
              aes(x = !!sym(input$social_var), 
                  y = proportion, 
                  fill = lifestyle_mostFreqSocialMedia)) +
    geom_bar(stat = "identity", position = "dodge") +
    custom_theme() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = text_size()),
          axis.title.y = element_text(
            margin = margin(r = 20)  # éloigne le titre de l'axe
          ),
          axis.text.y = element_text(size = text_size()),
          legend.text = element_text(size = text_size()),
          legend.title = element_text(size = text_size()),
          plot.title = element_text(size = text_size() + 2),
          axis.title = element_text(size = text_size())) +
    labs(title = "Réseaux sociaux les plus fréquentés par groupe social",
         x = "Groupe social",
         y = "Proportion (%)",
         fill = "Réseau social") +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = sns_colors)
  print(p)
})

output$download_plot_reseaux <- downloadHandler(
  filename = function() {
    paste("plot_reseaux", Sys.Date(), ".png", sep = "")
  },
  content = function(file) {
    
    # --------------------------------------------------------------------
    # (1) Créer et sauvegarder le ggplot dans un fichier temporaire
    # --------------------------------------------------------------------
    df_social_groups <- apply_mapping(df_social_groups, input$social_var)
    data <- df_social_groups %>%
      filter(!is.na(.data[[input$social_var]]), !is.na(lifestyle_mostFreqSocialMedia)) %>%
      group_by(!!sym(input$social_var), lifestyle_mostFreqSocialMedia) %>%
      summarise(count = n(), .groups = 'drop') %>%
      group_by(!!sym(input$social_var)) %>%
      mutate(proportion = count / sum(count)) %>%
      ungroup()
    
    export_plot <- ggplot(data, aes(x = !!sym(input$social_var), y = proportion, fill = lifestyle_mostFreqSocialMedia)) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_datagotchi_light(base_size = input$export_text_size) +
      theme(
        axis.text.x  = element_text(angle = 45, hjust = 1, size = input$export_text_size),
        axis.title.y = element_text(
          margin = margin(r = 20)  # éloigne le titre de l'axe
        ),
        axis.text.y  = element_text(size = input$export_text_size),
        legend.text  = element_text(size = input$export_text_size),
        legend.title = element_text(size = input$export_text_size),
        plot.title   = element_text(size = input$export_text_size + 2),
        axis.title   = element_text(size = input$export_text_size)
      ) +
      labs(
        title = "Réseaux sociaux les plus fréquentés par groupe social",
        x = "Groupe social",
        y = "Proportion (%)",
        fill = "Réseau social"
      ) +
      scale_y_continuous(labels = scales::percent) +
      scale_fill_manual(values = sns_colors)
    
    # 1.2 Sauvegarde temporaire
    tmp_file <- tempfile(fileext = ".png")
    ggsave(
      filename = tmp_file,
      plot = export_plot,
      width  = input$export_width,
      height = input$export_height,
      units  = "in",
      dpi    = 300
    )
    
    # --------------------------------------------------------------------
    # (2) Charger l'image principale + logo
    # --------------------------------------------------------------------
    main_img <- readPNG(tmp_file)
    logo_img <- readPNG("www/Logo.PNG")
    
    # --------------------------------------------------------------------
    # (3) Créer l'appareil graphique final
    # --------------------------------------------------------------------
    png(filename = file,
        width  = input$export_width * 100,
        height = input$export_height * 100,
        res    = 100
    )
    
    # --------------------------------------------------------------------
    # (4) Afficher le graphique principal
    # --------------------------------------------------------------------
    grid.raster(main_img, width = unit(1, "npc"), height = unit(1, "npc"))
    
    # --------------------------------------------------------------------
    # (5) Superposer le logo
    # --------------------------------------------------------------------
    grid.raster(
      logo_img, 
      x = 0.95,
      y = 0.05,
      width  = unit(0.87, "inches"), 
      height = unit(0.4, "inches")
    )
    
    # --------------------------------------------------------------------
    # (6) Fermer l'appareil graphique
    # --------------------------------------------------------------------
    dev.off()
  }
)
    
    
    # In groupsModule.R server function
    custom_theme <- reactive({
      create_custom_theme(text_size())
    })
    
  }) 
}
