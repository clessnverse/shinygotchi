# Module de pondération de données

library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(DT)

# Fonction de préparation des poids
prepare_weights <- function(df_weights_clean) {
  # Transformer les données de poids en format long et standardisé
  weights_base <- df_weights_clean %>%
    mutate(
      # Convertir value en caractère si nécessaire
      value = as.character(value)
    )
  
  # Poids spécifiques pour le genre basés sur les données de Statistique Canada
  gender_weights <- tibble(
    location = "can",
    variable = "ses_gender",
    value = c("female", "male", "non_binary", "queer", "trans_man", "trans_woman"),
    population = c(
      # Estimation basée sur le fait que les hommes et femmes représentent la majorité
      15000000,  # female
      15000000,  # male
      41355,     # non_binary (0,33% de la population)
      round(41355 * 0.3),  # queer (estimation)
      round(100815 * 0.6), # trans_man 
      round(100815 * 0.4)  # trans_woman
    )
  ) %>%
  mutate(
    total_population = sum(population),
    proportion = population / total_population
  )
  
  # Combiner les poids existants avec les poids de genre
  weights_combined <- bind_rows(
    weights_base %>% 
      filter(variable != "ses_gender"),
    gender_weights
  ) %>%
  group_by(variable) %>%
  mutate(
    total_population = sum(population),
    proportion = population / total_population
  ) %>%
  ungroup()
  
  return(weights_combined)
}

# Module UI pour l'upload et la pondération
dataWeightingUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 12,
        div(
          class = "title-box",
          h2("Pondération de données"),
          p("Téléchargez votre fichier RDS contenant les mêmes variables pour obtenir les données pondérées pour des analyses scientifiques.")
        )
      )
    ),
    
    fluidRow(
      column(
        width = 4,
        div(
          class = "card",
          div(
            class = "card-body",
            h4("Télécharger votre fichier", class = "card-title"),
            p("Veuillez télécharger un fichier RDS. Assurez-vous qu'il contient des variables compatibles avec notre système de pondération."),
            fileInput(ns("upload_rds"), "Choisir un fichier RDS",
                     accept = c(".rds"),
                     buttonLabel = "Parcourir...",
                     placeholder = "Aucun fichier sélectionné"),
            selectInput(ns("weighting_vars"), 
                        "Variables de pondération", 
                        choices = NULL, 
                        multiple = TRUE),
            actionButton(ns("process_data"), "Pondérer les données", 
                        class = "btn-success", 
                        icon = icon("balance-scale")),
            downloadButton(ns("download_weighted"), "Télécharger les données pondérées", 
                           class = "btn-primary mt-3", 
                           icon = icon("download"))
          )
        )
      ),
      
      column(
        width = 8,
        conditionalPanel(
          condition = paste0("input['", ns("upload_rds"), "'] != null"),
          div(
            class = "card",
            div(
              class = "card-body",
              h4("Aperçu des données", class = "card-title"),
              uiOutput(ns("data_info")),
              dataTableOutput(ns("preview_table")) %>% withSpinner(),
              verbatimTextOutput(ns("weighting_stats")) %>% withSpinner()
            )
          )
        )
      )
    ),
    
    fluidRow(
      column(
        width = 12,
        div(
          class = "card mt-4",
          div(
            class = "card-body",
            h4("Comment utiliser cet outil", class = "card-title"),
            p("Cette fonction permet de pondérer vos données brutes pour qu'elles soient représentatives de la population canadienne, en utilisant des données de recensement."),
            tags$ol(
              tags$li("Téléchargez votre fichier RDS contenant les variables socio-démographiques"),
              tags$li("Sélectionnez les variables à pondérer"),
              tags$li("Cliquez sur 'Pondérer les données'"),
              tags$li("Téléchargez le fichier pondéré pour vos analyses")
            ),
            p("Note: Les variables clés utilisées pour la pondération sont: l'âge, le sexe, l'éducation, la province, le revenu, et la langue.")
          )
        )
      )
    )
  )
}

# Module Server pour l'upload et la pondération
dataWeightingServer <- function(id, df_weights_clean) {
  moduleServer(id, function(input, output, session) {
    # Préparer les poids
    weights_prepared <- prepare_weights(df_weights_clean)
    
    # Données chargées
    uploaded_data <- reactiveVal(NULL)
    weighted_data <- reactiveVal(NULL)
    
    # Charger les données RDS
    observeEvent(input$upload_rds, {
      req(input$upload_rds)
      
      tryCatch({
        data <- readRDS(input$upload_rds$datapath)
        uploaded_data(data)
        
        # Convertir toutes les colonnes en caractères pour la comparaison
        data_for_vars <- data %>%
          mutate(across(where(is.numeric), as.character))
        
        # Mettre à jour les choix de variables de pondération
        available_vars <- intersect(
          names(data_for_vars), 
          unique(weights_prepared$variable)
        )
        updateSelectInput(
          session, 
          "weighting_vars", 
          choices = available_vars,
          selected = available_vars
        )
      }, error = function(e) {
        showNotification(
          paste("Erreur de chargement du fichier RDS:", e$message), 
          type = "error"
        )
      })
    })
    
    # Informations sur les données
    output$data_info <- renderUI({
      req(uploaded_data())
      data <- uploaded_data()
      
      tagList(
        p(paste("Observations:", nrow(data))),
        p(paste("Variables:", paste(names(data), collapse = ", "))),
        h5("Variables clés disponibles:"),
        tags$ul(
          lapply(c("ses_age_4Cat", "ses_gender", "ses_educ_3Cat", 
                   "ses_province", "ses_incomeCensus", "ses_language"), function(var) {
            if (var %in% names(data)) {
              tags$li(HTML(paste0("<span style='color:green'><i class='fas fa-check'></i></span> ", var)))
            } else {
              tags$li(HTML(paste0("<span style='color:red'><i class='fas fa-times'></i></span> ", var, " (manquante)")))
            }
          })
        )
      )
    })
    
    # Aperçu des données
    output$preview_table <- renderDataTable({
      req(uploaded_data())
      data <- uploaded_data()
      
      # Limiter le nombre de lignes et de colonnes pour l'aperçu
      head(data, 10)
    }, options = list(
      scrollX = TRUE,
      pageLength = 5,
      lengthMenu = c(5, 10, 15)
    ))
    
    # Fonction de pondération
    perform_weighting <- function(data, weights_prepared, vars_to_weight) {
      # Convertir les colonnes de données en caractères
      data_converted <- data %>%
        mutate(across(where(is.numeric), as.character))
      
      # Initialiser avec des poids unitaires
      result <- data_converted %>% mutate(weight = 1.0)
      
      # Stocker les statistiques
      before_stats <- list()
      after_stats <- list()
      
      # Pondérer par chaque variable sélectionnée
      for (var in vars_to_weight) {
        # Distribution actuelle
        current_dist <- result %>% 
          group_by(!!sym(var)) %>%
          summarise(count = n(), .groups = "drop") %>%
          mutate(proportion = count / sum(count))
        
        # Distribution cible
        target_dist <- weights_prepared %>%
          filter(variable == var) %>%
          select(value, proportion)
        
        # Calculer les facteurs de pondération
        weight_factors <- current_dist %>%
          left_join(target_dist, by = setNames("value", var)) %>%
          mutate(
            factor = ifelse(!is.na(proportion.y), 
                            proportion.y / proportion.x, 
                            1)
          )
        
        # Appliquer les facteurs de pondération
        result <- result %>%
          left_join(
            weight_factors %>% 
              select(!!sym(var), factor), 
            by = setNames(var, var)
          ) %>%
          mutate(
            weight = weight * ifelse(!is.na(factor), factor, 1),
            factor = NULL
          )
        
        # Stocker les statistiques
        before_stats[[var]] <- current_dist
        
        # Statistiques après pondération
        after_dist <- result %>%
          group_by(!!sym(var)) %>%
          summarise(
            weighted_count = sum(weight), 
            .groups = "drop"
          ) %>%
          mutate(proportion = weighted_count / sum(weighted_count))
        
        after_stats[[var]] <- after_dist
      }
      
      # Normaliser les poids
      result <- result %>% 
        mutate(weight = weight * (nrow(data) / sum(weight)))
      
      # Restaurer les types de colonnes originaux
      result <- bind_cols(
        data %>% select(-any_of(names(result))),
        result
      )
      
      list(
        weighted_data = result,
        before_stats = before_stats,
        after_stats = after_stats
      )
    }
    
    # Pondération des données
    observeEvent(input$process_data, {
      req(uploaded_data(), input$weighting_vars)
      
      weighting_result <- perform_weighting(
        uploaded_data(), 
        weights_prepared, 
        input$weighting_vars
      )
      
      weighted_data(weighting_result$weighted_data)
      
      showNotification("Pondération terminée", type = "message")
    })
    
    # Statistiques de pondération
    output$weighting_stats <- renderPrint({
      req(weighted_data())
      
      cat("Statistiques de pondération:\n")
      cat("Nombre total de poids:", nrow(weighted_data()), "\n")
      cat("Moyenne des poids:", mean(weighted_data()$weight), "\n")
      cat("Écart-type des poids:", sd(weighted_data()$weight), "\n")
      cat("Min des poids:", min(weighted_data()$weight), "\n")
      cat("Max des poids:", max(weighted_data()$weight), "\n")
      cat("Coefficient de variation:", 
          sd(weighted_data()$weight) / mean(weighted_data()$weight), "\n")
    })
    
    # Option de téléchargement des données pondérées
    output$download_weighted <- downloadHandler(
      filename = function() {
        paste0("donnees_ponderees_", format(Sys.time(), "%Y%m%d_%H%M"), ".rds")
      },
      content = function(file) {
        req(weighted_data())
        saveRDS(weighted_data(), file)
      }
    )
  })
}