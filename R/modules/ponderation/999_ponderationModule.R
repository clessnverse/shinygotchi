# Module de pondération de données

library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(DT)
library(shinycssloaders)

# Fonction de préparation des poids - MODIFIÉE
prepare_weights <- function(df_weights_clean) {
  # Transformer les données de poids en format long et standardisé
  weights_base <- df_weights_clean %>%
    mutate(
      # Convertir value en caractère si nécessaire
      value = as.character(value)
    ) %>%
    # Calculer les proportions pour chaque variable
    group_by(variable) %>%
    mutate(
      total_population = sum(population),
      proportion = population / total_population
    ) %>%
    ungroup()
  
  # Nous n'avons plus besoin de gérer les catégories de genre séparément
  # car elles ont été ajoutées en amont dans df_weights_clean
  
  return(weights_base)
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
                        "Variables de pondération (toutes requises)", 
                        choices = NULL, 
                        multiple = TRUE,
                        selectize = TRUE),
            tags$script(HTML(paste0("
              $(document).on('shiny:inputchanged', function(event) {
                if(event.name === '", ns("weighting_vars"), "') {
                  var selectInput = $('#", ns("weighting_vars"), "').data('selectize');
                  if(selectInput) {
                    // Réactiver les options disponibles
                    var allOptions = selectInput.options;
                    var selectedOptions = selectInput.items;
                    
                    // Si une option est désélectionnée, la resélectionner
                    if(Object.keys(allOptions).length > selectedOptions.length) {
                      Object.keys(allOptions).forEach(function(option) {
                        if(selectedOptions.indexOf(option) === -1) {
                          selectInput.addItem(option);
                        }
                      });
                    }
                  }
                }
              });
            "))),
            actionButton(ns("process_data"), "Pondérer les données", 
                        class = "btn-success", 
                        icon = icon("balance-scale")),
                        downloadButton(
                          ns("download_weighted"),
                          span(icon("download"), "Télécharger les données pondérées"),
                          class = "btn",
                          style = "
                            background-color: #2c3e50; 
                            color: white; 
                            width: 100%; 
                            margin-top: 15px; 
                            font-weight: bold; 
                            border: none; 
                            border-radius: 4px;
                            padding: 10px 15px; 
                            font-size: 1em;
                            height: 45px;
                            box-shadow: 0 1px 3px rgba(0,0,0,0.2);
                          "
                        )
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
              
              # Code de diagnostic pour les variables sélectionnées
              conditionalPanel(
                condition = paste0("input['", ns("weighting_vars"), "'].length > 0"),
                h4("Diagnostic des variables", class = "mt-4"),
                uiOutput(ns("variable_diagnostics"))
              ),
              
              # Statistiques après pondération
              conditionalPanel(
                condition = paste0("input['", ns("process_data"), "'] > 0"),
                div(
                  class = "card mt-3",
                  div(
                    class = "card-body",
                    h4("Résultats de la pondération", class = "card-title"),
                    verbatimTextOutput(ns("weighting_stats")) %>% withSpinner()
                  )
                )
              )
            )
          )
        )
      )
    ),

    # Affichage des données pondérées
    conditionalPanel(
      condition = paste0("input['", ns("process_data"), "'] > 0"),
      div(
        class = "card mt-3",
        div(
          class = "card-body",
          h4("Données pondérées", class = "card-title"),
          tags$div(id = ns("weighted_container"), 
                  dataTableOutput(ns("weighted_data_preview")))
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
              tags$li("Vérifiez les variables disponibles pour la pondération (toutes seront utilisées)"),
              tags$li("Cliquez sur 'Pondérer les données'"),
              tags$li("Téléchargez le fichier pondéré pour vos analyses")
            ),
            p("Note: Toutes les variables disponibles sont automatiquement sélectionnées et requises pour la pondération. Les variables clés sont: l'âge, le sexe, l'éducation, la province, le revenu, et la langue.")
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
    weighting_details <- reactiveVal(NULL)
    
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
        # Forcer la sélection de toutes les variables disponibles
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
      
      # Variables clés pour la pondération
      key_vars <- c("ses_age_4Cat", "ses_gender", "ses_educ_3Cat", 
                    "ses_province", "ses_incomeCensus", "ses_language")
      
      tagList(
        div(
          class = "alert alert-info",
          p(strong("Informations sur les données:")),
          p(paste("Observations:", nrow(data))),
          p(paste("Variables totales:", ncol(data)))
        ),
        h5("Variables clés disponibles:"),
        tags$ul(
          lapply(key_vars, function(var) {
            if (var %in% names(data)) {
              tags$li(HTML(paste0("<span style='color:green'><i class='fas fa-check'></i></span> ", var)))
            } else {
              tags$li(HTML(paste0("<span style='color:red'><i class='fas fa-times'></i></span> ", var, " (manquante)")))
            }
          })
        )
      )
    })
    
    # Diagnostic des variables sélectionnées
    output$variable_diagnostics <- renderUI({
      req(uploaded_data(), input$weighting_vars)
      data <- uploaded_data()
      
      # Vérifier les variables sélectionnées
      diagnostics <- lapply(input$weighting_vars, function(var) {
        # Vérifier si la variable existe
        if (!var %in% names(data)) {
          return(tags$div(
            class = "alert alert-danger",
            HTML(paste0("<strong>Variable manquante:</strong> ", var))
          ))
        }
        
        # Compter les valeurs uniques et manquantes
        n_unique <- length(unique(data[[var]]))
        n_missing <- sum(is.na(data[[var]]))
        
        # Obtenez les valeurs de poids disponibles
        weight_values <- weights_prepared %>%
          filter(variable == var) %>%
          pull(value)
        
        # Obtenez les valeurs dans les données
        data_values <- unique(as.character(data[[var]]))
        
        # Trouvez les valeurs qui sont dans les données mais pas dans les poids
        missing_in_weights <- setdiff(data_values, weight_values)
        
        # Message d'alerte pour les valeurs manquantes dans les poids
        missing_values_warning <- if(length(missing_in_weights) > 0) {
          tags$div(
            class = "alert alert-warning",
            HTML(paste0("<strong>Valeurs non pondérables:</strong> ", 
                       paste(missing_in_weights, collapse = ", "),
                       " (", length(missing_in_weights), " sur ", length(data_values), " valeurs)"))
          )
        } else {
          tags$div(
            class = "alert alert-success",
            HTML("<strong>Excellente compatibilité:</strong> Toutes les valeurs peuvent être pondérées")
          )
        }
        
        # Afficher les diagnostics
        tags$div(
          h5(var),
          tags$div(
            class = "row",
            tags$div(
              class = "col-md-6",
              tags$ul(
                tags$li(paste("Valeurs uniques:", n_unique)),
                tags$li(paste("Valeurs manquantes:", n_missing, "(", round(n_missing/nrow(data)*100, 1), "%)")),
                tags$li(paste("Valeurs dans les poids:", length(weight_values)))
              )
            ),
            tags$div(
              class = "col-md-6",
              missing_values_warning
            )
          ),
          hr()
        )
      })
      
      # Combiner tous les diagnostics
      tagList(
        diagnostics
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
    
    # Fonction de pondération - OPTIMISÉE
    perform_weighting <- function(data, weights_prepared, vars_to_weight) {
      # Convertir les colonnes de données en caractères
      data_converted <- data %>%
        mutate(across(all_of(vars_to_weight), as.character))
      
      # Initialiser avec des poids unitaires
      result <- data_converted %>% mutate(weight = 1.0)
      
      # Stocker les statistiques
      before_stats <- list()
      after_stats <- list()
      weight_factors_list <- list()
      
      # Pondérer par chaque variable sélectionnée
      for (var in vars_to_weight) {
        # Vérifier les valeurs manquantes
        missing_count <- sum(is.na(result[[var]]))
        if (missing_count > 0) {
          warning(paste0("Variable '", var, "' contient ", missing_count, " valeurs manquantes"))
        }
        
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
        
        # Stocker les facteurs pour analyse
        weight_factors_list[[var]] <- weight_factors
        
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
      for (col in names(data)) {
        if (col %in% names(result) && !col %in% vars_to_weight) {
          result[[col]] <- data[[col]]
        }
      }
      
      # Calculer le design effect et la taille effective de l'échantillon
      design_effect <- 1 + (var(result$weight) / mean(result$weight)^2)
      effective_sample_size <- nrow(result) / design_effect
      
      list(
        weighted_data = result,
        before_stats = before_stats,
        after_stats = after_stats,
        weight_factors = weight_factors_list,
        design_effect = design_effect,
        effective_sample_size = effective_sample_size
      )
    }
    
    # Pondération des données
    observeEvent(input$process_data, {
      req(uploaded_data(), input$weighting_vars)
      
      # Afficher un message de progression
      withProgress(message = 'Pondération en cours...', value = 0.5, {
        weighting_result <- perform_weighting(
          uploaded_data(), 
          weights_prepared, 
          input$weighting_vars
        )
        
        weighted_data(weighting_result$weighted_data)
        weighting_details(weighting_result)
        
        showNotification("Pondération terminée", type = "message")
      })
    })
    
    # Statistiques de pondération - AMÉLIORÉES
    output$weighting_stats <- renderPrint({
      req(weighted_data(), weighting_details())
      
      details <- weighting_details()
      data <- weighted_data()
      
      cat("Statistiques de pondération:\n")
      cat("====================================\n")
      cat("Nombre total d'observations:", nrow(data), "\n")
      cat("Moyenne des poids:", round(mean(data$weight), 4), "\n")
      cat("Écart-type des poids:", round(sd(data$weight), 4), "\n")
      cat("Min des poids:", round(min(data$weight), 4), "\n")
      cat("Max des poids:", round(max(data$weight), 4), "\n")
      cat("Coefficient de variation:", 
          round(sd(data$weight) / mean(data$weight), 4), "\n\n")
      
      cat("Diagnostics avancés:\n")
      cat("====================================\n")
      cat("Design Effect:", round(details$design_effect, 2), "\n")
      cat("Taille effective de l'échantillon:", round(details$effective_sample_size), "\n")
      cat("Ratio d'efficacité:", round(100 * details$effective_sample_size / nrow(data), 1), "%\n\n")
      
      cat("Variables pondérées:", paste(input$weighting_vars, collapse=", "), "\n")
    })
    
   # Aperçu des données pondérées
output$weighted_data_preview <- renderDataTable({
  req(weighted_data())
  # Forcer la réactivité en récupérant les données maintenant
  data <- isolate(weighted_data())
  
  # S'assurer que les données sont disponibles avant de les afficher
  validate(need(!is.null(data), "Données pondérées non disponibles"))
  
  # Mettre la colonne de poids en premier et arrondir pour une meilleure présentation
  data_preview <- data %>%
    mutate(weight = round(weight, 4)) %>%
    select(weight, everything()) %>%
    head(10)
  
  # Préparation des données pour l'affichage
  data_preview
}, options = list(
  scrollX = TRUE,
  pageLength = 5,
  lengthMenu = c(5, 10, 15),
  columnDefs = list(
    list(
      targets = 0,  # La colonne de poids (première colonne)
      render = JS("function(data, type, row) {
        return type === 'display' ? 
          '<span style=\"font-weight: bold; color: #4e73df;\">' + 
          parseFloat(data).toFixed(4) + '</span>' : data;
      }")
    )
  )
))

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