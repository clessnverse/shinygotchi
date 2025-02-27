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
            actionButton(ns("process_data"), "Pondérer les données", 
                        class = "btn-success", 
                        icon = icon("balance-scale"))
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
              conditionalPanel(
                condition = paste0("input['", ns("process_data"), "'] > 0"),
                downloadButton(ns("download_weighted"), "Télécharger les données pondérées", 
                             class = "btn-primary mt-3", 
                             icon = icon("download"))
              )
            )
          )
        )
      )
    ),
    
    fluidRow(
      column(
        width = 12,
        conditionalPanel(
          condition = paste0("input['", ns("process_data"), "'] > 0"),
          div(
            class = "card mt-4",
            div(
              class = "card-body",
              h4("Résultats de la pondération", class = "card-title"),
              tabsetPanel(
                tabPanel("Statistiques", 
                         verbatimTextOutput(ns("weighting_stats")) %>% withSpinner()),
                tabPanel("Comparaison avant/après",
                         plotOutput(ns("comparison_plot"), height = "500px") %>% withSpinner())
              )
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
              tags$li("Cliquez sur 'Pondérer les données'"),
              tags$li("Examinez les résultats et les statistiques de pondération"),
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
    # Réactive pour stocker les données chargées
    uploaded_data <- reactiveVal(NULL)
    weighted_data <- reactiveVal(NULL)
    
    # Observer qui charge le fichier RDS quand il est téléchargé
    observeEvent(input$upload_rds, {
      req(input$upload_rds)
      
      tryCatch({
        # Charger les données
        data <- readRDS(input$upload_rds$datapath)
        uploaded_data(data)
        
        # Réinitialiser les données pondérées
        weighted_data(NULL)
      }, error = function(e) {
        showNotification(
          "Erreur lors du chargement du fichier RDS. Vérifiez le format.",
          type = "error", 
          duration = 10
        )
      })
    })
    
    # Afficher des informations sur les données téléchargées
    output$data_info <- renderUI({
      req(uploaded_data())
      data <- uploaded_data()
      
      tagList(
        p(paste("Nombre d'observations:", nrow(data))),
        p(paste("Nombre de variables:", ncol(data))),
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
    
    # Afficher un aperçu des données
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
    
    # Fonction pour pondérer les données
    perform_weighting <- function(data, weights) {
      # Variables à utiliser pour la pondération
      weighting_vars <- intersect(
        c("ses_age_4Cat", "ses_gender", "ses_educ_3Cat", 
          "ses_province", "ses_incomeCensus", "ses_language"),
        names(data)
      )
      
      if (length(weighting_vars) < 3) {
        return(list(
          success = FALSE,
          message = "Pas assez de variables communes pour la pondération. Au moins 3 variables sont nécessaires."
        ))
      }
      
      # Filtrer les poids
      weights_filtered <- weights %>%
        filter(variable %in% weighting_vars)
      
      # Calculer les poids
      weighted_result <- tryCatch({
        # Initialiser avec poids à 1
        result <- data %>% mutate(weight = 1.0)
        
        # Statistiques avant pondération
        before_stats <- lapply(weighting_vars, function(var) {
          data %>% 
            group_by(!!sym(var)) %>%
            summarise(count = n(), .groups = "drop") %>%
            mutate(proportion = count / sum(count))
        })
        names(before_stats) <- weighting_vars
        
        # Appliquer les poids pour chaque variable
        for (var in weighting_vars) {
          # Distribution actuelle
          current_dist <- data %>% 
            group_by(!!sym(var)) %>%
            summarise(count = n(), .groups = "drop") %>%
            mutate(proportion = count / sum(count))
          
          # Distribution cible
          target_dist <- weights_filtered %>%
            filter(variable == var) %>%
            group_by(value) %>%
            summarise(population = sum(population), .groups = "drop") %>%
            mutate(target_proportion = population / sum(population))
          
          # Calculer les facteurs de pondération
          weight_factors <- current_dist %>%
            left_join(target_dist %>% select(value, target_proportion), 
                     by = setNames("value", var)) %>%
            mutate(factor = ifelse(!is.na(target_proportion), 
                                  target_proportion / proportion,
                                  1))
          
          # Appliquer les facteurs
          result <- result %>%
            left_join(weight_factors %>% select(!!sym(var), factor), 
                     by = setNames(var, var)) %>%
            mutate(weight = weight * ifelse(!is.na(factor), factor, 1),
                  factor = NULL)
        }
        
        # Normaliser les poids
        mean_weight <- mean(result$weight, na.rm = TRUE)
        result <- result %>% mutate(weight = weight / mean_weight)
        
        # Statistiques après pondération
        after_stats <- list()
        for (var in weighting_vars) {
          after_stats[[var]] <- result %>% 
            group_by(!!sym(var)) %>%
            summarise(weighted_count = sum(weight), .groups = "drop") %>%
            mutate(proportion = weighted_count / sum(weighted_count))
        }
        
        list(
          success = TRUE,
          data = result,
          before_stats = before_stats,
          after_stats = after_stats,
          weighting_vars = weighting_vars
        )
      }, error = function(e) {
        list(
          success = FALSE,
          message = paste("Erreur lors de la pondération:", e$message)
        )
      })
      
      return(weighted_result)
    }
    
    # Déclencher la pondération quand le bouton est cliqué
    observeEvent(input$process_data, {
      req(uploaded_data())
      
      # Afficher une notification pendant le traitement
      id <- showNotification(
        "Pondération des données en cours...",
        type = "message",
        duration = NULL,
        closeButton = FALSE
      )
      
      # Effectuer la pondération
      result <- perform_weighting(uploaded_data(), df_weights_clean)
      
      # Fermer la notification
      removeNotification(id)
      
      if (result$success) {
        weighted_data(result$data)
        
        # Afficher une notification de succès
        showNotification(
          "Pondération terminée avec succès!",
          type = "message",
          duration = 5
        )
      } else {
        # Afficher une notification d'erreur
        showNotification(
          result$message,
          type = "error",
          duration = 10
        )
      }
    })
    
    # Sortie pour les statistiques de pondération
    output$weighting_stats <- renderPrint({
      req(weighted_data())
      
      result <- perform_weighting(uploaded_data(), df_weights_clean)
      
      if (result$success) {
        cat("Statistiques de pondération:\n\n")
        
        cat("Poids calculés:\n")
        summary_stats <- summary(result$data$weight)
        print(summary_stats)
        
        cat("\nCoefficient de variation des poids:", 
            sd(result$data$weight) / mean(result$data$weight), "\n")
        
        cat("\nNombre d'observations avec un poids > 2:",
            sum(result$data$weight > 2), "\n")
        
        cat("\nNombre d'observations avec un poids < 0.5:",
            sum(result$data$weight < 0.5), "\n")
      } else {
        cat("Erreur lors de la pondération:\n")
        cat(result$message)
      }
    })
    
    # Graphique de comparaison
    output$comparison_plot <- renderPlot({
      req(weighted_data())
      
      result <- perform_weighting(uploaded_data(), df_weights_clean)
      
      if (!result$success) return(NULL)
      
      # Sélectionner la première variable pour la visualisation
      var_to_plot <- result$weighting_vars[1]
      
      # Préparer les données avant pondération
      before_data <- result$before_stats[[var_to_plot]] %>%
        rename(category = 1) %>%
        mutate(source = "Avant pondération")
      
      # Préparer les données après pondération
      after_data <- result$after_stats[[var_to_plot]] %>%
        rename(category = 1) %>%
        select(category, proportion) %>%
        mutate(source = "Après pondération")
      
      # Préparer les données cibles
      target_data <- df_weights_clean %>%
        filter(variable == var_to_plot) %>%
        group_by(value) %>%
        summarise(population = sum(population), .groups = "drop") %>%
        mutate(proportion = population / sum(population)) %>%
        rename(category = value) %>%
        select(category, proportion) %>%
        mutate(source = "Données de recensement")
      
      # Combiner toutes les données
      plot_data <- bind_rows(before_data, after_data, target_data)
      
      # Créer le graphique
      ggplot(plot_data, aes(x = category, y = proportion, fill = source)) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_fill_manual(values = c("Avant pondération" = "#3498DB",
                                    "Après pondération" = "#18BC9C",
                                    "Données de recensement" = "#F39C12")) +
        labs(title = paste("Comparaison des distributions pour", var_to_plot),
             x = "Catégorie",
             y = "Proportion",
             fill = "Source") +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "top"
        )
    })
    
    # Téléchargement des données pondérées
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
