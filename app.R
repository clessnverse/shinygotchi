library(shiny)
library(ggplot2)
library(dplyr)
library(rlang)
ui <- fluidPage(
  titlePanel("Application Shiny avec Manipulation de Données et Visualisation"),
  
  # Création des onglets
  tabsetPanel(
    tabPanel("Manipulation des Données",
             sidebarLayout(
               sidebarPanel(
                 fileInput('datafile', 'Importer un fichier RDS', accept = c('.rds')),
                 
                 hr(),
                 h4("Manipulation de Données"),
                 
                 # Zone de texte pour créer de nouvelles variables
                 textAreaInput('mutate_expr', 'Créer de nouvelles variables (optionnel)', 
                               placeholder = "Exemple :\nrepublican_vote = ifelse(true_party_id %in% c('strong_republican', 'soft_republican'), 1, 0)\nincome_group_3 = case_when(\n  ses_income_large_groups == 'low' ~ 'Revenu faible',\n  ses_income_large_groups %in% c('mid', 'upper_mid') ~ 'Revenu moyen',\n  ses_income_large_groups == 'high' ~ 'Revenu élevé'\n)",
                               rows = 8),
                 
                 # Bouton pour mettre à jour les variables
                 actionButton('update_vars', 'Mettre à jour les variables'),
                 
                 # Zone de texte pour filtrer les données
                 textInput('filter_expr', 'Expression de Filtrage (optionnel)', placeholder = "Ex: age > 30 & gender == 'Female'"),
                 
                 # Bouton pour mettre à jour le filtrage
                 actionButton('update_filter', 'Mettre à jour le filtrage'),
                 
                 hr(),
                 h4("Exportation"),
                 
                 # Champ pour spécifier le chemin d'exportation
                 textInput('export_path', 'Chemin pour exporter les données manipulées', value = "data_manipulees.rds"),
                 
                 # Bouton pour exporter les données
                 actionButton('export_data', 'Exporter les données')
               ),
               mainPanel(
                 h4("Aperçu des Données Manipulées"),
                 verbatimTextOutput('data_preview_manipulation')  # Pour afficher un aperçu des données manipulées
               )
             )
    ),
    tabPanel("Visualisation",
             sidebarLayout(
               sidebarPanel(
                 h4("Visualisation"),
                 
                 # Sélection de variables pour le regroupement
                 uiOutput("group_vars_ui"),
                 
                 # Sélection de la variable à résumer
                 uiOutput("summary_var_ui"),
                 
                 # Sélection de la fonction de résumé
                 selectInput('summary_func', 'Fonction de Résumé',
                             choices = c('Moyenne' = 'mean', 'Somme' = 'sum', 'Compte' = 'n', 'Proportion' = 'proportion', 'Moyenne pondérée' = 'weighted.mean')),
                 
                 # Sélection de la variable de pondération (si moyenne pondérée)
                 uiOutput("weight_var_ui"),
                 
                 hr(),
                 
                 # Sélection des variables pour les axes X et Y
                 uiOutput("xvar_select"),
                 uiOutput("yvar_select"),
                 
                 # Type de graphique
                 selectInput('plot_type', 'Type de graphique', 
                             choices = c('Barres' = 'bar', 'Points' = 'point', 'Ligne' = 'line')),
                 
                 # Bouton pour générer le graphique
                 actionButton('generate_plot', 'Générer le graphique')
               ),
               mainPanel(
                 plotOutput('plot'),
                 h4("Aperçu des Données Résumées"),
                 verbatimTextOutput('data_preview_visualisation')  # Pour afficher un aperçu des données résumées
               )
             )
    )
  )
)
server <- function(input, output, session) {
  
  # Lecture du fichier de données
  data <- reactiveVal(NULL)
  
  observeEvent(input$datafile, {
    req(input$datafile)
    infile <- input$datafile
    ext <- tools::file_ext(infile$name)
    if (ext == "rds") {
      df <- readRDS(infile$datapath)
      data(df)
      showNotification("Données importées avec succès.", type = "message")
    } else {
      showNotification("Veuillez importer un fichier RDS.", type = "error")
      data(NULL)
    }
  })
  
  # Données après les mutations (création de nouvelles variables)
  mutated_data <- reactiveVal(NULL)
  
  observeEvent(input$update_vars, {
    req(data())
    df <- data()
    
    # Appliquer les expressions de manipulation si fournies
    if (!is.null(input$mutate_expr) && input$mutate_expr != "") {
      tryCatch({
        # Parse the input into expressions
        exprs <- parse(text = input$mutate_expr)
        # Convert to list and use mutate with unquote-splicing
        df <- df %>% mutate(!!!as.list(exprs))
        showNotification("Variables mises à jour avec succès.", type = "message")
      }, error = function(e) {
        showNotification(paste("Erreur dans l'expression de manipulation :", e$message), type = "error")
      })
    } else {
      showNotification("Aucune expression de manipulation fournie.", type = "warning")
    }
    
    mutated_data(df)
  })
  
  # Données filtrées
  filtered_data <- reactiveVal(NULL)
  
  observeEvent(input$update_filter, {
    req(mutated_data())
    df <- mutated_data()
    
    # Filtrer les données si une expression de filtrage est fournie
    if (!is.null(input$filter_expr) && input$filter_expr != "") {
      tryCatch({
        df <- df %>% filter(!!parse(text = input$filter_expr))
        showNotification("Données filtrées avec succès.", type = "message")
      }, error = function(e) {
        showNotification(paste("Erreur dans l'expression de filtrage :", e$message), type = "error")
      })
    } else {
      showNotification("Aucune expression de filtrage fournie.", type = "warning")
    }
    
    filtered_data(df)
  })
  
  # Exporter les données manipulées
  observeEvent(input$export_data, {
    req(filtered_data())
    export_path <- input$export_path
    tryCatch({
      saveRDS(filtered_data(), export_path)
      showNotification(paste("Données exportées avec succès vers", export_path), type = "message")
    }, error = function(e) {
      showNotification(paste("Erreur lors de l'exportation :", e$message), type = "error")
    })
  })
  
  # Afficher un aperçu des données manipulées
  output$data_preview_manipulation <- renderPrint({
    req(filtered_data())
    head(filtered_data())
  })
  
  # --- Onglet Visualisation ---
  
  # Données manipulées pour la visualisation
  visual_data <- reactive({
    req(filtered_data())
    df <- filtered_data()
    return(df)
  })
  
  # Variables pour stocker les noms de colonnes
  group_vars_choices <- reactiveVal()
  summary_var_choices <- reactiveVal()
  weight_var_choices <- reactiveVal()
  
  # Mettre à jour les choix des menus déroulants lorsque visual_data() change
  observeEvent(visual_data(), {
    req(visual_data())
    cols <- names(visual_data())
    
    group_vars_choices(cols)
    summary_var_choices(cols)
    weight_var_choices(cols)
  })
  
  # UI pour sélectionner les variables de regroupement
  output$group_vars_ui <- renderUI({
    selectInput('group_vars', 'Variables de Regroupement', group_vars_choices(), multiple = TRUE)
  })
  
  # UI pour sélectionner la variable à résumer
  output$summary_var_ui <- renderUI({
    selectInput('summary_var', 'Variable à Résumer', summary_var_choices())
  })
  
  # UI pour sélectionner la variable de pondération (si moyenne pondérée)
  output$weight_var_ui <- renderUI({
    if (input$summary_func == 'weighted.mean') {
      selectInput('weight_var', 'Variable de Pondération', weight_var_choices())
    }
  })
  
  # Données résumées pour la visualisation
  summarized_data <- eventReactive(input$generate_plot, {
    df <- visual_data()
    
    req(input$summary_var)
    
    # Créer un symbole pour la variable à résumer
    summary_var_sym <- sym(input$summary_var)
    
    # Vérifier si la variable de pondération est nécessaire
    if (input$summary_func == 'weighted.mean') {
      req(input$weight_var)
      weight_var_sym <- sym(input$weight_var)
    }
    
    # Regrouper les données si des variables de regroupement sont sélectionnées
    if (!is.null(input$group_vars) && length(input$group_vars) > 0) {
      df_grouped <- df %>% group_by(across(all_of(input$group_vars)))
    } else {
      df_grouped <- df
    }
    
    # Appliquer la fonction de résumé appropriée
    if (input$summary_func == 'mean') {
      df_summary <- df_grouped %>%
        summarise(value = mean(as.numeric(!!summary_var_sym), na.rm = TRUE)) %>%
        ungroup()
    } else if (input$summary_func == 'sum') {
      df_summary <- df_grouped %>%
        summarise(value = sum(as.numeric(!!summary_var_sym), na.rm = TRUE)) %>%
        ungroup()
    } else if (input$summary_func == 'n') {
      df_summary <- df_grouped %>%
        summarise(value = n()) %>%
        ungroup()
    } else if (input$summary_func == 'proportion') {
      # Calcul de la proportion d'une catégorie spécifique
      # Demander à l'utilisateur de spécifier la valeur de la catégorie
      showModal(modalDialog(
        title = "Valeur de la Catégorie",
        textInput("category_value", "Entrez la valeur de la catégorie pour calculer la proportion :", value = ""),
        easyClose = TRUE,
        footer = tagList(
          modalButton("Annuler"),
          actionButton("ok_proportion", "OK")
        )
      ))
      
      # Attendre que l'utilisateur fournisse la valeur
      observeEvent(input$ok_proportion, {
        removeModal()
        category_value <- input$category_value
        
        df_summary <- df_grouped %>%
          summarise(value = mean(!!summary_var_sym == category_value, na.rm = TRUE)) %>%
          ungroup()
        
        # Mettre à jour les données résumées
        summarized_data <<- reactive({ df_summary })
        
        # Mettre à jour les sélecteurs de variables pour l'axe Y
        output$yvar_select <- renderUI({
          selectInput('yvar', 'Variable pour l\'axe Y', names(df_summary))
        })
        
        # Générer le graphique
        output$plot <- renderPlot({
          req(input$xvar, input$yvar)
          plot_data <- df_summary
          req(plot_data)
          
          p <- ggplot(plot_data, aes(x = .data[[input$xvar]], y = .data[[input$yvar]]))
          
          if (input$plot_type == 'bar') {
            p <- p + geom_bar(stat = 'identity', fill = "#2c3e50")
          } else if (input$plot_type == 'point') {
            p <- p + geom_point(color = "#2c3e50", size = 3)
          } else if (input$plot_type == 'line') {
            p <- p + geom_line(color = "#2c3e50", linewidth = 1)
          }
          
          p + theme_minimal() +
            labs(x = input$xvar, y = input$yvar)
        })
        
        # Afficher un aperçu des données résumées
        output$data_preview_visualisation <- renderPrint({
          req(df_summary)
          head(df_summary)
        })
      })
      
      # Sortir de la fonction pour éviter l'exécution du code suivant
      return(NULL)
    } else if (input$summary_func == 'weighted.mean') {
      df_summary <- df_grouped %>%
        summarise(value = weighted.mean(as.numeric(!!summary_var_sym), w = as.numeric(!!weight_var_sym), na.rm = TRUE)) %>%
        ungroup()
    } else {
      showNotification("Fonction de résumé non reconnue.", type = "error")
      return(NULL)
    }
    
    return(df_summary)
  })
  
  # UI pour sélectionner la variable X
  output$xvar_select <- renderUI({
    req(summarized_data())
    selectInput('xvar', 'Variable pour l\'axe X', names(summarized_data()))
  })
  
  # UI pour sélectionner la variable Y
  output$yvar_select <- renderUI({
    req(summarized_data())
    selectInput('yvar', 'Variable pour l\'axe Y', names(summarized_data()))
  })
  
  # Génération du graphique
  output$plot <- renderPlot({
    req(input$generate_plot)
    req(input$xvar, input$yvar)
    plot_data <- summarized_data()
    req(plot_data)
    
    p <- ggplot(plot_data, aes(x = .data[[input$xvar]], y = .data[[input$yvar]]))
    
    if (input$plot_type == 'bar') {
      p <- p + geom_bar(stat = 'identity', fill = "#2c3e50")
    } else if (input$plot_type == 'point') {
      p <- p + geom_point(color = "#2c3e50", size = 3)
    } else if (input$plot_type == 'line') {
      p <- p + geom_line(color = "#2c3e50", linewidth = 1)
    }
    
    p + theme_minimal() +
      labs(x = input$xvar, y = input$yvar)
  })
  
  # Afficher un aperçu des données résumées
  output$data_preview_visualisation <- renderPrint({
    req(summarized_data())
    head(summarized_data())
  })
}
shinyApp(ui = ui, server = server)
