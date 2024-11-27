# Chargement des bibliothèques nécessaires
library(shiny)
library(ggplot2)
library(dplyr)
library(rlang)

# Interface Utilisateur (UI)
ui <- fluidPage(
  titlePanel("Application Shiny avec Manipulation de Données et Visualisation"),
  
  # Création des onglets
  tabsetPanel(
    tabPanel("Manipulation des Données",
             sidebarLayout(
               sidebarPanel(
                 fileInput('datafile', 'Importer un fichier RDS', accept = c('.rds')),
                 
                 hr(),
                 h4("Création de Nouvelles Variables"),
                 
                 # Interface pour créer une nouvelle variable
                 textInput('new_var_name', 'Nom de la Nouvelle Variable', value = ""),
                 selectInput('operation', 'Opération', choices = c('Transformation' = 'transform', 'Recodage' = 'recode')),
                 uiOutput('var_transform_ui'),
                 actionButton('apply_mutation', 'Créer la Variable'),
                 
                 hr(),
                 h4("Filtrage des Données"),
                 
                 # Interface pour filtrer les données
                 uiOutput('filter_var_ui'),
                 selectInput('filter_operator', 'Opérateur', choices = c('=', '!=', '>', '<', '>=', '<=')),
                 uiOutput('filter_value_ui'),
                 actionButton('apply_filter', 'Appliquer le Filtre'),
                 
                 hr(),
                 h4("Exportation"),
                 
                 # Bouton pour télécharger les données manipulées
                 downloadButton('download_data', 'Télécharger les données manipulées')
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

# Serveur
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
      mutated_data(df)   # Initialiser mutated_data avec les données importées
      filtered_data(df)  # Initialiser filtered_data avec les données importées
      showNotification("Données importées avec succès.", type = "message")
    } else {
      showNotification("Veuillez importer un fichier RDS.", type = "error")
      data(NULL)
    }
  })
  
  # Données après les mutations (création de nouvelles variables)
  mutated_data <- reactiveVal(NULL)
  
  # Données filtrées
  filtered_data <- reactiveVal(NULL)
  
  # UI pour la création de nouvelles variables
  output$var_transform_ui <- renderUI({
    req(input$operation)
    if (input$operation == 'transform') {
      tagList(
        selectInput('transform_var', 'Variable à Transformer', choices = names(mutated_data())),
        selectInput('transform_func', 'Fonction', choices = c('Logarithme' = 'log', 'Carré' = 'square', 'Racine Carrée' = 'sqrt'))
      )
    } else if (input$operation == 'recode') {
      tagList(
        selectInput('recode_var', 'Variable à Recoder', choices = names(mutated_data())),
        textAreaInput('recode_rules', 'Règles de Recodage', placeholder = "Exemple :\nold_value1 = new_value1\nold_value2 = new_value2", rows = 5)
      )
    }
  })
  
  # Appliquer la création de la nouvelle variable
  observeEvent(input$apply_mutation, {
    req(mutated_data())
    df <- mutated_data()
    new_var <- input$new_var_name
    
    if (new_var == "") {
      showNotification("Veuillez entrer un nom pour la nouvelle variable.", type = "error")
      return()
    }
    
    if (input$operation == 'transform') {
      req(input$transform_var, input$transform_func)
      var <- input$transform_var
      func <- input$transform_func
      
      if (func == 'log') {
        df <- df %>% mutate(!!new_var := log(.data[[var]]))
      } else if (func == 'square') {
        df <- df %>% mutate(!!new_var := (.data[[var]])^2)
      } else if (func == 'sqrt') {
        df <- df %>% mutate(!!new_var := sqrt(.data[[var]]))
      }
    } else if (input$operation == 'recode') {
      req(input$recode_var, input$recode_rules)
      var <- input$recode_var
      rules <- strsplit(input$recode_rules, "\n")[[1]]
      
      recode_list <- setNames(
        sapply(rules, function(x) trimws(strsplit(x, "=")[[1]][2])),
        sapply(rules, function(x) trimws(strsplit(x, "=")[[1]][1]))
      )
      
      df <- df %>% mutate(!!new_var := recode(as.character(.data[[var]]), !!!recode_list))
    }
    
    mutated_data(df)
    filtered_data(df)  # Mettre à jour les données filtrées
    showNotification("Nouvelle variable créée avec succès.", type = "message")
  })
  
  # UI pour sélectionner la variable de filtrage
  output$filter_var_ui <- renderUI({
    req(mutated_data())
    selectInput('filter_var', 'Variable à Filtrer', choices = names(mutated_data()))
  })
  
  # UI pour saisir la valeur de filtrage
  output$filter_value_ui <- renderUI({
    req(input$filter_var)
    df <- mutated_data()
    var_values <- unique(df[[input$filter_var]])
    if (is.numeric(df[[input$filter_var]])) {
      numericInput('filter_value', 'Valeur', value = var_values[1])
    } else {
      selectInput('filter_value', 'Valeur', choices = var_values)
    }
  })
  
  # Appliquer le filtrage
  observeEvent(input$apply_filter, {
    req(mutated_data())
    df <- mutated_data()
    
    req(input$filter_var, input$filter_operator, input$filter_value)
    
    # Construire l'expression de filtrage
    if (is.numeric(df[[input$filter_var]])) {
      filter_expr <- paste0("`", input$filter_var, "`", input$filter_operator, input$filter_value)
    } else {
      filter_expr <- paste0("`", input$filter_var, "`", input$filter_operator, "'", input$filter_value, "'")
    }
    
    tryCatch({
      df_filtered <- df %>% filter(eval(parse(text = filter_expr)))
      filtered_data(df_filtered)
      showNotification("Données filtrées avec succès.", type = "message")
    }, error = function(e) {
      showNotification(paste("Erreur dans le filtrage :", e$message), type = "error")
    })
  })
  
  # Télécharger les données manipulées
  output$download_data <- downloadHandler(
    filename = function() {
      paste('data_manipulees', '.rds', sep = '')
    },
    content = function(file) {
      req(filtered_data())
      saveRDS(filtered_data(), file)
    }
  )
  
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

# Exécution de l'application Shiny
shinyApp(ui = ui, server = server)
