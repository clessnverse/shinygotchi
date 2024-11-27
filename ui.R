# ui.R

# Load the necessary libraries for the UI components
library(shiny)
library(ggplot2)
library(dplyr)
library(rlang)

# Define the user interface of the Shiny app
ui <- fluidPage(
  # Application title
  titlePanel("Application Shiny avec Manipulation de Données et Visualisation"),
  
  # Create a set of tabs in the UI
  tabsetPanel(
    # First tab: Data Manipulation
    tabPanel("Manipulation des Données",
             # Layout with sidebar and main panel
             sidebarLayout(
               sidebarPanel(
                 # File input to upload an RDS file
                 fileInput('datafile', 'Importer un fichier RDS', accept = c('.rds')),
                 
                 hr(),
                 h4("Manipulation de Données"),
                 
                 # Text area for users to input expressions to create new variables
                 textAreaInput('mutate_expr', 'Créer de nouvelles variables (optionnel)', 
                               placeholder = "Exemple :\nrepublican_vote = ifelse(true_party_id %in% c('strong_republican', 'soft_republican'), 1, 0)\nincome_group_3 = case_when(\n  ses_income_large_groups == 'low' ~ 'Revenu faible',\n  ses_income_large_groups %in% c('mid', 'upper_mid') ~ 'Revenu moyen',\n  ses_income_large_groups == 'high' ~ 'Revenu élevé'\n)",
                               rows = 8),
                 
                 # Button to update variables based on the expressions provided
                 actionButton('update_vars', 'Mettre à jour les variables'),
                 
                 # Text input for filtering data using expressions
                 textInput('filter_expr', 'Expression de Filtrage (optionnel)', placeholder = "Ex: age > 30 & gender == 'Female'"),
                 
                 # Button to update the data filtering
                 actionButton('update_filter', 'Mettre à jour le filtrage'),
                 
                 hr(),
                 h4("Exportation"),
                 
                 # Text input to specify the path for exporting manipulated data
                 textInput('export_path', 'Chemin pour exporter les données manipulées', value = "data_manipulees.rds"),
                 
                 # Button to export the manipulated data
                 actionButton('export_data', 'Exporter les données')
               ),
               mainPanel(
                 h4("Aperçu des Données Manipulées"),
                 # Output area to display a preview of the manipulated data
                 verbatimTextOutput('data_preview_manipulation')  # To display a preview of manipulated data
               )
             )
    ),
    # Second tab: Visualization
    tabPanel("Visualisation",
             sidebarLayout(
               sidebarPanel(
                 h4("Visualisation"),
                 
                 # UI output for selecting grouping variables
                 uiOutput("group_vars_ui"),
                 
                 # UI output for selecting the variable to summarize
                 uiOutput("summary_var_ui"),
                 
                 # Dropdown to select the summary function
                 selectInput('summary_func', 'Fonction de Résumé',
                             choices = c('Moyenne' = 'mean', 'Somme' = 'sum', 'Compte' = 'n', 'Proportion' = 'proportion', 'Moyenne pondérée' = 'weighted.mean')),
                 
                 # UI output for selecting the weighting variable (if weighted mean is selected)
                 uiOutput("weight_var_ui"),
                 
                 hr(),
                 
                 # UI outputs for selecting variables for X and Y axes
                 uiOutput("xvar_select"),
                 uiOutput("yvar_select"),
                 
                 # Dropdown to select the type of plot
                 selectInput('plot_type', 'Type de graphique', 
                             choices = c('Barres' = 'bar', 'Points' = 'point', 'Ligne' = 'line')),
                 
                 # Button to generate the plot
                 actionButton('generate_plot', 'Générer le graphique')
               ),
               mainPanel(
                 # Output area to display the plot
                 plotOutput('plot'),
                 h4("Aperçu des Données Résumées"),
                 # Output area to display a preview of the summarized data
                 verbatimTextOutput('data_preview_visualisation')  # To display a preview of summarized data
               )
             )
    )
  )
)
