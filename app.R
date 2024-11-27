library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Visualisation de données à partir de fichiers RDS"),

  sidebarLayout(
    sidebarPanel(
      fileInput('datafile', 'Importer un fichier RDS',
                accept = c('.rds')),
      uiOutput("xvar_select"),
      uiOutput("yvar_select"),
      selectInput('plot_type', 'Type de graphique', 
                  choices = c('Points' = 'point', 'Ligne' = 'line', 'Barres' = 'bar'))
    ),
    mainPanel(
      plotOutput('plot')
    )
  )
)

server <- function(input, output, session) {

  # Lecture du fichier de données
  data <- reactive({
    req(input$datafile)
    infile <- input$datafile
    # Vérifier l'extension du fichier
    ext <- tools::file_ext(infile$name)
    if (ext == "rds") {
      df <- readRDS(infile$datapath)
      return(df)
    } else {
      showNotification("Veuillez importer un fichier RDS.", type = "error")
      return(NULL)
    }
  })

  # Sélecteur pour la variable X
  output$xvar_select <- renderUI({
    req(data())
    selectInput('xvar', 'Variable pour l\'axe X', names(data()))
  })

  # Sélecteur pour la variable Y
  output$yvar_select <- renderUI({
    req(data())
    selectInput('yvar', 'Variable pour l\'axe Y', names(data()))
  })

  # Génération du graphique
  output$plot <- renderPlot({
    req(input$xvar, input$yvar)
    plot_data <- data()
    ggplot(plot_data, aes_string(x = input$xvar, y = input$yvar)) +
      {
        if (input$plot_type == 'point') {
          geom_point(color = "#2c3e50", size = 3)
        } else if (input$plot_type == 'line') {
          geom_line(color = "#2c3e50", size = 1)
        } else if (input$plot_type == 'bar') {
          geom_bar(stat = 'identity', fill = "#2c3e50")
        }
      } +
      theme_minimal() +
      labs(x = input$xvar, y = input$yvar)
  })
}

shinyApp(ui = ui, server = server)
