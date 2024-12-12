# Load required packages
library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(tidyr)
library(shinydashboard)
library(shinyjs)
library(waiter)
library(shinycssloaders)
library(fresh)
library(bslib)
library(fontawesome)

# Source modules
source("R/utils/viz.R")
source("R/utils/themes.R")
font_init()
source("R/modules/plot_builder/plotBuilderModule.R")
source("R/modules/groups/groupsModule.R")

# Custom theme
my_theme <- bs_theme(
  version = 5,
  bootswatch = "pulse",
  primary = "#2C3E50",
  secondary = "#95A5A6",
  success = "#18BC9C",
  info = "#3498DB",
  warning = "#F39C12",
  danger = "#E74C3C"
) %>%
  bs_add_rules(sass::sass_file("www/custom.scss"))

# UI Definition
ui <- tagList(
  useWaiter(), # Loading screen
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta3/css/all.min.css"),
    tags$style(HTML("
    @font-face {
      font-family: 'PixelOperatorSC';
      src: url('PixelOperatorSC.ttf') format('truetype');
    }

    /* Apply PixelOperatorSC globally */
    body, label, input, button, select, textarea, h1, h2, h3, h4, h5, h6 {
      font-family: 'PixelOperatorSC', monospace !important;
    }
  "))
  ),
  navbarPage(
    title = tagList(
      tags$img(src = "https://raw.githubusercontent.com/clessnverse/shinygotchi/refs/heads/main/www/datagotchi.png", height = "70px"),
      span(
        HTML("Explorateur de données<br/>Léger x Datagotchi"),
        style = "display: inline-block; text-align: center;"
      )
    ),
    theme = my_theme,
    id = "nav",

    # Groups Tab
    tabPanel(
      "Groupes sociaux",
      icon = icon("users"),
      div(
        class = "animate-fade-in",
        fluidRow(
          column(
            width = 12,
            div(
              class = "title-box",
              h2("Groupes sociaux"),
              p("Explorer les schémas démographiques et comportementaux à travers différents groupes sociaux.")
            )
          )
        ),
        socialGroupsUI("social_explorer")
      )
    ),
    tabPanel(
      "Enjeux",
      icon = icon("comments"), div(
        class = "animate-fade-in construction-container",
        div(
          class = "construction-content",
          tags$img(
            src = "construction-worker.gif",
            class = "construction-gif",
            alt = "Construction worker with jackhammer"
          ),
          h2("Section en Construction", class = "construction-title")
        )
      )
    ),
    # Lifestyles Tab
    tabPanel(
      "Lifestyles",
      icon = icon("heart"),
      div(
        class = "animate-fade-in construction-container",
        div(
          class = "construction-content",
          tags$img(
            src = "construction-worker.gif",
            class = "construction-gif",
            alt = "Construction worker with jackhammer"
          ),
          h2("Section en Construction", class = "construction-title")
        )
      )
    ),

    # Parties Tab
    tabPanel(
      "Partis",
      icon = icon("flag"),
      div(
        class = "animate-fade-in construction-container",
        div(
          class = "construction-content",
          tags$img(
            src = "construction-worker.gif",
            class = "construction-gif",
            alt = "Construction worker with jackhammer"
          ),
          h2("Section en Construction", class = "construction-title")
        )
      )
    ),

    # Ridings Tab
    tabPanel(
      "Circonscriptions",
      icon = icon("map-marker-alt"),
      div(
        class = "animate-fade-in construction-container",
        div(
          class = "construction-content",
          tags$img(
            src = "construction-worker.gif",
            class = "construction-gif",
            alt = "Construction worker with jackhammer"
          ),
          h2("Section en Construction", class = "construction-title")
        )
      )
    ),
    # Plot Builder Tab
    tabPanel(
      "Constructeur de graphiques",
      icon = icon("chart-bar"),
      div(
        class = "animate-fade-in",
        fluidRow(
          column(
            width = 12,
            div(
              class = "title-box",
              h2("Créateur de Graphiques Personnalisés."),
              p("Créez et personnalisez vos propres visualisations de données.")
            )
          )
        ),
        plotBuilderUI("plot_builder")
      )
    ),
    # Nouvel onglet "Partenaires"
    tabPanel(
      "Partenaires",
      icon = icon("handshake"),
      div(
        class = "partner-image-container",
        tags$img(src = "datagotchi_partners.png", alt = "Partenaire", class = "partner-image")
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  # Show loading screen
  waiter_show(
    html = tagList(
      spin_flower(),
      h3("Chargement de l'explorateur de données Datagotchi...", style = "color: #2C3E50;")
    ),
    color = "#ffffff"
  )

  # Initialize modules
  plotBuilderServer("plot_builder")
  socialGroupsServer("social_explorer", data)

  # Hide loading screen after delay
  Sys.sleep(1) # Simulate loading time
  waiter_hide()

  # Add tab switching animation
  observeEvent(input$nav, {
    shinyjs::runjs('
      $(".tab-pane").fadeOut(0);
      $(".tab-pane.active").fadeIn(500);
    ')
  })

  output$logo_image <- renderUI({
    tags$img(src = "datagotchi.png", height = "30px")
  })
}


# Run the application
shinyApp(ui = ui, server = server)
