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
            "Religious Affiliation" = "ses_religiosity",
            "Housing Status" = "ses_owner",
            "Religious Groups" = "ses_religion",
            "Sexual Orientation" = "ses_orientation_factor"
          )
        )
    ),
    
    mainPanel(
      
    )
  )
}

socialGroupsServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
   
    df_soial_groups <- readRDS("data/df_canada.rds")

    lifestyle_vars <- list(
      # Activities
      "Vote choice" = "dv_vote_choice", # Barplot x = social_var, y = summarise(n = n() / nrow(data)), fill = dv_vote_choice. Ne pas oublier de mettre les couleurs officielles. LO
      "Left vs Right" = "dv_attitude_leftvsright", # Dotplot avec coordflip Etienne
      "Turnout" = "dv_turnout", # dotplot avec coordflip Etienne
      "Hunting" = "lifestyle_hunting_freq_numeric", # barplot LO
      "Manual Tasks" = "lifestyle_manual_tasks_freq_numeric", # barplot avec art Etienne 
      "Art" = "lifestyle_performing_arts_freq_numeric", # barplot avec manual Etienne
      "Transport" = "lifestyle_choice_transport_clean", # Même que vote choice LO 
    )

    social_vars <- list(
      "Age Groups" = "ses_age_group",
      "Gender" = "ses_gender_factor",
      "Geographic Location" = "ses_region",
      "Language" = "ses_language",
      "Socioeconomic Status" = "ses_income",
      "Education" = "ses_education_group",
      "Ethnicity" = "ses_ethnicity",
      "Religious Affiliation" = "ses_religiosity",
      "Housing Status" = "ses_owner",
      "Religious Groups" = "ses_religion",
      "Sexual Orientation" = "ses_orientation_factor"
    ) 
  })
}

# Example usage in app.R:
# ui <- fluidPage(
#   socialGroupsUI("social_explorer")
# )
# 
# server <- function(input, output, session) {
#   socialGroupsServer("social_explorer", your_data)
# }
# 
# shinyApp(ui, server)
# Module UI



