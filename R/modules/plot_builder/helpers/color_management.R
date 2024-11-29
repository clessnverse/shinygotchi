# R/modules/plot_builder/helpers/color_management.R

create_color_inputs <- function(ns, fill_variable, data) {
  req(fill_variable != "None")
  
  fill_values <- unique(data[[fill_variable]])
  fill_values <- fill_values[!is.na(fill_values)]
  
  tagList(
    h4("Customize colors:"),
    lapply(fill_values, function(value) {
      colourInput(
        inputId = ns(paste0("color_", make.names(as.character(value)))),
        label = paste("Color for", value),
        value = "#000000"
      )
    })
  )
}

get_color_values <- function(input, fill_values) {
  sapply(fill_values, function(value) {
    input[[paste0("color_", make.names(as.character(value)))]]
  })
}
