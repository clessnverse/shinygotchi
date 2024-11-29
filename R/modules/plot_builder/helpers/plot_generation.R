# R/modules/plot_builder/helpers/plot_generation.R library(ggplot2)

create_bar_plot <- function(data, x_var, y_var, fill_var = NULL, 
                           facet_var = NULL, color_values = NULL) {
  p <- ggplot(data) +
    aes(x = .data[[x_var]], 
        y = .data[[y_var]],
        fill = if (!is.null(fill_var)) as.factor(.data[[fill_var]]) else NULL) +
    geom_bar(stat = "identity", position = "dodge") +
    theme_minimal()
    
  if (!is.null(color_values)) {
    p <- p + scale_fill_manual(values = color_values)
  }
  
  if (!is.null(facet_var)) {
    p <- p + facet_wrap(vars(.data[[facet_var]]))
  }
  
  return(p)
}

create_scatter_plot <- function(data, x_var, y_var, color_var = NULL, 
                              facet_var = NULL, color_values = NULL) {
  # Similar structure for scatter plots
}
