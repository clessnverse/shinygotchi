# R/utils/themes.R
create_custom_theme <- function(text_size) {
  tryCatch({
    theme_datagotchi_light(base_size = text_size)
  }, error = function(e) {
    # Fallback theme if everything fails
    theme_minimal(base_size = text_size) +
      theme(
        text = element_text(family = "sans-serif"),
        axis.text.x = element_text(size = text_size),
        axis.text.y = element_text(size = text_size),
        legend.text = element_text(size = text_size),
        legend.title = element_text(size = text_size),
        plot.title = element_text(size = text_size + 2),
        axis.title = element_text(size = text_size)
      )
  })
}
