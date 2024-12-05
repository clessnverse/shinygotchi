library(showtext)
library(sysfonts)

# Initialize the font
font_init <- function() {
  tryCatch({
    if (!("PixelOperatorSC" %in% font_families())) {
      font_add(
        family = "PixelOperatorSC",
        regular = "www/PixelOperatorSC.ttf"
      )
    }
    showtext_auto()
  }, error = function(e) {
    warning("Font loading failed, using sans-serif")
  })
}

# Simplified theme function
theme_datagotchi_light <- function(base_size = 11) {
  # Try to load font, fallback to sans-serif if it fails
  family <- tryCatch({
    font_init()
    "PixelOperatorSC"
  }, error = function(e) {
    "sans-serif"
  })
  
  theme_classic() +
    theme(
      text = element_text(size = base_size, family = family, colour = "grey30"),
      axis.text = element_text(colour = "grey30"),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank(),
      axis.line.x = element_blank(),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.background = element_rect(fill = NA),
      panel.grid.major.y = element_line(colour = "#f7f7f7"),
      plot.caption = element_text(hjust = 0, face = "italic"),
      plot.title = element_text(
        face = "bold",
        colour = "black",
        size = base_size * 1.5,
        hjust = 0.5
      ),
      plot.background = element_rect(fill = "white", colour = "white"),
      panel.background = element_rect(fill = NA),
      strip.background = element_blank()
    )
}
