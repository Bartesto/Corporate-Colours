library(ggplot2)
theme_set(theme_minimal())
# dbca corporate colors
dbca_colours <- c(
  `red`        = "#cc3300",
  `green`      = "#475945",
  `light green` = "#71ce3d",
  `blue`       = "#00227b",
  `light blue` = "#00b3ff",
  `orange`     = "#ff8000",
  `yellow`     = "#ffc426",
  `light grey` = "#cccccc",
  `dark grey`  = "#8c8c8c")

#' Function to extract dbca colours as hex codes
#'
#' @param ... Character names of dbca_colours 
#'
dbca_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (dbca_colours)
  
  dbca_colours[cols]
}
dbca_cols("orange")
dbca_cols()


ggplot(mtcars, aes(hp, mpg)) +
  geom_point(color = dbca_cols("green"),
             size = 4, alpha = .8)

dbca_palettes <- list(
  `main`  = dbca_cols("blue", "light blue", "yellow", "green"),
  
  `cool`  = dbca_cols("blue", "light blue", "light green", "green"),
  
  `hot`   = dbca_cols("yellow", "orange", "red"),
  
  `mixed` = dbca_cols("blue", "light green", "yellow", "orange", "red"),
  
  `grey`  = dbca_cols("light grey", "dark grey")
)


#' Return function to interpolate a dbca colour palette
#'
#' @param palette Character name of palette in dbca_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#'
dbca_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- dbca_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}


dbca_pal("cool")(10)


#' Color scale constructor for dbca colours
#'
#' @param palette Character name of palette in dbca_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_colour_dbca <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- dbca_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("dbca_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Fill scale constructor for dbca colours
#'
#' @param palette Character name of palette in dbca_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_fill_dbca <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- dbca_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("dbca_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

# Color by discrete variable using default palette
ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
  geom_point(size = 4) +
  scale_colour_dbca()

# Color by numeric variable with cool palette
ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Sepal.Length)) +
  geom_point(size = 4, alpha = .6) +
  scale_colour_dbca(discrete = FALSE, palette = "hot")


# Fill by discrete variable with different palette + remove legend (guide)
ggplot(mpg, aes(manufacturer, fill = manufacturer)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_dbca(palette = "mixed", guide = "none", discrete = TRUE)
