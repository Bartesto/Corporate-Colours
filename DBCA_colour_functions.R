library(ggplot2)
theme_set(theme_minimal())
# dbca corporate colors
dbca_colours <- c(
  `red`        = "#cc3300",
  `green`      = "#1C5553",
  `light green` = "#7DA765",
  `blue`       = "#266093",
  `light blue` = "#00A8B4",
  `orange`     = "#F7941D",
  `yellow`     = "#FDC82F",
  `light grey` = "#cccccc",
  `dark grey`  = "#8c8c8c")

#lb #00b3ff

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



dbca_palettes <- list(
  `main`  = dbca_cols("blue", "light blue", "yellow", "green"),
  
  `cool`  = dbca_cols("blue", "light blue", "light green", "green"),
  
  `hot`   = dbca_cols("yellow", "orange", "red"),
  
  `mixed` = dbca_cols("blue", "light green", "yellow", "orange", "red"),
  
  `grey`  = dbca_cols("light grey", "dark grey"),
  
  `all`   = dbca_cols("red", "green", "light green", "blue", "light blue",
                      "orange", "yellow", "light grey", "dark grey")
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

# Use colours directly
ggplot(mtcars, aes(hp, mpg)) +
  geom_point(color = dbca_cols("blue"),
             size = 4, alpha = .8) +
  labs(title = "mtcars dataset demo",
       subtitle = "Calling colours directly",
       caption = "DBCA Corporate colours")

# Color by discrete variable using default palette
ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
  geom_point(size = 4, alpha = .8) +
  scale_colour_dbca() +
  labs(title = "iris dataset demo",
       subtitle = "Calling colours by discrete variable using default palette",
       caption = "DBCA Corporate colours")

# Color by numeric variable with cool palette
ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Sepal.Length)) +
  geom_point(size = 4, alpha = .8) +
  scale_colour_dbca(discrete = FALSE, palette = "cool") +
  labs(title = "iris dataset demo",
       subtitle = "Calling colours by numeric variable with cool palette",
       caption = "DBCA Corporate colours")

# Color by numeric variable with hot palette
ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Sepal.Length)) +
  geom_point(size = 4, alpha = .8) +
  scale_colour_dbca(discrete = FALSE, palette = "hot") +
  labs(title = "iris dataset demo",
       subtitle = "Calling colours by numeric variable with hot palette",
       caption = "DBCA Corporate colours")


# Fill by discrete variable with different palette + remove legend (guide)
ggplot(mpg, aes(manufacturer, fill = manufacturer)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_dbca(palette = "mixed", guide = "none", discrete = TRUE) +
  labs(title = "mpg dataset demo",
       subtitle = "Calling colours by discrete variable with different palette",
       caption = "DBCA Corporate colours")

cols <- c("red", "green", "light green", "blue", "light blue",
          "orange", "yellow", "light grey", "dark grey")

colsf <- factor(cols, levels = cols)

df <- data.frame(colour = colsf,
                 val = c(rep(10, 9)))
ggplot(df, aes(colour, fill = colour)) +
  geom_bar(width=1) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank()) +
  scale_fill_dbca(palette = "all", guide = "none", discrete = TRUE) +
  coord_flip() +
  geom_text(aes(x = colour, y = 1, label = colour),
            position = position_stack(vjust = .5), colour = "white", fontface = "italic")
  