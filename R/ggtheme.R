#' Minimal Theme for ggplot2 with Additional Options
#'
#' Adds a minimal theme to your \code{ggplot} object. Allows you to
#' specify font size and family, colour of gridlines, which gridlines
#' to draw, legend position and transparency level of legend
#' background. Allows you to switch between white and transparent
#' background.
#'
#' @param base_size font base size in points
#' @param base_family font family (e.g. "sans", "serif", "mono")
#' @param grid which grid lines to draw: "x", "y", "xy" or "n"one
#' @param grid_colour the colour of the grid lines
#' @param legend_pos the position of the legend; either a character
#'   specification ("left", "right", "top", "bottom") or a 2 element
#'   numeric vector giving the relative x,y position in [0,1]
#' @param legend_bg_alpha the alpha (transparency) level of the white
#'   legend background in [0,1]
#' @param transparent_bg switches between transparent (TRUE) and white
#'   (FALSE) plot background
#'
#' @author Jonas Schoeley, Hadley Wickham
#'
#' @note Forked from \pkg{ggplot2} \code{theme-defaults.r} by Hadley
#'   Wickham.
#'
#' @seealso \code{\link[ggplot2]{theme}}
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point() +
#'   ggtheme_min()
#'
#' @import ggplot2
#'
#' @export
ggtheme_min <- function(base_size = 12, base_family = "serif",
                        grid = "y", grid_colour = "grey90",
                        legend_pos = "right",  legend_bg_alpha = 0,
                        transparent_bg = TRUE) {
  half_line <- base_size / 2

  # background transparency
  if (identical(transparent_bg, TRUE)) plot_background <- "transparent"
  if (identical(transparent_bg, FALSE)) plot_background <- "white"

  # grid lines switch
  grid_x <- element_blank(); grid_y <- element_blank()
  if (identical(grid, "x")) grid_x <- element_line(colour = grid_colour, size = 0.5)
  if (identical(grid, "y")) grid_y <- element_line(colour = grid_colour, size = 0.5)
  if (identical(grid, "xy")) {
    grid_x <- element_line(colour = grid_colour, size = 0.5)
    grid_y <- element_line(colour = grid_colour, size = 0.5)
  }
  if (identical(grid, "n")) {
    grid_x <- element_blank(); grid_y <- element_blank()
  }

  theme(
    # Elements in this first block aren't used directly, but are inherited
    # by others
    line =               element_line(colour = "black", size = 0.5, linetype = 1,
                                      lineend = "butt"),
    rect =               element_rect(fill = "white", colour = "black",
                                      size = 0.5, linetype = 1),
    text =               element_text(
      family = base_family, face = "plain",
      colour = "black", size = base_size,
      lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0,
      margin = margin(), debug = FALSE
    ),

    axis.line =          element_blank(),
    axis.text =          element_text(size = rel(0.8), colour = "black"),
    axis.text.x =        element_text(margin = margin(t = 0.8 * half_line / 2), vjust = 1),
    axis.text.y =        element_text(margin = margin(r = 0.8 * half_line / 2), hjust = 1),
    axis.ticks =         element_blank(),
    axis.ticks.length =  grid::unit(0, "cm"),
    axis.title.x =       element_text(
      margin = margin(t = 0.8 * half_line, b = 0.8 * half_line / 2)
    ),
    axis.title.y =       element_text(
      angle = 90,
      margin = margin(r = 0.8 * half_line, l = 0.8 * half_line / 2)
    ),

    legend.background =  element_rect(fill = scales::alpha("white", legend_bg_alpha),
                                      colour = NA),
    legend.margin =      grid::unit(0.2, "cm"),
    legend.key =         element_blank(),
    legend.key.size =    grid::unit(1.2, "lines"),
    legend.key.height =  NULL,
    legend.key.width =   NULL,
    legend.text =        element_text(size = rel(0.8)),
    legend.text.align =  NULL,
    legend.title =       element_text(face = "bold", hjust = 0),
    legend.title.align = NULL,
    legend.position =    legend_pos,
    legend.direction =   NULL,
    legend.justification = "center",
    legend.box =         NULL,

    panel.background =   element_rect(fill = plot_background, colour = NA),
    panel.border =       element_blank(),
    panel.grid.major.x = grid_x,
    panel.grid.major.y = grid_y,
    panel.grid.minor =   element_blank(),
    panel.margin =       grid::unit(half_line, "pt"),
    panel.margin.x =     NULL,
    panel.margin.y =     NULL,
    panel.ontop    =     FALSE,

    strip.background =   element_blank(),
    strip.text.x =       element_text(margin = margin(t = half_line, b = half_line)),
    strip.text.y =       element_text(angle = -90, margin = margin(l = half_line, r = half_line)),
    strip.switch.pad.grid = grid::unit(0.1, "cm"),
    strip.switch.pad.wrap = grid::unit(0.1, "cm"),

    plot.background =    element_rect(fill = plot_background, colour = NA),
    plot.title =         element_text(
      size = rel(1.2),
      margin = margin(b = half_line * 1.2)
    ),
    plot.margin =        margin(half_line, half_line, half_line, half_line),

    complete = TRUE
  )
}
