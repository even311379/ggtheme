#' Minimal Theme for ggplot2 with Additional Options
#'
#' Adds a minimal theme to your \code{ggplot} object. Allows you to
#' specify font size and family, colour of gridlines, which gridlines
#' to draw, legend position and transparency level of legend
#' background. Allows you to switch between white and transparent
#' background.
#'
#' @param base_size font base size in points \emph{(numeric, length == 1)}
#' @param base_family font family (e.g. "sans", "serif", "mono")
#'   \emph{(character, length == 1)}
#' @param grid which grid lines to draw: "x", "y", "xy" or "n"one
#'   \emph{(character, length == 1)}
#' @param grid_colour the colour of the grid lines \emph{(character, length
#'   == 1)}
#' @param legend_pos the position of the legend; either a character
#'   specification ("left", "right", "top", "bottom") or a 2 element
#'   numeric vector giving the relative x,y position in [0,1]
#'   \emph{(character, length == 1 | numeric, length == 2)}
#' @param legend_bg_alpha the alpha (transparency) level of the white
#'   legend background in [0,1] \emph{(numeric, length == 1)}
#' @param transparent_bg switches between transparent (TRUE) and white
#'   (FALSE) plot background \emph{(logical, length == 1)}
#'
#' @author Jonas Schoeley, Hadley Wickham
#'
#' @note Forked from \pkg{ggplot2} \code{theme-defaults.r} by Hadley
#'   Wickham.
#'
#' @seealso \code{\link{ggplot2}}, \code{\link{theme}}
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point() +
#'   ggtheme_min()
#'
#' @import ggplot2
#' @importFrom scales alpha
#' @importFrom grid unit
#'
#' @export
ggtheme_min <- function(base_size = 12, base_family = "serif",
                        grid = "y", grid_colour = "grey90",
                        legend_pos = "right",  legend_bg_alpha = 0,
                        transparent_bg = TRUE) {

  # background transparency
  if (identical(transparent_bg, TRUE)) plot_background <- "transparent"
  if (identical(transparent_bg, FALSE)) plot_background <- "white"

  # grid lines switch
  grid_x <- element_blank(); grid_y <- element_blank()
  if (identical(grid, "x")) grid_x <- element_line(colour = grid_colour)
  if (identical(grid, "y")) grid_y <- element_line(colour = grid_colour)
  if (identical(grid, "xy")) {
    grid_x <- element_line(colour = grid_colour)
    grid_y <- element_line(colour = grid_colour)
  }
  if (identical(grid, "n")) {
    grid_x <- element_blank(); grid_y <- element_blank()
  }

  theme(
    # Elements in this first block aren't used directly, but are inherited
    # by others
    line = element_line(colour = "black", size = 0.5, linetype = 1,
                        lineend = "butt"),
    rect = element_rect(fill = "white", colour = "black", size = 0.5, linetype = 1),
    text = element_text(family = base_family, face = "plain",
                        colour = "black", size = base_size,
                        hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9),
    axis.text = element_text(size = rel(0.8), colour = "black"),
    strip.text = element_text(size = rel(0.8)),
    axis.line = element_blank(),
    axis.text.x = element_text(vjust = 1),
    axis.text.y = element_text(hjust = 1),
    axis.ticks = element_blank(),
    axis.title.x = element_text(),
    axis.title.y = element_text(angle = 90),
    axis.ticks.length = unit(0, "cm"),
    axis.ticks.margin = unit(0.1, "cm"),
    legend.background = element_rect(fill = alpha("white", legend_bg_alpha),
                                     colour = NA),
    legend.margin = unit(0.2, "cm"),
    legend.key = element_blank(),
    legend.key.size = unit(1, "lines"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.text = element_text(size = rel(0.8)),
    legend.text.align = NULL,
    legend.title = element_text(size = rel(0.8), face = "bold", hjust = 0),
    legend.title.align = NULL,
    legend.position = legend_pos,
    legend.direction = NULL,
    legend.justification = "center",
    legend.box = NULL,
    legend.box.just = "left",
    panel.background = element_rect(fill = plot_background, colour = NA),
    panel.border = element_blank(),
    panel.grid.major.x = grid_x,
    panel.grid.major.y = grid_y,
    panel.grid.minor = element_blank(),
    panel.margin = unit(0.25, "lines"),
    panel.margin.x = NULL,
    panel.margin.y = NULL,
    strip.background = element_blank(),
    strip.text.x = element_text(),
    strip.text.y = element_text(angle = -90),
    plot.background = element_rect(fill = plot_background, colour = NA),
    plot.title = element_text(size = rel(1.2)),
    plot.margin = unit(c(1, 1, 0.5, 0.5), "lines"),
    complete = TRUE
  )
}
