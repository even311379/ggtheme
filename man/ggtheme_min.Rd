% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggtheme.R
\name{ggtheme_min}
\alias{ggtheme_min}
\title{Minimal Theme for ggplot2 with Additional Options}
\usage{
ggtheme_min(base_size = 12, base_family = "serif", grid = "y",
  grid_colour = "grey90", legend_pos = "right", legend_bg_alpha = 0,
  transparent_bg = TRUE)
}
\arguments{
\item{base_size}{font base size in points}

\item{base_family}{font family (e.g. "sans", "serif", "mono")}

\item{grid}{which grid lines to draw: "x", "y", "xy" or "n"one}

\item{grid_colour}{the colour of the grid lines}

\item{legend_pos}{the position of the legend; either a character
specification ("left", "right", "top", "bottom") or a 2 element
numeric vector giving the relative x,y position in [0,1]}

\item{legend_bg_alpha}{the alpha (transparency) level of the white
legend background in [0,1]}

\item{transparent_bg}{switches between transparent (TRUE) and white
(FALSE) plot background}
}
\description{
Adds a minimal theme to your \code{ggplot} object. Allows you to
specify font size and family, colour of gridlines, which gridlines
to draw, legend position and transparency level of legend
background. Allows you to switch between white and transparent
background.
}
\note{
Forked from \pkg{ggplot2} \code{theme-defaults.r} by Hadley
  Wickham.
}
\examples{
library(ggplot2)
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  ggtheme_min()

}
\author{
Jonas Schoeley, Hadley Wickham
}
\seealso{
\code{\link[ggplot2]{theme}}
}

