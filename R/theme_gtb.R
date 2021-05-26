#' ggplot2 theme for the online WHO Global TB Report
#'
#' @param base_size base font size, given in pts.
#' @param base_family base font family
#' @author: Hazim Timimi
#' @import ggplot2
#'
#' @export
#'
theme_gtb <- function(base_size = 14,
                            base_family = "") {
  gray <- "#BCBCBC"
  charcoal <- "#222222"
  ggplot2::theme(
    #Text format:
    plot.margin = margin(30, 5, 30, 5),
    #Legend format
    legend.position = "bottom",
    legend.text.align = 0,
    legend.background = element_blank(),
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(
      family = base_family,
      size = 14,
      color = charcoal
    ),
    #Axis format
    axis.title = element_text(
      family = base_family,
      size = 14,
      color = charcoal
    ),
    axis.text = element_text(
      family = base_family,
      size = 8,
      color = charcoal
    ),
    axis.text.x = ggplot2::element_text(margin = ggplot2::margin(5, b = 10)),
    axis.ticks.x = element_line(colour = gray),
    axis.ticks.y = element_blank(),
    axis.line.x = ggplot2::element_line(color = gray, size = 0.25),
    axis.line.y = element_blank(),
    #Grid lines
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(),
    # Have plain background in headings of facet sub-plots
    strip.background = element_rect(fill = "white", colour = NA),
    strip.text = element_text(
      family = base_family,
      size = 14,
      face = "bold",
      color = charcoal
    ),
    #Blank background
    panel.background = ggplot2::element_blank(),
  )
}

