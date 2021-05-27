#' ggplot2 theme for the WHO Global TB Report
#'
#' @param base_size base font size, given in pts.
#' @param base_family base font family
#' @param axis_text_size font size for axis text in pts.
#' @author: Hazim Timimi
#' @examples
#' p <- ggplot(mtcars, aes(wt, mpg)) +
#'      geom_point() + facet_wrap(~ cyl)
#' print(p)
#' p2 <- p + theme_gtb()
#' print(p2)
#'
#' @import ggplot2
#' @export
#'
theme_gtb <-
  function(base_size = 14,
           base_family = "",
           axis_text_size = 8) {
    faint_gray <- "#CCCCCC"
    gray <- "#BCBCBC"
    charcoal <- "#222222"

    ggplot2::theme(
      # Text format:
      plot.margin = ggplot2::margin(30, 5, 30, 5),

      # Legend format
      legend.position = "bottom",
      legend.text.align = 0,
      legend.background = ggplot2::element_blank(),
      legend.title = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(
        family = base_family,
        size = base_size,
        colour = charcoal
      ),

      # Axis format
      axis.title = ggplot2::element_text(
        family = base_family,
        size = base_size,
        colour = charcoal
      ),

      axis.text = ggplot2::element_text(
        family = base_family,
        size = axis_text_size,
        colour = charcoal
      ),
      axis.text.x = ggplot2::element_text(margin = ggplot2::margin(5, b = 10)),
      axis.ticks.x = ggplot2::element_line(colour = gray),
      axis.ticks.y = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_line(colour = gray, size = 0.25),
      axis.line.y = ggplot2::element_blank(),

      # Grid lines
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = faint_gray, size = 0.1),
      panel.grid.major.x = ggplot2::element_blank(),

      # Have plain background in headings of facet sub-plots
      strip.background = ggplot2::element_rect(fill = "white", colour = NA),
      strip.text = ggplot2::element_text(
        family = base_family,
        size = base_size,
        face = "bold",
        colour = charcoal
      ),

      # Blank background
      panel.background = ggplot2::element_blank(),
    )
  }
