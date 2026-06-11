#' Custom ggplot2 Theme for the Optics Package
#'
#' Provides a standardized, professional theme for all plots created within
#' the `Optics` package, inspired by the project's reporting standards.
#'
#' @param base_size The base font size for the plot text. Defaults to 14.
#' @return A `ggplot2` theme object.
#' @export
#' @import ggplot2
#' @examples
#' \dontrun{
#'   library(ggplot2)
#'   df <- data.frame(x = 1:10, y = 1:10)
#'   ggplot(df, aes(x, y)) +
#'     geom_point() +
#'     theme_optics()
#' }
theme_optics <- function(base_size = 14) {
  ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = ggplot2::rel(1.2), hjust = 0.5, face = "bold", color = "#5A5F63"),
      plot.subtitle = ggplot2::element_text(size = ggplot2::rel(1.0), hjust = 0.5, face = "italic", color = "#5A5F63"),
      panel.grid.major = ggplot2::element_line(color = "#D9DADB", linewidth = 0.2),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(color = "#5A5F63", linewidth = 0.5),
      axis.title = ggplot2::element_text(size = ggplot2::rel(1.1), face = "bold", color = "#5A5F63"),
      axis.text = ggplot2::element_text(color = "#5A5F63"),
      legend.title = ggplot2::element_text(size = ggplot2::rel(1.0), face = "bold", color = "#5A5F63"),
      legend.text = ggplot2::element_text(color = "#5A5F63"),
      strip.background = ggplot2::element_rect(fill = "grey80", color = "grey80"),
      strip.text = ggplot2::element_text(color = "white", face = "bold")
    )
}