#' Defines the Decades of Jurimetrics theme
#'
#' @return Decades of Jurimetrics theme, based on ggplot2::theme_dark.
#' @references
#' Wickham, H. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York, 2016.
#'
#' Zabala, F. J. and Silveira, F. F. (2019). Decades of Jurimetrics. arXiv.org...
#' @examples
#' library(ggplot2)
#'
#' theme_doj()
#' body(theme_doj)
#' @export
theme_doj <- function(){
  return(
    ggplot2::theme(
      # legend.position = 'bottom',
      panel.background = ggplot2::element_rect(fill = 'grey50', colour = NA),
      plot.background = ggplot2::element_rect(fill = 'transparent', color = NA),
      strip.background = ggplot2::element_rect(fill = 'transparent', colour = NA),
      legend.background = ggplot2::element_rect(fill = 'transparent'),
      legend.box.background = ggplot2::element_rect(fill = 'transparent'),
      plot.title = ggplot2::element_text(color='grey42'),
      legend.title = ggplot2::element_text(color='grey42'),
      legend.text = ggplot2::element_text(color='grey42'),
      panel.grid = ggplot2::element_line(colour = 'grey42'),
      panel.grid.major = ggplot2::element_line(size = ggplot2::rel(0.5)),
      panel.grid.minor = ggplot2::element_line(size = ggplot2::rel(0.5))
      )
  )
}
