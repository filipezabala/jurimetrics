#' Defines the Decades of Jurimetrics theme
#'
#' @return Decades of Jurimetrics theme, based on ggplot2::theme_dark.
#' @import ggplot2
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
    theme(
      # legend.position = 'bottom',
      panel.background = element_rect(fill = 'grey50', colour = NA),
      plot.background = element_rect(fill = 'transparent', colour = NA),
      strip.background = element_rect(fill = 'transparent', colour = NA),
      legend.background = element_rect(fill = 'transparent'),
      legend.box.background = element_rect(fill = 'transparent'),
      plot.title = element_text(colour='grey70'),
      legend.title = element_text(colour='grey70'),
      legend.text = element_text(colour='grey70'),
      axis.text.x = element_text(colour='grey70'),
      axis.text.y = element_text(colour='grey70'),
      panel.grid = element_line(colour = 'grey42'),
      panel.grid.major = element_line(size = rel(0.5)),
      panel.grid.minor = element_line(size = rel(0.5))
      )
  )
}
