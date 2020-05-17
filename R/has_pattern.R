#' Indicates if a string has a given pattern.
#'
#' @return Logical. `TRUE` indicates the string has a given pattern, `FALSE` otherwise.
#' @import stringi
#' @examples
#' library(stringi)
#'
#' text <- dplyr::as_tibble(c('A designer knows that he has achieved perfection',
#' 'not when there is nothing left to add',
#' 'but when there is nothing left to take away.'))
#' has_pattern(text, 'nothing')
#' apply(text, 1, has_pattern, 'nothing')
#'
#' @export
has_pattern <- function(x, pattern){
  has.pattern <- sum(suppressWarnings(stringi::stri_detect_fixed(x,pattern))) != 0
  return(has.pattern)
}
