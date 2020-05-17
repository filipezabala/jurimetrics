#' Pattern of 'dano moral'
#'
#' @return A char vector listing the 'dano moral' pattern.
#' @examples
#' pattern_danomoral()
#' @export
pattern_danomoral <- function(){
  pat <- c(
    'dano moral',
    'danos morais',
    'dano material e moral',
    'dano, material e moral',
    'dano, moral e material',
    'danos materiais e morais',
    'danos, materiais e morais',
    'abalo moral')
  return(pat)
}
