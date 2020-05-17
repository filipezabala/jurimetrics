has_pattern <- function(x, pattern){
  has.pattern <- sum(suppressWarnings(stringi::stri_detect_fixed(x,pattern))) != 0
  return(has.pattern)
}
