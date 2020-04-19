# retira colunas
tira_col <- function(x, tira = NULL){
  if(is.null(tira)){ tira <- c('X__1') }
  x <- select(x,-tira)
  return(x)
}
