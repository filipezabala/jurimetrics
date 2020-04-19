# filtra tipo_prcesso==73
filtra_tp <- function(x){
  x <- filter(x, `Tipo de Processo` == 'Revisão Criminal')
}
