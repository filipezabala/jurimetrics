#' Expected value of loss amount per business day.
#'
#' @param average.consult Average number of mensal credit bureau consultations.
#' @param prob.hire (Historical) probability of hiring. Can be considered as the number of converted customers divided by the number of  credit bureau consultations in a period of time.
#' @param average.ticket The average ticket of regular customers.
#' @return \code{$expected.value} Expected value of loss amount per business day of negative credit bureau attribution.
#' @return \code{$text} An automatic text that details the \code{$expected.value}.
#' @references
#' Feller, W. (1957) \href{https://archive.org/details/AnIntroductionToProbabilityTheoryAndItsApplicationsVolume1}{An Introduction to Probability Theory and its Applications}.
#' @examples
#' library(jurimetrics)
#' exp_loss(1000, 0.1, 3450)
#' @export
exp_loss <- function(average.consult, prob.hire, average.ticket, language = 'en'){
  if(language == 'pt'){
    ev <- round(average.consult * prob.hire * average.ticket/22, 2)
    text <- paste0('A perda esperada por dia útil é de $', ev, '.')
    return(list(perda.esperada = ev, texto = text))
  }
  else{
    ev <- round(average.consult * prob.hire * average.ticket/22, 2)
    text <- paste0('The estimated loss amount per business day is $', ev, '.')
    return(list(expected.loss = ev, text = text))
  }


}
