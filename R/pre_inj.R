#' Expected value of loss amount per business day of mistaken credit bureau attribution.
#'
#' @param average.consult Average number of mensal credit bureau consultations.
#' @param prob.hire (Historical) probability of hiring. Can be considered as the number of converted customers divided by the number of  credit bureau consultations in a period of time.
#' @param average.ticket The average ticket of regular customers.
#' @return \code{$fcast} predicted time series using the model that minimizes the forecasting mean square error.
#' \code{$runtime} running time.
#' \code{mse.pred} mean squared error of prediction. Used to decide the best model.
#' @import fpp2
#' @references
#' Feller, W. (1957) An Introduction to Probability Theory and its Applications. https://archive.org/details/AnIntroductionToProbabilityTheoryAndItsApplicationsVolume1
#' @examples
#' library(jurimetrics)
#'
#' pre_inj(1000, 0.1, 3450)
#' @export
pre_inj <- function(average.consult, prob.hire, average.ticket){
  ev <- round(average.consult * prob.hire * average.ticket/22, 2)
  text <- paste0('The estimated loss amount per business day is $', ev, '.')
  return(list(expected.value = ev, text = text))
}
