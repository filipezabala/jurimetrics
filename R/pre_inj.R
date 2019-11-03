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
#' Hyndman, R.J., & Athanasopoulos, G. (2018) Forecasting: principles and practice, 2nd edition, OTexts: Melbourne, Australia. https://otexts.com/fpp2.
#'
#' https://robjhyndman.com/hyndsight/nnetar-prediction-intervals/
#'
#' https://robjhyndman.com/talks/Google-Oct2015-part1.pdf
#'
#' Zabala, F. J. and Silveira, F. F. (2019). Decades of Jurimetrics. arXiv.org...
#' @examples
#' library(jurimetrics)
#'
#' fits(livestock)
#' fits(livestock, theme.doj=T)
#' fits(livestock, show.main.graph = F, show.sec.graph = T, show.value = F)
#' fits(h02, .9)
#' fits(gas)
#'
#' data(count_year_month)
#' y <- ts(count_year_month$count, start = c(2000,1), frequency = 12)
#' fits(y, train = 0.8, steps = 24)
#' @export
pre_inj <- function(average.consult, prob.hire, average.ticket){
  ev <- round(average.consult * prob.hire * average.ticket/22, 2)
  text <- paste0('The estimated loss amount per business day is $', ev, '.')
  return(list(expected.value = ev, text = text))
}

# pre_inj(1000,0.1,3450)
