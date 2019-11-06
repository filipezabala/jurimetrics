#' Fits the best model from classes ARIMA, ETS, TBATS and NNETAR.
#'
#' @param x A vector or ts object.
#' @param train The (initial) percentage of the time series to be used to train the models. Must be \code{0 < train < 1}.
#' @param steps Number of steps to forecast. If \code{NULL}, uses the number of points not used in training (testing points). Can't be less than the number of testing points.
#' @param max.points Limits the maximum number of points to be used in modeling. Uses the first \code{max.points} points of the series.
#' @param show.main.graph Logical. Should the main graphic (with the final model) be displayed?
#' @param show.sec.graph Logical. Should the secondary graphics (with the training models) be displayed?
#' @param show.value Logical. Should the values be displayed?
#' @param PI Prediction Interval used in nnar models. May take long time processing.
#' @param theme.doj Logical. Should the theme of Decades Of Jurimetrics be used?
#' @return \code{$fcast} Predicted time series using the model that minimizes the forecasting mean square error.
#' @return \code{$mse.pred} Mean squared error of prediction. Used to decide the best model.
#' @return \code{$runtime} Running time.
#' @import fpp2
#' @references
#' Hyndman, R.J., & Athanasopoulos, G. (2018) Forecasting: principles and practice, 2nd edition, OTexts: Melbourne, Australia. \href{https://otexts.com/fpp2}{otexts.com/fpp2}.
#'
#' \href{https://robjhyndman.com/hyndsight/nnetar-prediction-intervals/}{https://robjhyndman.com/hyndsight/nnetar-prediction-intervals/}
#'
#' \href{https://robjhyndman.com/talks/Google-Oct2015-part1.pdf}{https://robjhyndman.com/talks/Google-Oct2015-part1.pdf}
#'
#' Zabala, F. J. and Silveira, F. F. (2019). Decades of Jurimetrics.
#' @examples
#' library(jurimetrics)
#'
#' fits(livestock)
#' fits(livestock, theme.doj = T)
#' fits(livestock, show.main.graph = F, show.sec.graph = T, show.value = F)
#'
#' fits(h02, .9)
#'
#' fits(gas)
#'
#' data('tjmg_year')
#' y1 <- ts(tjmg_year$count, start = c(2000,1), frequency = 1)
#' fits(y1)
#'
#' data(tjrs_year_month)
#' y2 <- ts(tjrs_year_month$count, start = c(2000,1), frequency = 12)
#' fits(y2, train = 0.8, steps = 24)
#' @export
fits <- function(x, train = 0.8,
  steps = NULL,
  max.points = 500,
  show.main.graph = T,
  show.sec.graph = F,
  show.value = T,
  PI = F,
  theme.doj = F){

  ini <- Sys.time()

  # filtering max.points
  n0 <- length(x)
  if(n0 > max.points) {x <- x[(n0-max.points+1):n0]}

  # train-test
  n <- length(x)
  i <- ceiling(train*n)
  xTrain <- 1:i
  xTest <- (i+1):n

  # models
  fit.aa <- forecast::auto.arima(x[xTrain])
  fit.ets <- forecast::ets(x[xTrain])
  fit.tb <- forecast::tbats(x[xTrain])
  set.seed(1); fit.nn <- forecast::nnetar(x[xTrain])

  # forecast
  if(is.null(steps)) {steps <- length(xTest)}
  fcast.aa <- forecast::forecast(fit.aa, h=steps)
  fcast.ets <- forecast::forecast(fit.ets, h=steps)
  fcast.tb <- forecast::forecast(fit.tb, h=steps)
  fcast.nn <- forecast::forecast(fit.nn, h=steps, PI = PI)

  # akaike information criteria
  aic <- cbind(aa = c(aic=fit.aa$aic, aicc=fit.aa$aicc, bic=fit.aa$bic),
               ets = c(aic=fit.ets$aic, aicc=fit.ets$aicc, bic=fit.ets$bic),
               tb = c(aic=fit.tb$AIC, aicc=NA, bic=NA))#,
               # nn = c(aic=NA, aicc=NA, bic=NA))

  # mean squared error (residuals)
  mse.fit <- data.frame(mse.fit.aa = mean(residuals(fit.aa)^2),
                        mse.fit.ets = mean(residuals(fit.ets)^2),
                        mse.fit.tb = mean(residuals(fit.tb)^2))#,
                        # mse.fit.nn = mean(residuals(fit.nn)^2))

  # mean squared error (forecast)
  if(train != 1){
    mse.pred <- data.frame(mse.pred.aa = mean((fcast.aa$mean[1:length(xTest)]-x[xTest])^2),
                           mse.pred.ets = mean((fcast.ets$mean[1:length(xTest)]-x[xTest])^2),
                           mse.pred.tb = mean((fcast.tb$mean[1:length(xTest)]-x[xTest])^2),
                           mse.pred.nn = mean((fcast.nn$mean[1:length(xTest)]-x[xTest])^2))
  }

  # fitting best model based on mse.pred
  bestModel <- which.min(mse.pred)

  if(bestModel == 1){
    fit <- forecast::auto.arima(x)
    fcast <- forecast::forecast(fit, h = steps)
  }
  else if(bestModel == 2){
    fit <- forecast::ets(x)
    fcast <- forecast::forecast(fit, h = steps)
  }
  else if(bestModel == 3){
    fit <- forecast::tbats(x)
    fcast <- forecast::forecast(fit, h = steps)
  }
  else if(bestModel == 4){
    set.seed(1); fit <- forecast::nnetar(x)
    fcast <- forecast::forecast(fit, h = steps, PI = PI)
  }

  # train/test plots
  if(show.sec.graph){
    par(mfrow=c(2,2))
    plot(fcast.aa); points(c(rep(NA,i), x[xTest]))
    plot(fcast.ets); points(c(rep(NA,i), x[xTest]))
    plot(fcast.tb); points(c(rep(NA,i), x[xTest]))
    plot(fcast.nn); points(c(rep(NA,i), x[xTest]))
  }

  # main plot (best model)
  if(show.main.graph){

    if(!theme.doj){
      print(ggplot2::autoplot(fcast))
    }

    if(theme.doj){
      print(ggplot2::autoplot(fcast) +
          jurimetrics::theme_doj())
    }
  }

  # mean squared error (best fit residuals)
  # mse.fit.best <- mean(residuals(fit)^2)

  # presenting results
  if(show.value){
    return(list(fcast = fcast,
      mse.pred = mse.pred,
      runtime = Sys.time()-ini))
  }
}
