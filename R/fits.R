# References
# https://robjhyndman.com/talks/Google-Oct2015-part1.pdf
# https://robjhyndman.com/hyndsight/nnetar-prediction-intervals/
# https://otexts.org/fpp2/

# Installig, updating and opening packages
# packs <- c('fpp2')
# new.packages <- packs[!(packs %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(packs, dep=TRUE)
# update.packages(checkBuilt = TRUE, ask = FALSE)

#' Fits the best model from classes ARIMA, ETS, TBATS and NNETAR.
#'
#' @param x A vector or ts object.
#' @param train The (initial) percentage of the time series to be used to train the models. Must be \code{0 < train < 1}.
#' @param steps Number of steps to forecast. If \code{NULL}, uses the number of points not used in training (testing points). Can't be less than the number of testing points.
#' @param max.points Limits the maximum number of points to be used in modeling. Uses the last \code{max.points} points of the series.
#' @param graph Logical. Should the graphics be displayed?
#' @param PI Prediction Interval used in nnar models. Must take long time processing.
# #' @param lim Limit to maxima/minima observed.
#' @return The predicted time series using the model that minimizes the forecasting mean square error.
#' @import fpp2
#' @references
#' Hyndman, R.J., & Athanasopoulos, G. (2018) Forecasting: principles and practice, 2nd edition, OTexts: Melbourne, Australia. https://otexts.com/fpp2.
#'
#' Zabala, F. J. and Silveira, F. F. (2019). Decades of Jurimetrics. arXiv.org...
#' @examples
#' library(jurimetrics)
#' library(fpp2)
#'
#' fits(livestock)
#' fits(livestock, graf = F)
#' fits(h02, .9)
#' fits(gas)
#'
#' data(count_year_month)
#' y <- ts(count_year_month$count, start = c(2000,1), frequency = 12)
#' fits(y, train = 0.8, steps = 24)
#' @export
fits <- function(x, train = 0.8, steps = NULL,
                 max.points = 500, graf = T, PI = F){
  ini <- Sys.time()

  # libraries
  # suppressMessages(library(fpp2))

  # filtering max.points
  n0 <- length(x)
  if(n0 > max.points) {x <- x[(n0-max.points+1):n0]}

  # train-test
  n <- length(x)
  i <- ceiling(train*n)
  xTrain <- 1:i
  xTest <- (i+1):n

  # models
  fit.aa <- auto.arima(x[xTrain])
  fit.ets <- ets(x[xTrain])
  fit.tb <- tbats(x[xTrain])
  set.seed(1); fit.nn <- nnetar(x[xTrain])

  # forecast
  if(is.null(steps)) {steps <- length(xTest)}
  fcast.aa <- forecast(fit.aa, h=steps)
  fcast.ets <- forecast(fit.ets, h=steps)
  fcast.tb <- forecast(fit.tb, h=steps)
  fcast.nn <- forecast(fit.nn, h=steps, PI = PI)

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
  # print(mse.pred)

  # fitting best model based on mse.pred
  bestModel <- which.min(mse.pred)

  if(bestModel == 1){
    fit <- auto.arima(x)
    fcast <- forecast(fit, h=steps)
  }
  else if(bestModel == 2){
    fit <- ets(x)
    fcast <- forecast(fit, h=steps)
  }
  else if(bestModel == 3){
    fit <- tbats(x)
    fcast <- forecast(fit, h=steps)
  }
  else if(bestModel == 4){
    set.seed(1); fit <- nnetar(x)
    fcast <- forecast(fit, h=steps, PI = PI)
  }

  # train/test plots (if graf = T)
  if(graf){
    par(mfrow=c(2,2))
    plot(fcast.aa); points(c(rep(NA,i), x[xTest]))
    plot(fcast.ets); points(c(rep(NA,i), x[xTest]))
    plot(fcast.tb); points(c(rep(NA,i), x[xTest]))
    plot(fcast.nn); points(c(rep(NA,i), x[xTest]))

    # best model plot
    par(mfrow=c(1,1))
    plot(fcast)
  }

  # mean squared error (best fit residuals)
  # mse.fit.best <- mean(residuals(fit)^2)

  # presenting results
  return(list(fcast = fcast,
              runtime = print(Sys.time()-ini),
              mse.pred = mse.pred))
}
