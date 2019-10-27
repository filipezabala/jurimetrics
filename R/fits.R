#' Fits the best model from classes ARIMA, ETS, TBATS and NNETAR.
#'
#' @param x A vector or ts object.
#' @param train The (initial) percentage of the time series to be used to train the models. Must be \code{0 < train < 1}.
#' @param steps Number of steps to forecast. If \code{NULL}, uses the number of points not used in training (testing points). Can't be less than the number of testing points.
#' @param max.points Limits the maximum number of points to be used in modeling. Uses the first \code{max.points} points of the series.
#' @param show.main.graph Logical. Should the main graphic (with the final model) be displayed?
#' @param show.sec.graph Logical. Should the secondary graphics (with the training models) be displayed?
#' @param show.value Logical. Should the values be displayed?
#' @param PI Prediction Interval used in nnar models. Must take long time processing.
#' @param theme.doj use DOJ theme for ggplot2 plots
# #' @param lim Limit to maxima/minima observed.
#' @return \code{$fcast} predicted time series using the model that minimizes the forecasting mean square error.
#' \code{$runtime} running time.
#' \code{mse.pred} mean squared error of prediction. Used to decide the best model.
#' @references
#' Hyndman, R.J., & Athanasopoulos, G. (2018) Forecasting: principles and practice, 2nd edition, OTexts: Melbourne, Australia. https://otexts.com/fpp2.
#'
#' https://robjhyndman.com/hyndsight/nnetar-prediction-intervals/
#'
#' https://robjhyndman.com/talks/Google-Oct2015-part1.pdf
#'
#' Zabala, F. J. and Silveira, F. F. (2019). Decades of Jurimetrics. arXiv.org...
#' @examples
#'
#' fits(fpp2::livestock)
#' fits(fpp2::livestock, theme.doj = TRUE)
#' fits(fpp2::livestock, show.main.graph = FALSE, show.sec.graph = TRUE, show.value = FALSE)
#' fits(fpp2::h02, .9)
#' fits(forecast::gas)
#'
#' y <- ts(count_year_month$count, start = c(2000,1), frequency = 12)
#' fits(y, train = 0.8, steps = 24)
#'
#' @export
fits <- function(x, train = 0.8,
  steps = NULL,
  max.points = 500,
  show.main.graph = TRUE,
  show.sec.graph = FALSE,
  show.value = TRUE,
  PI = FALSE,
  theme.doj = FALSE){

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
  mse.fit <- data.frame(mse.fit.aa = mean(stats::residuals(fit.aa)^2),
                        mse.fit.ets = mean(stats::residuals(fit.ets)^2),
                        mse.fit.tb = mean(stats::residuals(fit.tb)^2))#,
                        # mse.fit.nn = mean(residuals(fit.nn)^2))

  # mean squared error (forecast)
  if(train != 1){
    mse.pred <- data.frame(mse.pred.aa = mean((fcast.aa$mean[1:length(xTest)]-x[xTest])^2, na.rm = TRUE),
                           mse.pred.ets = mean((fcast.ets$mean[1:length(xTest)]-x[xTest])^2, na.rm = TRUE),
                           mse.pred.tb = mean((fcast.tb$mean[1:length(xTest)]-x[xTest])^2, na.rm = TRUE),
                           mse.pred.nn = mean((fcast.nn$mean[1:length(xTest)]-x[xTest])^2, na.rm = TRUE))
  }
  # print(mse.pred)

  # fitting best model based on mse.pred
  bestModel <- which.min(mse.pred)

  if(bestModel == 1){
    fit <- forecast::auto.arima(x)
    fcast <- forecast::forecast(fit, h=steps)
  }
  else if(bestModel == 2){
    fit <- forecast::ets(x)
    fcast <- forecast::forecast(fit, h=steps)
  }
  else if(bestModel == 3){
    fit <- forecast::tbats(x)
    fcast <- forecast::forecast(fit, h=steps)
  }
  else if(bestModel == 4){
    set.seed(1); fit <- forecast::nnetar(x)
    fcast <- forecast::forecast(fit, h=steps, PI = PI)
  }

  # train/test plots
  if(show.sec.graph){
    graphics::par(mfrow=c(2,2))
    graphics::plot(fcast.aa); graphics::points(c(rep(NA,i), x[xTest]))
    graphics::plot(fcast.ets); graphics::points(c(rep(NA,i), x[xTest]))
    graphics::plot(fcast.tb); graphics::points(c(rep(NA,i), x[xTest]))
    graphics::plot(fcast.nn); graphics::points(c(rep(NA,i), x[xTest]))
  }

  # main plot (best model)
  if(show.main.graph) {

    if(!theme.doj){
      print(forecast::autoplot(fcast))
    }

    if(theme.doj){
      print(forecast::autoplot(fcast) + theme_doj())
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
