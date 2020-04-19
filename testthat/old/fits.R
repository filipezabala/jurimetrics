###################################################
###  Attribution 4.0 International (CC BY 4.0)  ###
### https://creativecommons.org/licenses/by/4.0 ###
###           filipe.zabala@gmail.com           ###
###              www.aclassica.com              ###
###################################################

# References
# https://robjhyndman.com/talks/Google-Oct2015-part1.pdf
# https://robjhyndman.com/hyndsight/nnetar-prediction-intervals/
# https://otexts.org/fpp2/

# Installig, updating and opening packages
packs <- c('fpp2')
new.packages <- packs[!(packs %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(packs, dep=TRUE)
update.packages(checkBuilt = TRUE, ask = FALSE)

# Function
fits <- function(x, train = 0.9, steps = NULL, 
                 max.points = 500, lim = T, graf = F, PI = F){
  ini <- Sys.time()
  
  # libraries
  suppressMessages(library(fpp2))
  
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

  # limiting minima and maxima (if lim = T)
  # if(lim){
  #   fcast.aa <- limit(x, fcast.aa)
  #   fcast.ets <- limit(x, fcast.ets)
  #   fcast.tb <- limit(x, fcast.tb)
  #   # fcast.nn <- limit(x, fcast.nn)
  #   fcast <- limit(x, fcast)
  # }
  
  # tein/test plots (if graf = T)
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

# fits(livestock,.8,graf=T)
# fits(h02,.8,graf=T)
# fits(gas,.9,graf=T)
