# Fit VAR (Vector AutoRegresssive)

# Pacotes e dados
library("vars")
data("Canada")
summary(Canada)
?Canada

# Descritivas
plot(Canada, nc = 2, xlab = "")

# Teste aumentado de Dickey-Fuller, testa estacionariedade, bom é rejeitar
adf1 <- summary(ur.df(Canada[, "prod"], type = "trend", lags = 2))
adf1
adf2 <- summary(ur.df(diff(Canada[, "prod"]), type = "drift", lags = 1))
adf2
