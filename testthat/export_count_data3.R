# libs
library(lubridate)
library(jurimetrics)
library(usethis)

# lendo contagens 2000-2017
tjrs_00_17 <- read.csv('~/Dropbox/Jurimetria/bases/cota_inferior_2000-2017.csv')
tjrs_16_23 <- read.csv('~/Dropbox/Jurimetria/bases/cota_inferior_2016-2023.csv')

# as.Date(y$yearMonth)
tjrs_00_17$yearMonth <- as.Date(tjrs_00_17$yearMonth, format = '%Y-%m-%d')
tjrs_16_23$yearMonth <- as.Date(tjrs_16_23$yearMonth, format = '%Y-%m-%d')

# diferenças para evitar séries fracamente regulares
diff(tjrs_00_17$yearMonth)
year(tjrs_00_17$yearMonth)

diff(tjrs_16_23$yearMonth)
year(tjrs_16_23$yearMonth)

# filtrando meses incompletos
tjrs_16_23 <- tjrs_16_23[-(1:5),]
tjrs_16_23 <- tjrs_16_23[year(tjrs_16_23$yearMonth) > 2015,]

# filtrando 2016-2017
ts_00_17 <- tjrs_00_17[year(tjrs_00_17$yearMonth) >= 2016,]
ts_16_23 <- tjrs_16_23[year(tjrs_16_23$yearMonth) <= 2017,]

# comparando
plot(ts_00_17$count, ts_16_23$count)
fit <- lm(ts_16_23$count ~ ts_00_17$count - 1)
summary(fit)
abline(0, fit$coefficients[1], col = 'red')
abline(0, 1, col = 'blue')

# descontando 2000-2017
tjrs_00_17$count_adjusted <- floor(tjrs_00_17$count * fit$coefficients[1])

# arrumando 2000-2017
tjrs_00_17$count <- ts(tjrs_00_17$count,
                     start = c(2000,1), frequency = 12)
tjrs_00_17$count_adjusted <- ts(tjrs_00_17$count_adjusted,
                              start = c(2000,1), frequency = 12)
str(tjrs_00_17)
tjrs_00_17$count
tjrs_00_17$count_adjusted


# inflando 2016-2023
tjrs_16_23$count_adjusted <- floor(tjrs_16_23$count / fit$coefficients[1])

# arrumando  2016-2023
tjrs_16_23$count <- ts(tjrs_16_23$count,
                     start = c(2016,1), frequency = 12)
tjrs_16_23$count_adjusted <- ts(tjrs_16_23$count_adjusted,
                              start = c(2016,1), frequency = 12)
str(tjrs_16_23)
tjrs_16_23$count
tjrs_16_23$count_adjusted

# gerando dados .rda
setwd('~/Dropbox/Jurimetria/codigos/git/jurimetrics/data/')
usethis::use_data(tjrs_00_17, overwrite = TRUE)
usethis::use_data(tjrs_16_23, overwrite = TRUE)

data(tjrs_00_17)
data(tjrs_16_23)

# # xts
# ts_00_17 <- zooreg(tjrs_00_17, start = c(2015, 1), frequency = 12)
# head(zm2) # zooreg()
# tjrs_00_17 <- as.xts(tjrs_00_17)
#
# # gráficos
# plot(cota_inferior_xts)
# ts_plot(cota_inferior_xts, Xgrid = TRUE, Ygrid = TRUE)
#
# # modelos
# tic()
# fits(cota_inferior_xts, show.sec.graph = TRUE, PI = TRUE)
# toc() # 52.113 sec elapsed
#
#
#
# # View(jurimetrics::count_year_month)
# # contaAnoMes
#
# # 2022 com volume muito baixo
# plot(contaAnoMes)
# plot(contaAnoMes[year(index(contaAnoMes)) >= 2021 & year(index(contaAnoMes)) <= 2023,])
# plot(window(contaAnoMes, start = '2021-07-01', end = '2023-06-30'))
#
# stats::monthplot(contaAnoMes)
# forecast::ggseasonplot(contaAnoMes_ts, col=rainbow(12), year.labels = TRUE)
# class(fpp2::h02)
