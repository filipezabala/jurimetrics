# restart R
# .rs.restartR()

# pacotes
library(tidyverse)  # varios
library(readxl)     # read_xlsx
library(lubridate)  # wday
library(devtools)
library(qcc)        # pareto.chart

# functions
# source('~/Dropbox/Jurimetria/codigos/git/jurimetrics/fits.R')

# set strings as factors to false
options(stringsAsFactors = FALSE)

# diretorio
setwd('~/Desktop/wbs/arquivosLiteExcel/')            #  Mac
# setwd('~/Área de Trabalho/wbs/arquivosExcel')   # Linux

# lendo bancos de dados
ini <- Sys.time()
data <- vector('list')
for(i in 2000:2017){
  df <- read_xlsx(paste0('tjrs',i,'.xlsx'))
  temp <- substr(df$`Data de Julgamento`,3,12)
  data[[paste0(i)]] <- as_tibble(as.Date(temp,  format = '%d/%m/%Y'))
  names(data[[paste0(i)]]) <- 'judgmentDate'
  print(paste0(i))
}
gc()
rm(df)
Sys.time() - ini # Time difference of 7.998097 mins


# criando colunas de semana e mês
ini <- Sys.time()
data <- lapply(data, mutate, weekDay = lubridate::wday(judgmentDate, label = T))
data <- lapply(data, mutate, yearMonth = as.Date(paste0(substr(judgmentDate,1,7),'-01'),
                                                  format = '%Y-%m-%d'))
# empilhando
data <- do.call(rbind, data)
Sys.time() - ini # Time difference of 27.99515 secs

# agrupando e contando o número de processos por dia
tjrs_day <- data %>%
  group_by(judgmentDate) %>%
  summarize(count = n())
use_data(tjrs_day, overwrite = T)
write_csv(tjrs_day, '~/Dropbox/Jurimetria/codigos/git/jurimetrics/data/tjrs_day.csv')

# agrupando e contando o número de processos por dia a semana
tjrs_week_day <- data %>%
  group_by(weekDay) %>%
  summarize(count = n())
use_data(tjrs_week_day, overwrite = T)
write_csv(tjrs_week_day, '~/Dropbox/Jurimetria/codigos/git/jurimetrics/data/tjrs_week_day.csv')
pareto.chart(table(data$weekDay))

# agrupando e contando o número de processos por mês
tjrs_year_month <- data %>%
  group_by(yearMonth) %>%
  summarize(count = n())
use_data(tjrs_year_month, overwrite = T)
write_csv(tjrs_year_month, '~/Dropbox/Jurimetria/codigos/git/jurimetrics/data/tjrs_year_month.csv')
ggplot(tjrs_year_month, aes(yearMonth, count)) +
  geom_line()


# exportando dia 2019-10-18
setwd('~/Dropbox/Jurimetria/codigos/git/jurimetrics/data/')
tjrs_20191018 <- read_xlsx('tjrs_20191018.xlsx')
use_data(tjrs_20191018, overwrite = T)

# data("tjrs_day")
# tjrs_day

# # projetando
# ini <- Sys.time()
# y <- ts(tjrs_year_month$count, start = c(2000,1), frequency = 12)
# fits(y, steps = 48, lim = F, graf = T, PI = F)
# Sys.time()-ini   # Time difference of 6.028008 mins, PI = T

