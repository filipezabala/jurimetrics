# restart R
# .rs.restartR()

# pacotes
library(tidyverse)  # varios
library(readxl)     # read_xlsx
library(lubridate)  # wday
library(qcc)        # pareto.chart
library(voice)
library(jurimetrics)
library(devtools)


# functions
# source('~/Dropbox/Jurimetria/codigos/git/jurimetrics/fits.R')

# set strings as factors to false
options(stringsAsFactors = FALSE)

# diretorio
# setwd('~/Desktop/wbs/arquivosLiteExcel/')      #  Mac
# setwd('~/Documentos/tjrs/arquivosLiteExcel/')  # Linux
setwd('~/Documentos/tjrs/csvFromXlsx/')  # Linux

# lendo bancos de dados 2000:2017
ini <- Sys.time()
data <- tibble()
for(i in 2015:2020){
  df <- read_csv(paste0('tjrs_',i,'.csv'))
  temp <- df %>%
    select(data_julgamento, tipo_processo, assunto_cnj) %>%
    arrange(data_julgamento)
  data <- bind_rows(data,temp)
  gc()
  print(i)
}
memory()
Sys.time() - ini # Time difference of 4.599254 mins

# criando colunas de semana e mês
ini <- Sys.time()
data <- data %>%
  mutate(weekDay = lubridate::wday(data_julgamento, label = T))
data <- data %>%
  mutate(yearMonth = as.Date(paste0(substr(data_julgamento,1,7),'-01'),
                             format = '%Y-%m-%d'))

# new wd
setwd('~/Dropbox/Jurimetria/codigos/git/jurimetrics/data/')

# agrupando e contando o número de processos por dia
count_day2 <- data %>%
  group_by(data_julgamento) %>%
  summarize(count = n())
use_data(count_day2, overwrite = T)
write_csv(count_day2, '~/Dropbox/Jurimetria/codigos/git/jurimetrics/data_extra/count_day2.csv')

# agrupando e contando o número de processos por dia por tipo de processo
count_day_type2 <- data %>%
  group_by(data_julgamento, tipo_processo) %>%
  summarize(count = n())
use_data(count_day_type2, overwrite = T)
write_csv(count_day_type2, '~/Dropbox/Jurimetria/codigos/git/jurimetrics/data_extra/count_day_type2.csv')

# agrupando e contando o número de processos por dia por assunto
count_day_subject2 <- data %>%
  group_by(tipo_processo, assunto_cnj) %>%
  summarize(count = n())
use_data(count_day_subject2, overwrite = T)
write_csv(count_day_subject2, '~/Dropbox/Jurimetria/codigos/git/jurimetrics/data_extra/count_day_subject2.csv')



# agrupando e contando o número de processos por dia da semana
count_week_day2 <- data %>%
  group_by(weekDay) %>%
  summarize(count = n())
use_data(count_week_day2, overwrite = T)
write_csv(count_week_day2, '~/Dropbox/Jurimetria/codigos/git/jurimetrics/data_extra/count_week_day2.csv')
# pareto.chart(table(data$weekDay))

# agrupando e contando o número de processos por dia da semana por tipo de processo
count_week_day_type2 <- data %>%
  group_by(weekDay, tipo_processo) %>%
  summarize(count = n())
use_data(count_week_day_type2, overwrite = T)
write_csv(count_week_day_type2, '~/Dropbox/Jurimetria/codigos/git/jurimetrics/data_extra/count_week_day_type2.csv')

# agrupando e contando o número de processos por dia da semana por assunto
count_week_day_subject2 <- data %>%
  group_by(weekDay, assunto_cnj) %>%
  summarize(count = n())
use_data(count_week_day_subject2, overwrite = T)
write_csv(count_week_day_subject2, '~/Dropbox/Jurimetria/codigos/git/jurimetrics/data_extra/count_day_subject2.csv')



# agrupando e contando o número de processos por mês
count_year_month2 <- data %>%
  group_by(yearMonth) %>%
  summarize(count = n())
use_data(count_year_month2, overwrite = T)
write_csv(count_year_month2, '~/Dropbox/Jurimetria/codigos/git/jurimetrics/data_extra/count_year_month2.csv')
# ggplot(count_year_month, aes(yearMonth, count)) +
#   geom_line()

# agrupando e contando o número de processos por dia da semana por tipo de processo
count_year_month_type2 <- data %>%
  group_by(yearMonth, tipo_processo) %>%
  summarize(count = n())
use_data(count_year_month_type2, overwrite = T)
write_csv(count_year_month_type2, '~/Dropbox/Jurimetria/codigos/git/jurimetrics/data_extra/count_year_month_type2.csv')

# agrupando e contando o número de processos por dia da semana por assunto
count_year_month_subject2 <- data %>%
  group_by(yearMonth, assunto_cnj) %>%
  summarize(count = n())
use_data(count_year_month_subject2, overwrite = T)
write_csv(count_year_month_subject2, '~/Dropbox/Jurimetria/codigos/git/jurimetrics/data_extra/count_year_month_subject2.csv')


# data("count_day")
# count_day

# # projetando
# ini <- Sys.time()
# y <- ts(count_year_month$count, start = c(2000,1), frequency = 12)
# fits(y, steps = 48, lim = F, graf = T, PI = F)
# Sys.time()-ini   # Time difference of 6.028008 mins, PI = T

