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
setwd('~/Documentos/tjrs/arquivosLiteExcel/')  # Linux

# lendo bancos de dados 2000:2017
ini <- Sys.time()
data <- vector('list')
for(i in 2010:2017){
  df <- read_xlsx(paste0('tjrs',i,'.xlsx'))
  temp0 <- substr(df$`Data de Julgamento`,3,12)
  ssplit1 <- sapply(df$`Tipo de Processo`, strsplit, "'")
  temp1 <- as.character(t(bind_cols(ssplit1)[2,]))
  ssplit2 <- sapply(df$`Assunto CNJ`, strsplit, "'")
  temp2 <- as.character(t(bind_cols(ssplit2)[2,]))
  data[[paste0(i)]] <- tibble(judgmentDate = as.Date(temp0,  format = '%d/%m/%Y'),
                              type = temp1,
                              subject = temp2)
  # names(data[[paste0(i)]]) <- 'judgmentDate'
  print(paste0(i))
}
memory()
Sys.time() - ini # Time difference of 4.599254 mins


# # 2018:2020
# setwd('~/Documentos/tjrs/csvFromXlsx/')
#
# # lendo bancos de dados csv
# ini <- Sys.time()
# data2 <- vector('list')
# for(i in 2017:2017){
#   df <- read_csv(paste0('tjrs_',i,'.csv'))
#   temp <- substr(df$data_julgamento,1,10)
#   rm(df)
#   gc()
#   data2[[paste0(i)]] <- as_tibble(as.Date(temp, format = '%Y-%m-%d'))
#   names(data2[[paste0(i)]]) <- 'judgmentDate'
#   print(paste0(i))
# }
# gc()
# memory()
# Sys.time() - ini # Time difference of 4.720226 mins


# criando colunas de semana e mês
ini <- Sys.time()
data <- lapply(data, mutate, weekDay = lubridate::wday(judgmentDate, label = T))
data <- lapply(data, mutate, yearMonth = as.Date(paste0(substr(judgmentDate,1,7),'-01'),
                                                 format = '%Y-%m-%d'))
# empilhando
data <- do.call(rbind, data)
Sys.time() - ini # Time difference of 27.99515 secs

# new wd
setwd('~/Dropbox/Jurimetria/codigos/git/jurimetrics/data/')

# agrupando e contando o número de processos por dia
count_day <- data %>%
  group_by(judgmentDate) %>%
  summarize(count = n())
use_data(count_day, overwrite = T)
write_csv(count_day, '~/Dropbox/Jurimetria/codigos/git/jurimetrics/data_extra/count_day.csv')

# agrupando e contando o número de processos por dia por tipo de processo
count_day_type <- data %>%
  group_by(judgmentDate, type) %>%
  summarize(count = n())
use_data(count_day_type, overwrite = T)
write_csv(count_day_type, '~/Dropbox/Jurimetria/codigos/git/jurimetrics/data_extra/count_day_type.csv')

# agrupando e contando o número de processos por dia por assunto
count_day_subject <- data %>%
  group_by(judgmentDate, subject) %>%
  summarize(count = n())
use_data(count_day_subject, overwrite = T)
write_csv(count_day_subject, '~/Dropbox/Jurimetria/codigos/git/jurimetrics/data_extra/count_day_subject.csv')



# agrupando e contando o número de processos por dia da semana
count_week_day <- data %>%
  group_by(weekDay) %>%
  summarize(count = n())
use_data(count_week_day, overwrite = T)
write_csv(count_week_day, '~/Dropbox/Jurimetria/codigos/git/jurimetrics/data_extra/count_week_day.csv')
# pareto.chart(table(data$weekDay))

# agrupando e contando o número de processos por dia da semana por tipo de processo
count_week_day_type <- data %>%
  group_by(weekDay, type) %>%
  summarize(count = n())
use_data(count_week_day_type, overwrite = T)
write_csv(count_week_day_type, '~/Dropbox/Jurimetria/codigos/git/jurimetrics/data_extra/count_week_day_type.csv')

# agrupando e contando o número de processos por dia da semana por assunto
count_week_day_subject <- data %>%
  group_by(weekDay, subject) %>%
  summarize(count = n())
use_data(count_week_day_subject, overwrite = T)
write_csv(count_week_day_subject, '~/Dropbox/Jurimetria/codigos/git/jurimetrics/data_extra/count_day_subject.csv')



# agrupando e contando o número de processos por mês
count_year_month <- data %>%
  group_by(yearMonth) %>%
  summarize(count = n())
use_data(count_year_month, overwrite = T)
write_csv(count_year_month, '~/Dropbox/Jurimetria/codigos/git/jurimetrics/data_extra/count_year_month.csv')
# ggplot(count_year_month, aes(yearMonth, count)) +
#   geom_line()

# agrupando e contando o número de processos por dia da semana por tipo de processo
count_year_month_type <- data %>%
  group_by(yearMonth, type) %>%
  summarize(count = n())
use_data(count_year_month_type, overwrite = T)
write_csv(count_year_month_type, '~/Dropbox/Jurimetria/codigos/git/jurimetrics/data_extra/count_year_month_type.csv')

# agrupando e contando o número de processos por dia da semana por assunto
count_year_month_subject <- data %>%
  group_by(yearMonth, subject) %>%
  summarize(count = n())
use_data(count_year_month_subject, overwrite = T)
write_csv(count_year_month_subject, '~/Dropbox/Jurimetria/codigos/git/jurimetrics/data_extra/count_year_month_subject.csv')


# data("count_day")
# count_day

# # projetando
# ini <- Sys.time()
# y <- ts(count_year_month$count, start = c(2000,1), frequency = 12)
# fits(y, steps = 48, lim = F, graf = T, PI = F)
# Sys.time()-ini   # Time difference of 6.028008 mins, PI = T

