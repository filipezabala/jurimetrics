# libs
library(jurimetrics)
library(tidyverse)
library(readxl)
library(parallel)
library(stringi)
library(xlsx)

# wd
setwd('~/Desktop/wbs/arquivos/tjrs_2020/')

# read files
files <- dir(pattern = '*.xlsx')
dfs <- mclapply(files, readxl::read_xlsx, mc.cores = detectCores())

# getting ementa
ementa <- mclapply(dfs, dplyr::select, 'ementa', mc.cores = detectCores())

# cleaning ementa
ementa <- mclapply(ementa, apply, 1, stringi::stri_trans_tolower,
                   mc.cores = detectCores())
ementa <- mclapply(ementa, dplyr::as_tibble, mc.cores = detectCores())

# labeling dano moral
pdm <- mclapply(ementa, apply, 1, jurimetrics::has_pattern,
                jurimetrics::pattern_danomoral(),
                mc.cores = detectCores())
sum(sapply(pdm,sum))
pdm <- lapply(pdm, as_tibble)

# binding
dfs <- bind_cols(bind_rows(dfs), bind_rows(pdm))

# removing duplicates
dfs <- distinct(dfs)

# filtering
dfsDM_full <- dfs %>%
  rename(danomoral = value) %>%
  filter(danomoral == T, decisao == 'Acordao', tipo_processo == 'Apelação Cível')

dfsDM_clean <- dfsDM_full %>%
  select(numero_processo, html)

dfsDM_html <- dfsDM_full %>%
  select(html)

# writing
setwd('~/Dropbox/Jurimetria/contatos/@Escola_AJURIS/dados/')
xlsx::write.xlsx(dfsDM_full, 'danomoral2020_full.xlsx')
xlsx::write.xlsx(dfsDM_clean,  'danomoral2020_clean.xlsx')
write_csv(dfsDM_clean,  'danomoral2020_clean.csv')
