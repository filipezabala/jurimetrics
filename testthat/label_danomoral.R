# libs
library(jurimetrics)
library(tidyverse)
library(readxl)
library(parallel)
library(stringi)
# library(textclean)


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
lapply(pdm,sum)
