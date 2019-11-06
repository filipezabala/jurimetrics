library(tidyverse)  # varios
library(devtools)

setwd('~/Dropbox/Jurimetria/codigos/git/jurimetrics/data/')

(poisson1837 <- read.table('poisson1837.txt', sep = ' ', header = T))
use_data(poisson1837, overwrite = TRUE)

tjmg_year <- read.table('tjmg_year.csv', sep = ',', header = T)
use_data(tjmg_year, overwrite = TRUE)
