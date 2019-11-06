library(tidyverse)  # varios
library(devtools)

setwd('~/Dropbox/Jurimetria/codigos/git/jurimetrics/data/')

(poisson1837 <- read.table('poisson1837.txt', sep = ' ', header = T))
use_data(poisson1837, overwrite = TRUE)

tjmg_volume_year <- read.table('tjmg_volume_year.csv', sep = ',', header = T)
use_data(tjmg_volume_year, overwrite = TRUE)
data("tjmg_volume_year")
tjmg_volume_year
