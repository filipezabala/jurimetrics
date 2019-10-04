library(tidyverse)  # varios
library(devtools)

setwd('~/Dropbox/Jurimetria/codigos/git/jurimetrics/data/')

poisson1837 <- read_delim('poisson1837.txt', ' ')
use_data(poisson1837, overwrite = TRUE)
