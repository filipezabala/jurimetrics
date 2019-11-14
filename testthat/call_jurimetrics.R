devtools::install_github('filipezabala/jurimetrics', force=T)
library(jurimetrics)
?fits
fits(livestock)

data('tjmg_year')
y1 <- ts(tjmg_year$count, start = c(2000,1), frequency = 1)
fits(y1)

data('tjrs_year_month')
y2 <- ts(tjrs_year_month$count, start = c(2000,1), frequency = 12)
fits(y2, show.sec.graph = T)

