devtools::install_github('filipezabala/jurimetrics', force=T)
library(jurimetrics)
?fits
fits(livestock)
data('tjrs_year_month')
y <- ts(tjrs_year_month$count, start = c(2000,1), frequency = 12)
fits(y, show.sec.graph = T)

data('tjmg_year')
y2 <- ts(tjmg_year$count, start = c(2000,1), frequency = 1)
fits(y2)
