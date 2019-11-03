devtools::install_github('filipezabala/jurimetrics', force=T)
library(jurimetrics)
?fits
fits(livestock)
data("count_year_month")
y <- ts(count_year_month$count, start = c(2000,1), frequency = 12)
fits(y, show.sec.graph = T)