# jurimetrics
General Purpouse Tools for Jurimetrics.

```
# installig and calling package
devtools::install_github('filipezabala/jurimetrics', force=T)
library(jurimetrics)

# getting help
?fits

# example
fits(livestock)

# processual volume in TJ-RS (Brazil)
data("tjrs_year_month")

# forecasting
y <- ts(tjrs_year_month$count, start = c(2000,1), frequency = 12)
fits(y)
```
