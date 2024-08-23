# jurimetrics
General-purpose Tools for Jurimetrics.

```r
# installig and calling package
devtools::install_github('filipezabala/jurimetrics', force=T)
library(jurimetrics)

# getting help
?fits

# Livestock (sheep) in Asia, 1961-2007.
fits(livestock)

# Lower quota for the volume of cases in the second instance of the TJ-RS (Brazil)
y <- ts(tjrs_00_17$count_adjusted, start = c(2000,1), frequency = 12)
fits(y)
```
