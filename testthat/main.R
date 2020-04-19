# main file
# http://r-pkgs.had.co.nz/

# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

# packs
library(devtools)

# session_info
# session_info()

# creating manual
devtools::document()

# loading jurimetrics
devtools::load_all()
# roxygen2::roxygenize()
search()

# installing and attaching jurimetrics
devtools::install_github('filipezabala/jurimetrics', force = T)
library(jurimetrics)
?fits
?theme_doj

data(count_day_subject)
data(count_day_type)
data(count_day)
data(count_week_day_subject)
data(count_week_day_type)
data(count_week_day)
data(count_year_month_subject)
data(count_year_month_type)
data(count_year_month)

