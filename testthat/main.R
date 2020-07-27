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

# pre install gcc (terminal)
# brew install gcc
# brew install gcc@5

# cd /usr/local/bin/
# ln -s gcc-9 gcc
# ln -s g++-9 g++
# brew link gcc
# brew link --overwrite gcc
# pip3 install xgboost

# installing and attaching jurimetrics
devtools::install_github('filipezabala/jurimetrics', force = T)
library(jurimetrics)
?fits
?theme_doj

citation('jurimetrics')

data(count_day_subject)
data(count_day_subject2)

data(count_day_type)
data(count_day)
data(count_week_day_subject)
data(count_week_day_type)
data(count_week_day)
data(count_year_month_subject)
data(count_year_month_subject2)

data(count_year_month_type)
data(count_year_month_type2)

data(count_year_month)
data(count_year_month2)
count_year_month2$count

count_year_month2 %>%
  print(n = Inf)
