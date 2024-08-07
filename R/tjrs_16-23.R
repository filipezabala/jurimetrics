#' TJRS 2016-2023 data
#'
#' Number of cases judged in the second instance of the TJRS between 2016 and 2023.
#'
#' @format ## `tjrs_2016_2023`
#' A data frame with 96 rows and 3 columns:
#' \describe{
#'   \item{yearMonth}{Year and month of the judgment.}
#'   \item{count}{Lower quota of cases judged in the second instance of the TJRS using a custom scraper.}
#'   \item{count_adjusted}{Adjusted lower quota of cases judged in the second instance of the TJRS using a custom scraper, corrected considering the slope between tjrs_2000_2017 and tjrs_2016_2023 counts.}
#' }
#' @source <https://www.tjrs.jus.br/buscas/jurisprudencia/?aba=jurisprudencia>
"tjrs_2016_2023"
