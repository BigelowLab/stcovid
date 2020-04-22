#' stcovid package
#'
#' Read and filter [COVID-19](https://github.com/covid19datahub/COVID19) data offerings,
#' with a county-by-sate centric perspective.
#'
#' @docType package
#' @importFrom dplyr %>%
#' @importFrom rlang := .data !!
#' @name stcovid
NULL

if(getRversion() >= "2.15.1") {
  utils::globalVariables(c(".", "state.abb", "state.name"))
}
