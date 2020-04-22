#' Retrieve a CDC path
#'
#' @export
#' @param ... path segments passed to file.path
#' @param root character, the root path to the CDC data directory
#' @return a file/directory path (not tested for existence)
stcovid_path <- function(...,
                       root = "/mnt/ecocast/projectdata/covid19/stcovid"){
  file.path(root, ...)
}
