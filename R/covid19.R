#' Merge datahub and census data - really set up for one day in one state
#'
#' @export
#' @param x tibble, the COVID-19 cases
#' @param pop sf object, as per \code{\link{read_census}}
#' @return the merged covid/census table as sf MULTIPOLYGON
merge_census <- function(x = read_datahub(state = "Maine",
                                         date = as.Date("2020-04-20")),
                               pop = read_census(state = "Maine")){
  if (nrow(x) == 0) return(x)

  if (nrow(pop) < nrow(x)){
    ix <- x$County %in% pop$County
    x <- x %>%
      dplyr::filter(ix)
  }
  if (nrow(x) < nrow(pop)){
    ix <- pop$County %in% x$County
    pop <- pop %>%
      dplyr::filter(ix)
  }

  x2 <- x %>%
    dplyr::arrange(.data$County) %>%
    dplyr::select(-.data$State, -.data$County)
  pop %>%
    dplyr::arrange(.data$County) %>%
    dplyr::bind_cols(x2) %>%
    dplyr::select(
      .data$date,
      .data$geoid,
      .data$County,
      .data$pop,
      .data$density,
      .data$Confirmed,
      .data$Recovered,
      .data$Hospitalizations,
      .data$Deaths
    )
}


#' Read the locally store datahub table
#'
#' @export
#' @param filename character, the name of the data file
#' @param state NULL or character, to filter the data
#' @param date NULL or Date, to filter the data
#' @return tibble
read_datahub <- function(
  filename = stcovid_path("cumcounts","datahub.csv"),
  state = NULL,
  date = NULL){
  x <- suppressMessages(readr::read_csv(filename))
  if (!is.null(state)) {
    x <- x %>%
      dplyr::filter(.data$State %in% state)
  }
  if (!is.null(date)){
    if (!inherits(date, "Date")) date <- as.Date(date)
    ix <- x$date %in% date
    x <- x %>%
      dplyr::filter(ix)
  }
  x
}

#' Update the locally stored datahub table
#'
#' @export
#' @param filename character, the name of the data file with datahub data
#' @return tibble of COVID19 data hub data
update_datahub <- function(
  filename = stcovid_path("cumcounts","datahub.csv")){

  x <- read_datahub(filename = filename)
  newdata <- fetch_datahub(start = max(x$date) + 1, save_file = FALSE)
  if (nrow(newdata) > 0){
    x <- x %>%
      dplyr::bind_rows(newdata) %>%
      dplyr::distinct() %>%
      readr::write_csv(filename)
  }
  x
}


#' Fetch updated county data from COVID19 data hub
#'
#' @seealso \url{https://covid19datahub.io/}
#' @export
#' @param simplify logical, if TRUE trim the dataset
#' @param save_file logical, if TRUE save the file.  Ignored if simplify is FALSE.
#' @param path charcater, the path to save the data
#' @param ... further arguments for \code{\link[COVID19]{covid19}}
#' @return tibble of COVID19 data hub data
fetch_datahub <- function(simplify = TRUE,
                          save_file = TRUE,
                          path = stcovid_path("cumcounts"),
                          ...){

  split_id <- function(x){
    x <- dplyr::as_tibble(do.call(rbind, strsplit(x, ", ", fixed = TRUE)))
    colnames(x) <- c("Country", "State", "County")
    x
  }
  x <- suppressWarnings(COVID19::covid19("USA", level = 3, ...))

  if (simplify){
    x <- x %>%
      dplyr::ungroup() %>%
      dplyr::filter(!grepl("Out of", .data$id, fixed = TRUE) &
                    !grepl("Unassigned", .data$id, fixed = TRUE)) %>%
      dplyr::select(
        .data$date,
        State = .data$state,
        County = .data$city,
        Confirmed = .data$confirmed,
        Recovered = .data$recovered,
        Hospitalizations = .data$hosp,
        Deaths = .data$deaths) %>%
      dplyr::mutate(date = as.Date(.data$date))
    if (save_file){
      x <- readr::write_csv(x, file.path(path, "datahub.csv"))
    }
  }
  x
}
