#' Read locally stored state population, density and boundaries from US Census
#'
#' @export
#' @param state character, the name of the state
#' @param path character, the path to the data file
#' @return sf MULTIPOLYGIN object of county populations and boundaries
read_census <- function(state = "Maine",
                        path = stcovid_path("census")){
  abb <- st_name2abb(state[1])
  if (is.na(abb)) stop("state not found:", state[1])
  sf::read_sf(file.path(path, paste0(abb, ".geojson")))
}

#' Retrieve the CENSUS_API_KEY
#'
#' @export
#' @return character key
get_census_key <- function(){
  Sys.getenv("CENSUS_API_KEY")
}

#' Get census estimates for counties by state
#'
#' @export
#' @param state character, the name of the state or 'all'
#' @param simplify logical, if TRUE simplify into a familiar form
#' @param save_file logical if TRUE save as geojson, ignored if simplify = FALSE
#' @param path character, the path to save the file
#' @return sf table of county population, density, geoid and geometry.
#'         If \code{state} is 'all' then iterate through all states and return
#'         a list of sf tables
fetch_census_estimates <- function(state = "all",
                                   simplify = TRUE,
                                   save_file = TRUE,
                                   path = stcovid_path("census")){

  if (tolower(state[1] == 'all')){

    xx <- lapply(state.name,
                 function(n){
                   fetch_census_estimates(state = n,
                                          simplify = simplify,
                                          save_file = save_file,
                                          path = path)
                 })
    names(xx) <- state.name
    return(xx)
  }


  stateabb <- st_name2abb(state[1])
  get_county_name <- function(NAME){
    gsub(" County", "",
         sapply(strsplit(NAME, ",", fixed = TRUE), "[[", 1),
         fixed = TRUE)
  }
  x <- suppressMessages(tidycensus::get_estimates(
                            geography = 'county',
                            product = "population",
                            state = stateabb,
                            key = get_census_key(),
                            geometry = TRUE))
  xx <- split(x, x$variable)
  nms <- names(xx)
  xx <- lapply(nms,
               function(nm){
                xx[[nm]] <- xx[[nm]] %>%
                  dplyr::mutate(!!nm := .data$value) %>%
                  dplyr::select(-.data$variable, -.data$value)
               })
  x <- xx[[1]] %>%
    dplyr::mutate(!!nms[2] := xx[[2]][[nms[2]]]) %>%
    dplyr::arrange(.data$NAME)

  if (simplify) {
    x <- x %>%
    dplyr::mutate(NAME = get_county_name(.data$NAME),
                  pop = .data$POP,
                  density = .data$DENSITY) %>%
    dplyr:: select(geoid = .data$GEOID,
                   County = .data$NAME,
                   .data$pop,
                   .data$density,
                   .data$geometry)

    if (save_file){
      ofile <- file.path(path, paste0(stateabb, ".geojson"))
      if (file.exists(ofile)) ok <- file.remove(ofile)
      x <- sf::write_sf(x, ofile)
    }

  }
  x
}

