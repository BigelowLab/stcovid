#' Convert state abbreviation to full name
#'
#' @export
#' @param x character, one or more state abbreviations
#' @return full state names or NA when not found
st_abb2name <- function(x = state.abb){
  state.name[match(x, state.abb)]
}

#' Convert state names to abbreviations
#'
#' @export
#' @param x character, one or more state names
#' @return state abbreviations or NA when not found
st_name2abb <- function(x = state.name){
  state.abb[match(x, state.name)]
}

