#' Create a two panel plot of COVID-19, by count and by density of confirmed
#' cases.
#'
#' @export
#' @param x sf object of merged COVID-19 and census data
#' @param N numeric, scaling for density.  By default cases per 1000 people.
#' @return a list of 2 ggplot2 graphics.  Use patchwork to draw together ala
#'     \code{result[[1]] + result[[2]]}
draw_confirmed <- function(x = merge_census(),
                          N = 1000){

  if (inherits(x$date[1], "character")){
    the_date <- x$date[1]
  } else {
    the_date <- format(x$date[1], "%Y-%m-%d")
  }

  x <- x %>%
    dplyr::mutate(dConfirmed = .data$Confirmed/(.data$pop/N))
  p1 <- ggplot2::ggplot(data = x)  +
    ggplot2::geom_sf(ggplot2::aes(fill = .data$Confirmed)) +
    ggplot2::labs(title = 'Cumulative Confirmed Cases',
                  subtitle = the_date,
                  fill = "Cases",
                  caption = "COVID-19 Source: https://covid19datahub.io/")
  p2 <- ggplot2::ggplot(data = x)  +
    ggplot2::geom_sf(ggplot2::aes(fill = .data$dConfirmed)) +
    ggplot2::labs(title = 'Cumulative Confirmed Density',
                  subtitle = the_date,
                  fill = sprintf("Cases/%i",N),
                  caption = "Census Source: US Census Bureau")
  list(p1, p2)
}
