#' Create a two maps of COVID-19, by count and by density of confirmed
#' cases.
#'
#' @export
#' @param x sf object of merged COVID-19 and census data
#' @param varname character, the name of the variable to draw ala
#'        'Confirmed', 'Recovered', 'Deaths' etc.
#' @param N numeric, scaling for density.  By default cases per 1000 people.
#' @return a list of 2 ggplot2 graphics.  Use patchwork to draw together ala
#'     \code{result[[1]] + result[[2]]}
draw_statemap <- function(x = merge_census(),
                          varname = 'Confirmed',
                          N = 1000){

  if (!(varname[1]) %in% colnames(x)) stop("varname not found: ", varname[1])
  dvarname <- paste0("d", varname)
  x <- x %>%
    dplyr::mutate(!!dvarname := .data[[varname]]/(.data$pop/N))

  if (inherits(x$date[1], "character")){
    the_date <- x$date[1]
  } else {
    the_date <- format(x$date[1], "%Y-%m-%d")
  }

  p1 <- ggplot2::ggplot(data = x)  +
    ggplot2::geom_sf(ggplot2::aes(fill = .data[[varname]])) +
    ggplot2::labs(title = sprintf('Cumulative %s Cases', varname),
                  subtitle = the_date,
                  fill = "Cases",
                  caption = "COVID-19 Source: https://covid19datahub.io/")
  p2 <- ggplot2::ggplot(data = x)  +
    ggplot2::geom_sf(ggplot2::aes(fill = .data[[dvarname]])) +
    ggplot2::labs(title = sprintf('Cumulative %s Density', varname),
                  subtitle = the_date,
                  fill = sprintf("Cases/%i", N),
                  caption = "Census Source: US Census Bureau")
  list(p1, p2)
}

#' Create a plot of COVID-19, by count and by density of confirmed
#' cases.
#'
#' @export
#' @param x tibble of COVID-19 and census data
#' @param pop SF MUTLIPOLYGON table of county level geometry and population
#' @param varname character, the name of the variable to draw ala
#'        'Confirmed', 'Recovered', 'Deaths' etc.
#' @param State_name character, for title, otherwise skip with ""
#' @param N numeric, scaling for density.  By default cases per 1000 people.
#' @param threshold numeric, clip records (by county) where varname is less than this
#'        value.  Set to NA to keep all.
#' @param logscale logical, if TRUE then logscale the the dependent variables
#' @return a ggplot object
draw_history <- function(x = read_datahub(state = "Maine"),
                         pop = read_census(state = "Maine"),
                         varname = 'Confirmed',
                         State_name = "",
                         N = 1,
                         threshold = c(10, NA)[1],
                         logscale = TRUE){

  if (FALSE){
    x = read_datahub(state = "Maine")
    pop = read_census(state = "Maine")
    varname = 'Confirmed'
    State_name = ""
    N = 1
    threshold = c(10, NA)[1]
    logscale = TRUE
  }
  if (!(varname[1]) %in% colnames(x)) stop("varname not found: ", varname[1])
  dvarname <- paste0("d", varname[1])
  ix <- match(x$County, pop$County)
  x <- x %>%
    dplyr::mutate(pop = pop$pop[ix],
                  !!dvarname := .data[[varname]]/(.data$pop/N))
  if (N > 1){
    origname <- varname[1]
    varname <- dvarname
    title <- sprintf('%s Density', origname)
    ylab <- sprintf('%s Density (/%i)', origname, N)
  } else {
    origname <- varname[1]
    title <- sprintf('%s Cases', origname)
    ylab <- sprintf('%s Cases', origname)
  }
  if (State_name[1] != "") title <- paste(State_name, title, sep = " ")
  if (!is.na(threshold[1])){
    x <- x %>%
      dplyr::group_by(.data$County) %>%
      dplyr::filter(.data[[varname]] >= threshold[1]) %>%
      dplyr::ungroup()
  }
  caption <- "Sources: https://covid19datahub.io/ and https://www.census.gov/"
  ggplot2::ggplot(data = x,
                        ggplot2::aes(.data$date, .data[[varname]],
                                     group = .data$County))  +
    ggplot2::geom_line() +
    ggthemes::scale_color_colorblind() +
    ggplot2::labs(title = title,
                  x = "Date",
                  y = ylab,
                  caption = caption) +
    ggplot2::scale_y_continuous(trans=ifelse(logscale,'log10', 'identity'))
}


#' Create a two plots of COVID-19, by count and by density of confirmed
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

#' Create a two panel plot of COVID-19, by count and by density of confirmed
#' cases.
#'
#' @export
#' @param x tibble of COVID-19 and census data
#' @param pop SF MUTLIPOLYGON table of county level geometry and population
#' @param N numeric, scaling for density.  By default cases per 1000 people.
#' @param logscale logical, if TRUE then logscale the the dependent variables
#' @return a list of 2 ggplot2 graphics.  Use patchwork to draw together ala
#'     \code{result[[1]] + result[[2]]}
draw_infected <- function(x = read_datahub(state = "Maine"),
                          pop = read_census(state = "Maine"),
                          N = 1000,
                          logscale = TRUE){

  ix <- match(x$County, pop$County)
  x <- x %>%
    dplyr::mutate(Infected = .data$Confirmed - .data$Recovered - .data$Deaths,
                  pop = pop$pop[ix],
                  dInfected = .data$Infected/(.data$pop/N)) %>%
    dplyr::group_by(.data$County) %>%
    dplyr::filter(.data$Infected >= 10) %>%
    dplyr::ungroup()

  p1 <- ggplot2::ggplot(data = x,
                        ggplot2::aes(.data$date, .data$Infected,
                                     group = .data$County))  +
    ggplot2::geom_line() +
    ggthemes::scale_color_colorblind() +
    ggplot2::labs(title = 'Infected Cases',
                  x = "Date",
                  y = "Infected Cases",
                  caption = "COVID-19 Source: https://covid19datahub.io/") +
    ggplot2::scale_y_continuous(trans=ifelse(logscale,'log10', 'identity'))

  p2 <- ggplot2::ggplot(data = x,
                        ggplot2::aes(.data$date, .data$dInfected,
                                      group = .data$County))  +
    ggplot2::geom_line() +
    ggthemes::scale_color_colorblind() +
    ggplot2::labs(title = 'Infected Density',
                  x = 'Date',
                  y = sprintf('Infected Case Density ( per %i)', N),
                  caption = "Census Source: US Census Bureau") +
    ggplot2::scale_y_continuous(trans=ifelse(logscale,'log10', 'identity'))

  list(p1, p2)
}

