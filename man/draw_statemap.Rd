% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graphics.R
\name{draw_statemap}
\alias{draw_statemap}
\title{Create a two maps of COVID-19, by count and by density of confirmed
cases.}
\usage{
draw_statemap(x = merge_census(), varname = "Confirmed", N = 1000)
}
\arguments{
\item{x}{sf object of merged COVID-19 and census data}

\item{varname}{character, the name of the variable to draw ala
'Confirmed', 'Recovered', 'Deaths' etc.}

\item{N}{numeric, scaling for density.  By default cases per 1000 people.}
}
\value{
a list of 2 ggplot2 graphics.  Use patchwork to draw together ala
    \code{result[[1]] + result[[2]]}
}
\description{
Create a two maps of COVID-19, by count and by density of confirmed
cases.
}
