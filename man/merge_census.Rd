% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/covid19.R
\name{merge_census}
\alias{merge_census}
\title{Merge datahub and census data - really set up for one day in one state}
\usage{
merge_census(
  x = read_datahub(state = "Maine", date = as.Date("2020-04-20")),
  pop = read_census(state = "Maine")
)
}
\arguments{
\item{x}{tibble, the COVID-19 cases}

\item{pop}{sf object, as per \code{\link{read_census}}}
}
\value{
the merged covid/census table as sf MULTIPOLYGON
}
\description{
Merge datahub and census data - really set up for one day in one state
}
