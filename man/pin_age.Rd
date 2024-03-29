% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pin_age.R
\name{pin_age}
\alias{pin_age}
\alias{hetu_age}
\title{Extract Age from Personal Identity Code}
\usage{
pin_age(pin, date = Sys.Date(), timespan = "years", allow.temp = FALSE)

hetu_age(pin, date = Sys.Date(), timespan = "years", allow.temp = FALSE)
}
\arguments{
\item{pin}{Finnish personal identity code(s) as a character vector}

\item{date}{Date at which age is calculated. If a vector is provided it
must be of the same length as the \code{pin} argument.}

\item{timespan}{Timespan to use to calculate age. The possible timespans are:
\itemize{
  \item \code{years} (Default)
  \item \code{months}
  \item \code{weeks}
  \item \code{days}
}}

\item{allow.temp}{Allow artificial or temporary PINs (personal numbers
900-999). If \code{FALSE} (default), only PINs intended for official
use (personal numbers 002-899) are allowed.}
}
\value{
Age as an integer vector.
}
\description{
Calculate age in years, months, weeks or days from 
   personal identity codes.
}
\examples{
ex_pin <- c("010101-0101", "111111-111C")
pin_age(ex_pin, date = "2012-01-01")

ex_pin <- c("010101-0101", "111111-111C")
hetu_age(ex_pin, date = "2012-01-01")
}
