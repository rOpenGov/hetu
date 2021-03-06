% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rpin.R
\name{rpin}
\alias{rpin}
\alias{rhetu}
\title{Generate a vector of random \code{hetu}}
\usage{
rpin(
  n,
  start_date = as.Date("1895-01-01"),
  end_date = as.Date(Sys.Date()),
  p.male = 0.4,
  p.temp = 0
)

rhetu(
  n,
  start_date = as.Date("1895-01-01"),
  end_date = as.Date(Sys.Date()),
  p.male = 0.4,
  p.temp = 0
)
}
\arguments{
\item{n}{number of generated \code{hetu}-pins}

\item{start_date}{Lower limit of generated \code{hetu} dates. Default is 1895-01-01.}

\item{end_date}{Upper limit of generated \code{hetu}. Default is the current date.}

\item{p.male}{Proportion of males. Default is 0.4.}

\item{p.temp}{Proportion of temporary identification numbers. Default is 0.0.}
}
\value{
a vector of generated \code{hetu}-pins.
}
\description{
A function that generates random \code{hetu}-pins.
}
\examples{
x <- rpin(3)
hetu(x)
hetu(x, extract = "sex")
hetu(x, extract = "checksum")

x <- rhetu(3)
x
}
\author{
Pyry Kantanen, Jussi Paananen
}
