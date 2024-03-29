% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rpin.R
\name{rpin}
\alias{rpin}
\alias{rhetu}
\title{Generate Random Personal Identity Codes}
\usage{
rpin(
  n,
  start.date = as.Date("1895-01-01"),
  end.date = Sys.Date(),
  p.male = 0.4,
  p.temp = 0,
  num.cores = 1
)

rhetu(
  n,
  start.date = as.Date("1895-01-01"),
  end.date = Sys.Date(),
  p.male = 0.4,
  p.temp = 0,
  num.cores = 1
)
}
\arguments{
\item{n}{number of generated \code{hetu}-pins}

\item{start.date}{Lower limit of generated \code{hetu} dates,
character string in ISO 8601 standard, for example "2001-02-03".
Default is "1895-01-01".}

\item{end.date}{Upper limit of generated \code{hetu}.
Default is current date.}

\item{p.male}{Probability of males, between 0.0 and 1.0. Default is 0.4.}

\item{p.temp}{Probability of temporary identification numbers, between
0.0 and 1.0. Default is 0.0.}

\item{num.cores}{The number of cores for parallel processing. The number
of available cores can be determined with \code{detectCores()}.
Default is 1.}
}
\value{
a vector of generated \code{hetu}-pins.
}
\description{
A function that generates random Finnish personal identity codes 
   (\code{hetu} codes).
}
\details{
There is a finite number of valid personal identity codes available per day.
More specifically, there are 498 odd personal numbers for males and 498 even
personal numbers for females from range 002-899. Additionally there are 50
odd numbers for males and 50 even numbers for females in the temporary
personal identity code number range 900-999 that is not normally in use.
This function will return an error "too few positive probabilities" in
sample.int function if you try to generate too many codes in a short enough
timeframe.

The theoretical upper limit of valid PINs is in the millions since there are
898 PINs available for each day, 327770 for each year. In practice this
number is much lower since same personal number component cannot be
"recycled" if it has been used in the past. To illustrate, if an identity
code "010101-0101" has already been assigned to someone born in 1901-01-01,
a similar code "010101A0101" for someone born in 2001-01-01 could not be
used.
}
\examples{
x <- rpin(3)
hetu(x)
hetu(x, extract = "sex")
hetu(x, extract = "ctrl.char")

x <- rhetu(3)
x
}
\author{
Pyry Kantanen, Jussi Paananen
}
