% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pin_ctrl.R
\name{bid_ctrl}
\alias{bid_ctrl}
\title{Check Validity of Finnish Business ID (Y-tunnus)}
\usage{
bid_ctrl(bid)
}
\arguments{
\item{bid}{a vector of 1 or more business identity numbers}
}
\description{
A function that checks whether a \code{bid} (Finnish Business ID) is valid.
Returns \code{TRUE} or \code{FALSE}.
}
\examples{
bid_ctrl(c("0000000-0", "0000001-9")) # TRUE TRUE
bid_ctrl("0737546-1") # FALSE
}
