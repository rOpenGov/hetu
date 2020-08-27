#' Generate a vector of random \code{hetu}
#'
#' @description 
#' A function that generates random \code{hetu}-pins. 
#' The generated \code{hetu} is uniformely distributed over the time period.
#'
#' @param n number of observations.
#' @param start_date Lower limit of generated \code{hetu} dates. Default is 1895-01-01.
#' @param end_date Upper limit of generated \code{hetu}. Default is the current date.
#' @param p.male Proportion of males. Default is 0.3.
#' @param p.temp Proportion of temporary identification numbers. Default is 0.1. Currently not implemented.
#' 
#' @return a vector of generated \code{hetu}-pins.
#' 
#' @examples 
#' x <- rpin(3)
#' hetu(x)
#' hetu(x, extract = "gender")
#' hetu(x, extract = "checksum")
#' 
#' @export
rpin <- function(n, start_date = as.Date("1895-01-01"), end_date = Sys.Date(), p.male = 0.3, p.temp = 0.1){
  rdate <- sample(start_date:end_date, n, replace = TRUE)
  rdate <- as.Date(rdate, origin = "1970-01-01")
  ddmmyy <- format(rdate, "%d%m%y")
  century <- function(x) {
    switch(substr(x, 1, 2),
         "20" = "A",
         "19" = "-",
         "18" = "+",
         stop("Invalid `x` value")
    )
  }
  sapply(rdate, century)
  zz <- sample(0:89, n, replace = TRUE)
  zz <- formatC(zz, width = 2, format = "d", flag = "0")
  z <- sample(x = as.character(c(2, 4, 6, 8, 3, 5, 7, 9)), 
            prob = c(rep(1 - p.male, 4), rep(p.male, 4)), replace = TRUE, 
            size = n)
  checklist <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E", "F", "H", "J", "K", "L", "M", "N", "P", "R", "S", "T", "U", "V", "W", "X", "Y")
  checksum <- checklist[as.numeric(paste0(ddmmyy, zz, z)) %% 31 + 1]
  paste0(ddmmyy, sapply(rdate, century), zz, z, checksum)
}
