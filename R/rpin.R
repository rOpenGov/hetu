#' Generate a vector of random \code{hetu}
#'
#' @description 
#' A function that generates random \code{hetu}-pins. 
#' The generated \code{hetu} is uniformely distributed over the time period.
#'
#' @param n number of observations.
#' @param start_date Lower limit of generated \code{hetu} dates. Default is 1895-01-01.
#' @param end_date Upper limit of generated \code{hetu}. Default is the current date.
#' @param p.male Proportion of males. Default is 0.4.
#' @param p.temp Proportion of temporary identification numbers. Default is 0.0.
#' 
#' @return a vector of generated \code{hetu}-pins.
#' 
#' @examples 
#' x <- rpin(3)
#' hetu(x)
#' hetu(x, extract = "sex")
#' hetu(x, extract = "checksum")
#' 
#' @export
rpin <- function(n, start_date = as.Date("1895-01-01"), end_date = Sys.Date(), p.male = 0.4, p.temp = 0.0){
  rdate <- sample(start_date:end_date, n, replace = TRUE)
  # origin date according to POSIX standard
  rdate <- as.Date(rdate, origin = "1970-01-01")
  
  ddmmyy <- format(rdate, "%d%m%y")
  
  # determine the correct century marker
  century <- function(x) {
    switch(substr(x, 1, 2),
         "20" = "A",
         "19" = "-",
         "18" = "+",
    )
  }
  
  test_zzz <- "000"
  while ("000" %in% test_zzz | "001" %in% test_zzz) {
    zz_norm <- sample(x = 0:89, replace = TRUE, size = round(n*(1-p.temp)))
    zz_temp <- sample(x = 90:99, replace = TRUE, size = round(n*p.temp))
    zz <- append(zz_norm, zz_temp)
    #randomize order of pins to make the vector seem more natural
    zz <- sample(zz)
    zz <- formatC(zz, width = 2, format = "d", flag = "0")
    z <- sample(x = as.character(c(0, 2, 4, 6, 8, 1, 3, 5, 7, 9)), 
                prob = c(rep(1 - p.male, 5), rep(p.male, 5)), replace = TRUE, 
                size = n)
    test_zzz <- paste0(zz,z)
  }
  
  # characters used for determining the checksum of hetu
  checklist <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", 
                 "A", "B", "C", "D", "E", "F", "H", "J", "K", "L", 
                 "M", "N", "P", "R", "S", "T", "U", "V", "W", "X", "Y")
  
  # "+ 1" is necessary as checklist[0] returns nothing
  checksum <- checklist[as.numeric(paste0(ddmmyy, zz, z)) %% 31 + 1]
  paste0(ddmmyy, sapply(rdate, century), zz, z, checksum)
}