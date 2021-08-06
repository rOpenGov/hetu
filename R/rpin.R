#' @title Generate a vector of random \code{hetu}
#'
#' @description 
#' A function that generates random \code{hetu}-pins. 
#'
#' @param n number of generated \code{hetu}-pins
#' @param start.date Lower limit of generated \code{hetu} dates,
#'    character string in ISO 8601 standard, for example "2001-02-03".
#'    Default is "1895-01-01".
#' @param end.date Upper limit of generated \code{hetu}. 
#'    Default is current date.
#' @param p.male Proportion of males, between 0.0 and 1.0. Default is 0.4.
#' @param p.temp Proportion of temporary identification numbers, between
#'    0.0 and 1.0. Default is 0.0.
#'
#' @return a vector of generated \code{hetu}-pins.
#' 
#' @author Pyry Kantanen, Jussi Paananen
#' 
#' @examples 
#' x <- rpin(3)
#' hetu(x)
#' hetu(x, extract = "sex")
#' hetu(x, extract = "checksum")
#' 
#' @importFrom assertthat assert_that
#' 
#' @export
rpin <- function(n, 
                 start.date = as.Date("1895-01-01"),
                 end.date = Sys.Date(),
                 p.male = 0.4,
                 p.temp = 0.0){
  
  start.date <- as.Date(start.date)
  end.date <- as.Date(end.date)
  
  assert_that(p.temp >= 0 & p.temp <= 1)
  assert_that(p.male >= 0 & p.male <= 1)
  assert_that(end.date <= Sys.Date())
  assert_that(start.date <= end.date)
  assert_that(start.date >= as.Date("1860-01-01"))

  max_p_sex <- max(p.male, (1 - p.male))
  max_p_temp <- max(p.temp, (1 - p.temp))
  
  # available personal numbers per day are 002-899, length(2:899) = 898
  # if p.temp != 0, then available personal numbers are 002-999; 998 numbers
  if (isTRUE(all.equal(p.temp, 0))) {
    max_pins_per_day <- 898
  } else if (p.temp > 0) {
    max_pins_per_day <- 998
  }
  # available personal numbers per sex per day is 898/2 = 449
  max_pins_per_sex <- 449
  # available temp pins per day is length(900:999) = 100
  max_temp_pins <- 100
  
  days_in_time_period <- length(start.date:end.date)
  
  if (n > max_pins_per_day * days_in_time_period){
    stop("You can not generate more random PINs than the maximum number 
  available: 898 per day")
  }
  
  if (max_p_sex * n > max_pins_per_sex * days_in_time_period){
    stop("You can generate only 449 PINs per sex per day")
  }
  
  if (p.temp * n > max_temp_pins * days_in_time_period) {
    stop("You can generate only 100 temporary PINs per day")
  }
  
  # Oversample a bit to make up for filtered PINs (duplicates, PINs with 
  # inadequate personal numbers) 
  rdates <- sample(start.date:end.date,
                   size = n,
                   replace = TRUE)
  
  rdates <- as.Date(rdates, origin = "1970-01-01")
  
  dates_table <- table(rdates)
  # names(dates_table) <- format(as.Date(names(dates_table)), "%d%m%y")
  
  # odd numbers for males
  x1 <- (2:899)[2:899 %% 2 != 0]
  # even numbers for females
  x2 <- (2:899)[2:899 %% 2 == 0]
  
  x1 <- formatC(x1, width = 3, format = "d", flag = "0")
  x2 <- formatC(x2, width = 3, format = "d", flag = "0")
  
  prob_x1 <- rep(p.male, length(x1))
  prob_x2 <- rep(1-p.male, length(x2))
  
  if (.Platform$OS.type == "windows") {
    num_cores <- 1
  } else {
    num_cores <- detectCores()
  }

  p_nums <- unlist(
    mclapply(X = dates_table, 
           FUN = function(x) sample(c(x1, x2), 
                                    size = x, 
                                    prob = c(prob_x1, prob_x2)
                                    ),
           mc.cores = num_cores
           )
    )

  ddmmyyyy <- rep(names(dates_table), times = dates_table)

  century <- lapply(X = ddmmyyyy, 
                    FUN = function(y) switch(substr(y, 1, 2), 
                                             "20" = "A",
                                             "19" = "-",
                                             "18" = "+",
                                             stop("Invalid input")))

  ddmmyy <- format(as.Date(ddmmyyyy), "%d%m%y")
  
  incomplete_pins <- paste0(ddmmyy, century, p_nums)
  control_chars <- hetu_control_char(pin = incomplete_pins, with.century = TRUE)
  paste0(incomplete_pins, control_chars)

}

#' @rdname rpin
#' @examples 
#' x <- rhetu(3)
#' x
#' @export
rhetu <- rpin

#' @title Generate a vector of random Finnish Business ID's (y-tunnus)
#' 
#' @description 
#' A function that generates random Finnish Business ID's, 
#'    \code{bid}-numbers (Y-tunnus). 
#' 
#' @param 
#' n number of generated BIDs
#' 
#' @return a vector of generated \code{BID}-numbers.
#' 
#' @examples 
#' x <- rbid(3)
#' bid_ctrl(x)
#' @export
rbid <- function(n) {
  
  # produce a slightly larger sample to make up for removed BIDs 
  x <- ceiling(n * 1.5)
  
  numbers <- sample(0:9, size = x * 7, replace = TRUE)
  
  matrix <- matrix(numbers, ncol = 7, byrow = TRUE)
  
  bid_frame <- as.data.frame(matrix)
  
  # transpose matrix to perform row-wise multiplication
  bid_frame$check <- rowSums(t(t(bid_frame) * c(7,9,10,5,8,4,2))) %% 11
  
  # as a result of this, only checknums 0-9 should remain
  bid_frame$check <- ifelse(test = bid_frame$check %in% c(2:10), 
                            yes = (11 - bid_frame$check), 
                            no = bid_frame$check)
  
  # this removes BIDs with invalid checknum 1
  bid_frame$valid.bid <- ifelse(bid_frame$check == 1, FALSE, TRUE)
  
  # choose only BIDs with valid checksum
  bid_frame <- bid_frame[bid_frame$valid.bid,]
  
  # Produce a vector of finalized BIDs
  bids <- rep(NA, nrow(bid_frame))
  for (i in seq_len(nrow(bid_frame))) {
    bids[i] <- paste0(paste(bid_frame[i,1:7], collapse = ''),
                      "-", 
                      paste0(bid_frame[i,8], collapse = ''))
    next
  }
  
  # since the sample is probably larger than needed, take only n number of BIDs
  # in the rare case of getting less samples than needed, allow replacing
  if (length(bids) < n) {
    sample(bids, size = n, replace = TRUE)
  } else {
    sample(bids, size = n, replace = FALSE)
  }
  
}