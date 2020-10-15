#' @title Generate a vector of random \code{hetu}
#'
#' @description 
#' A function that generates random \code{hetu}-pins. 
#'
#' @param n number of generated \code{hetu}-pins
#' @param start_date Lower limit of generated \code{hetu} dates. Default is 1895-01-01.
#' @param end_date Upper limit of generated \code{hetu}. Default is the current date.
#' @param p.male Proportion of males. Default is 0.4.
#' @param p.temp Proportion of temporary identification numbers. Default is 0.0.
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
#' @export
rpin <- function(n, start_date = as.Date("1895-01-01"), end_date = as.Date(Sys.Date()), p.male = 0.4, p.temp = 0.0){
  # Oversample a bit to make up for filtered PINs (duplicates, PINs with inadequate personal numbers) 
  n_sample <- ceiling(n * 1.1)
  
  rdate <- sample(as.Date(start_date):as.Date(end_date), n_sample, replace = TRUE)
  
  # origin date according to POSIX standard
  rdate <- as.Date(rdate, origin = "1970-01-01")
  
  # DDMMYY in DDMMYYCZZZQ
  ddmmyy <- format(rdate, "%d%m%y")
  
  # Determine the correct century marker (C in DDMMYYCZZZQ)
  century <- function(x) {
    switch(substr(x, 1, 2),
         "20" = "A",
         "19" = "-",
         "18" = "+",
    )
  }
  century.char <- sapply(rdate, century, USE.NAMES = FALSE)
  
  # Generate the personal number part of hetu (ZZZ in DDMMYYCZZZQ)
  zz_norm <- sample(x = 0:89, replace = TRUE, size = round(n_sample*(1-p.temp)))
  zz_temp <- sample(x = 90:99, replace = TRUE, size = round(n_sample*p.temp))
  zz <- append(zz_norm, zz_temp)
  #randomize order of pins to make the vector seem more natural
  # zz <- sample(zz)
  zz <- formatC(zz, width = 2, format = "d", flag = "0")
  z <- sample(x = as.character(c(0, 2, 4, 6, 8, 1, 3, 5, 7, 9)), 
              prob = c(rep(1 - p.male, 5), 
              rep(p.male, 5)), 
              replace = TRUE, 
              size = n_sample)

  # Allowed characters used for determining the checksum of hetu
  checklist <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", 
                 "A", "B", "C", "D", "E", "F", "H", "J", "K", "L", 
                 "M", "N", "P", "R", "S", "T", "U", "V", "W", "X", "Y")
  names(checklist) <- 0:30
  
  # Determine the checksum part of hetu (Q in DDMMYYCZZZQ)
  checksum <- checklist[as.character(as.numeric(paste0(ddmmyy, zz, z)) %% 31)]
  
  pins <- paste0(ddmmyy, century.char, zz, z, checksum)
  # Remove duplicates
  pins <- pins[!duplicated(pins)]
  
  # Remove pins with 000 and 001 in personal number
  pins <- pins[!substr(pins, 8, 10) == "000"]
  pins <- pins[!substr(pins, 8, 10) == "001"]
  
  # Final product
  # Select a subsample of desired size from a slightly larger sample 
  pins <- sample(pins, n)
  pins
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
#' A function that generates random Finnish Business ID's, \code{bid}-numbers (Y-tunnus). 
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
  for (i in 1:nrow(bid_frame)) {
    bids[i] <- paste0(paste(bid_frame[i,1:7], collapse = ''), "-", paste0(bid_frame[i,8], collapse = ''))
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
  
  
  