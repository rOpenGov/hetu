#' @title Finnish Personal Identification Number Validator
#' @description Validate Finnish personal identification numbers (hetu).
#' @param pin Finnish personal identification number as a character vector, or
#'   vector of identification numbers as a character vectors.
#' @param allow.temp If TRUE, temporary PINs (personal numbers 900-999) are handled
#'  similarly to regular PINs (personal numbers 002-899), meaning that otherwise valid
#'  temporary PIN will return a TRUE. Default is \code{FALSE}.
#' @return Logical indicating whether the input string is a valid Finnish personal identification number,
#' @author Pyry Kantanen
#' @seealso \code{\link{hetu}} For extracting information from Finnish personal
#'   identification numbers.
#' @examples
#' pin_ctrl("010101-0101") # TRUE
#' pin_ctrl("010101-010A") # FALSE
#' @export
pin_ctrl <- function(pin, allow.temp = FALSE) {

  return(hetu(pin, extract = "valid.pin", allow.temp = allow.temp))
  
}

#' @rdname pin_ctrl
#' @examples
#' hetu_ctrl("010101-0101") # TRUE
#' hetu_ctrl("010101-010A") # FALSE
#' @export
hetu_ctrl <- pin_ctrl

#' @title Check Finnish Business ID (y-tunnus) validity
#' 
#' @description 
#' A function that checks whether a \code{bid} (Finnish Business ID) is valid. 
#' Returns \code{TRUE} or \code{FALSE}.
#' 
#' @param 
#' bid a vector of 1 or more business identity numbers
#' 
#' @examples 
#' bid_ctrl(c("0737546-2", "1572860-0")) # TRUE TRUE
#' bid_ctrl("0737546-1") # FALSE
#' @export
bid_ctrl <- function(bid) {
  
  # Try to create Business ID -object from the given bid, check if created object 
  # is of the correct class 
  if (length(bid) > 1) {
    return(sapply(bid, FUN=bid_ctrl, USE.NAMES = FALSE))
  }
  
  if(!is.character(bid)) {bid <- as.character(bid)}
  
  # Check bid number of characters
  if (nchar(bid) != 9) {
    warning(paste("Invalid number of characters in Business ID", bid))
    valid.length <- FALSE
    return(valid.length)
  } else {valid.length <- TRUE}
  
  # Check separator character
  dash <- substr(bid, start=8, stop=8)
  if (!dash %in% c("-")) {
    warning(paste0("Invalid separator character '", dash, "' in Business ID ", bid))
    valid.separator <- FALSE
    return(valid.separator)
  } else {valid.separator <- TRUE}
  
  # Calculate if BID is correct using Mod 11-2
  x <- substr(bid, start = 1, stop = 7)
  x_control <- substr(bid, start = 9, stop = 9)
  x <- as.numeric(unlist(strsplit(x, split="")))
  x <- x * c(7,9,10,5,8,4,2)
  x <- sum(x)
  check <- x %% 11
  if (check == 0) {
    check <- check
  } else if (check %in% c(2:10)) {
      check <- 11 - check
  } else {
    check <- FALSE
    return(check)
  }
  check <- check == x_control
  return(check)
}