#' @title Finnish personal identification number validator
#' @description Validate Finnish personal identification numbers (hetu).
#' @param pin Finnish personal identification number as a character vector, or
#'   vector of identification numbers as a character vectors.
#' @param allow.temp Allow temporary PINs (personal numbers 900-999) or only
#'   regular PINs (personal numbers 002-899). Default is \code{FALSE}.
#' @return Is the given string a valid Finnish personal identification number,
#'   \code{TRUE} or \code{FALSE}.
#' @author Jussi Paananen
#' @seealso \code{\link{hetu}} For extracting information from Finnish personal
#'   identification numbers.
#' @examples
#' pin_ctrl("010101-0101") # TRUE
#' pin_ctrl("010101-010A") # FALSE
#' @aliases pin_ctrl
#' @export
pin_ctrl <- function(pin, allow.temp = FALSE) {

  # Try to create hetu-object from the given pin, check if created object 
  # is of the correct class 
  if (length(pin) > 1) {
    return(sapply(pin, FUN=pin_ctrl))
  }

  return(class(hetu(pin, allow.temp = allow.temp)) == "data.frame")
}


