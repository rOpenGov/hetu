#' @title Finnish Personal Identification Number Validator
#' @description Validate Finnish personal identification numbers (hetu).
#' @param pin Finnish personal identification number as a character vector, or
#'   vector of identification numbers as a character vectors.
#' @param allow.temp If TRUE, temporary PINs (personal numbers 900-999) are handled
#'  similarly to regular PINs (personal numbers 002-899), meaning that otherwise valid
#'  temporary PIN will return a TRUE. Default is \code{FALSE}.
#' @return Logical indicating whether the input string is a valid Finnish personal identification number,
#' @author Jussi Paananen
#' @seealso \code{\link{hetu}} For extracting information from Finnish personal
#'   identification numbers.
#' @examples
#' pin_ctrl("010101-0101") # TRUE
#' pin_ctrl("010101-010A") # FALSE
#' @export
pin_ctrl <- function(pin, allow.temp = FALSE) {

  # Try to create hetu-object from the given pin, check if created object 
  # is of the correct class 
  if (length(pin) > 1) {
    return(sapply(pin, FUN=pin_ctrl, allow.temp = allow.temp))
  }

  return(class(hetu(pin, allow.temp = allow.temp)) == "data.frame")
}

#' @rdname pin_ctrl
#' @examples
#' hetu_ctrl("010101-0101") # TRUE
#' hetu_ctrl("010101-010A") # FALSE
#' @export
hetu_ctrl <- pin_ctrl
