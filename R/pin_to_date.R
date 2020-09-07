## pin_to_date.R
#' @title ID to Date.
#' @description Calculates the date of birth in date format.
#' @param pin Finnish ID number
#' @return Date of birth as a vector in date format.
#' 
#' @name pin_to_date-deprecated
#' @usage pin_to_date(pin)
#' @seealso \code{\link{hetu-deprecated}}
#' @keywords internal
NULL

#' @rdname hetu-deprecated
#' @section \code{pin_to_date}:
#' For \code{pin_to_date}, use \code{\link{pin_date}}.
#' 
#' @export
pin_to_date <- function(pin) {
  .Deprecated(new = "pin_date", package = "hetu")
  lubridate::ymd(hetu(pin, extract = "date", allow.temp = TRUE))
}
