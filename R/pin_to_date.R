#' @title ID to Date.
#' @description Calculates the date of birth in date format.
#' @inheritParams hetu
#' @return Date of birth as a vector in date format.
#' @examples
#' pin_to_date(c("010101-0101", "111111-111C"))
#' 
#' @export
pin_to_date <- function(pin) {
  lubridate::ymd(hetu(pin, extract = "date"))
}
