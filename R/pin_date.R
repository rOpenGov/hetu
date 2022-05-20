#' @title Extract Date of Birth from Personal Identity Code
#' @description Returns the date of birth in date format.
#' @inheritParams hetu
#' @return Date of birth as a vector in date format.
#' @examples
#' pin_date(c("010101-0101", "111111-111C"))
#'
#' @export
pin_date <- function(pin, allow.temp = FALSE) {
  hetu(pin, extract = "date", allow.temp = allow.temp)
}

#' @rdname pin_date
#' @examples
#' hetu_date(c("010101-0101", "111111-111C"))
#' @export
hetu_date <- pin_date
