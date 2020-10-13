#' @title Age from ID
#' @description Calculate the age in full years for a given date.
#' @inheritParams hetu
#' @param date Date at which age is calculated. If a vector is provided it must be
#'  of the same length as the \code{pin} argument.
#' @param timespan Timespan to use to calculate age. The actual timespans are:
#' \itemize{
#'   \item \code{years} (Default)
#'   \item \code{months}
#'   \item \code{weeks}
#'   \item \code{days}
#' }
#' @aliases hetu_age 
#' @return Age as an integer vector.
#'
#' @examples
#' ex_pin <- c("010101-0101", "111111-111C")
#' pin_age(ex_pin, date = "2012-01-01")
#'
#' @export
pin_age <- function(pin, date=Sys.Date(), timespan = "years", allow.temp = FALSE) {

  date <- as.Date(date)
  checkmate::assert_date(date, any.missing = FALSE)
  checkmate::assert_choice(timespan, choices = c("years", "months", "weeks", "days"))
  
  if (length(date) == 1) {
    message("The age in ", timespan, " has been calculated at ", as.character(date), 
            ".")
  } else if (length(date) == length(pin)){
    message("The age is calculated relative to the '", deparse(substitute(date)), "' date")
  } else {
    stop("Multiple dates used.")
  }
  hetuframe <- hetu(pin)
  date <- lubridate::ymd(date)

  all_pins <- pin
  all_pins[!hetuframe$valid.pin] <- NA
  if (length(date) > 1){
    valid_diff <- !is.na(all_pins) & !is.na(date)
  } else{
    valid_diff <- !is.na(all_pins)
  }
  pin <- all_pins[valid_diff]
  
  pin_dates <- as.Date(hetuframe$date[valid_diff])
  
  diff <- lubridate::interval(pin_dates, date)

  timespan_lubridate <-
    switch(timespan,
           "years" = lubridate::years(1),
           "months" = lubridate::period(months=1),
           "weeks" = lubridate::weeks(1),
           "days" = lubridate::days(1))
  
  age <- suppressMessages(as.integer(diff %/% timespan_lubridate))
  if(any(date < pin_dates)) warning("Negative age(s).")
  
  all_age <- rep(as.integer(NA), length(all_pins))
  all_age[valid_diff] <- age
  all_age
  
}

#' @rdname pin_age
#' @examples
#' ex_pin <- c("010101-0101", "111111-111C")
#' hetu_age(ex_pin, date = "2012-01-01")
#' @export
hetu_age <- pin_age