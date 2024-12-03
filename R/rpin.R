#' @title Generate Random Personal Identity Codes
#'
#' @description
#' A function that generates random Finnish personal identity codes
#'    (\code{hetu} codes).
#'
#' @details
#' This function will return an error "too few positive probabilities" in
#' \code{\link{sample.int}} function if you try to generate too many codes
#' in a short enough timeframe. The theoretical upper limit of valid PINs is
#' in the millions, but the number of valid PINs per day used to be 898 PINs
#' at maximum, meaning 327770 for each year. Attempting to generate e.g.
#' a 1000 pins for a timespan of one day would result in an error.
#'
#' In practice this theoretical upper limit number was
#' much lower since the old practice was that the same personal number
#' component cannot be "recycled" if it has been used in the past.
#' To illustrate, if an identity code "010101-0101" has already been assigned
#' to someone born in 1901-01-01, a similar code "010101A0101" for someone
#' born in 2001-01-01 could not be used.
#'
#' In hetu package version 1.1.0 we have taken into account a new government
#' decree that increased the amount of valid century markers and therefore
#' increased the amount of valid personal codes per day. Additionally, the
#' decree has made it possible to recycle individual codes, as the century
#' marker is now thought to be a distinguishing character of the personal
#' identity code.
#'
#' However, the current implementation still keeps the old 898 codes per day
#' limit intact, and assigns new century markers with a low probability: old
#' markers "-" and "A" are given a 95 % probability of appearing and the new
#' markers are given a 1 % probability each.
#'
#' In the future this may be altered
#' into a waterfall pattern so that the initial 898 codes for each date
#' get "-" as the century marker, the next 898 get "Y", and so on.
#' This would mean that each day would have 5388 valid codes and the
#' distribution of century markers would be more
#' realistic in the sense that additional century markers are taken into use
#' only after the previous range has been exhausted. However, this would
#' require generating rather large datasets even for basic testing purposes.
#'
#' @param n number of generated \code{hetu}-pins
#' @param start.date Lower limit of generated \code{hetu} dates,
#'    character string in ISO 8601 standard, for example "2001-02-03".
#'    Default is "1895-01-01".
#' @param end.date Upper limit of generated \code{hetu}.
#'    Default is current date.
#' @param p.male Probability of males, between 0.0 and 1.0. Default is 0.4.
#' @param p.temp Probability of temporary identification numbers, between
#'    0.0 and 1.0. Default is 0.0.
#' @param num.cores The number of cores for parallel processing. The number
#'    of available cores can be determined with \code{detectCores()}.
#'    Default is 1.
#'
#' @return a vector of generated \code{hetu}-pins.
#'
#' @author Pyry Kantanen, Jussi Paananen
#'
#' @examples
#' x <- rpin(3)
#' hetu(x)
#' hetu(x, extract = "sex")
#' hetu(x, extract = "ctrl.char")
#'
#' @importFrom checkmate assert_double assert_date
#' @importFrom parallel mclapply
#'
#' @export
rpin <- function(n,
                 start.date = as.Date("1895-01-01"),
                 end.date = Sys.Date(),
                 p.male = 0.4,
                 p.temp = 0.0,
                 num.cores = 1) {

  start_date <- as.Date(start.date)
  end_date <- as.Date(end.date)

  assert_double(p.temp, 0, 1)
  assert_double(p.male, 0, 1)
  assert_date(end_date, start_date, Sys.Date())
  assert_date(start_date, as.Date("1860-01-01"), end_date)

  # Oversample a bit to make up for filtered PINs (duplicates, PINs with
  # inadequate personal numbers)
  rdates <- sample(start_date:end_date,
                   size = n,
                   replace = TRUE)

  rdates <- as.Date(rdates, origin = "1970-01-01")

  dates_table <- table(rdates)

  # odd numbers for males
  male_nums <- (2:899)[2:899 %% 2 != 0]
  male_temp <- (900:999)[900:999 %% 2 != 0]
  # even numbers for females
  female_nums <- (2:899)[2:899 %% 2 == 0]
  female_temp <- (900:999)[900:999 %% 2 == 0]

  male_nums <- formatC(male_nums, width = 3, format = "d", flag = "0")
  female_nums <- formatC(female_nums, width = 3, format = "d", flag = "0")

  prob_male <- rep((p.male * (1 - p.temp)), length(male_nums))
  prob_male_temp <- rep((p.male * p.temp), length(male_temp))
  prob_female <- rep(((1 - p.male) * (1 - p.temp)), length(female_nums))
  prob_female_temp <- rep(((1 - p.male) * p.temp), length(female_temp))

  p_nums <-
    unlist(
      mclapply(
        X = dates_table,
        FUN = function(x) {
          sample(
            c(male_nums, female_nums, male_temp, female_temp),
            size = x,
            prob = c(prob_male, prob_female, prob_male_temp, prob_female_temp)
          )
        },
        mc.cores = num.cores
      )
    )

  ddmmyyyy <- rep(names(dates_table), times = dates_table)

  century <- lapply(X = ddmmyyyy,
                    FUN = function(y) {
                      switch(substr(y, 1, 2),
                             "20" = sample(
                               c("A", "B", "C", "D", "E", "F"),
                               size = 1,
                               prob = c(0.95, 0.01, 0.01, 0.01, 0.01, 0.01)
                             ),
                             "19" = sample(
                               c("-", "Y", "X", "W", "V", "U"),
                               size = 1,
                               prob = c(0.95, 0.01, 0.01, 0.01, 0.01, 0.01)
                             ),
                             "18" = "+",
                             stop("Invalid input"))
                    })

  ddmmyy <- format(as.Date(ddmmyyyy), "%d%m%y")

  incomplete_pins <- paste0(ddmmyy, century, p_nums)
  control_chars <- hetu_control_char(pin = incomplete_pins, with.century = TRUE)
  object <- paste0(incomplete_pins, control_chars)
  return(object)

}

#' @rdname rpin
#' @examples
#' x <- rhetu(3)
#' x
#' @export
rhetu <- rpin

#' @title Generate Random Finnish Business ID's (Y-tunnus)
#'
#' @description
#' A function that generates random Finnish Business ID's, \code{bid}-numbers
#' (Y-tunnus).
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
  bid_frame$check <- rowSums(t(t(bid_frame) * c(7, 9, 10, 5, 8, 4, 2))) %% 11

  # as a result of this, only checknums 0-9 should remain
  bid_frame$check <- ifelse(test = bid_frame$check %in% c(2:10),
                            yes = (11 - bid_frame$check),
                            no = bid_frame$check)

  # this removes BIDs with invalid checknum 1
  bid_frame$valid.bid <- ifelse(bid_frame$check == 1, FALSE, TRUE)

  # choose only BIDs with valid control character
  bid_frame <- bid_frame[bid_frame$valid.bid, ]

  # Produce a vector of finalized BIDs
  bids <- rep(NA, nrow(bid_frame))
  for (i in seq_len(nrow(bid_frame))) {
    bids[i] <- paste0(paste(bid_frame[i, 1:7], collapse = ""),
                      "-",
                      paste0(bid_frame[i, 8], collapse = ""))
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
