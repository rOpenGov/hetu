#' @title Generic Extraction Tool for Finnish Personal Identity Codes
#' @description Extract embedded information from Finnish personal identity
#'    codes (hetu).
#' @param pin Finnish personal identity code(s) as a character vector
#' @param extract Extract only selected part of the information.
#'    Valid values are "\code{hetu}", "\code{sex}", "\code{p.num}",
#'    "\code{ctrl.char}", "\code{date}", "\code{day}", "\code{month}",
#'    "\code{year}", "\code{century}", "\code{is.temp}".
#'    If \code{NULL} (default), returns all information.
#' @param allow.temp Allow artificial or temporary PINs (personal numbers
#'    900-999). If \code{FALSE} (default), only PINs intended for official
#'    use (personal numbers 002-899) are allowed.
#' @param diagnostic Print additional information about possible problems in
#'    PINs. The checks are "\code{valid.p.num}", "\code{valid.ctrl.char}",
#'    "\code{correct.ctrl.char}", "\code{valid.date}", "\code{valid.day}",
#'    "\code{valid.month}", "\code{valid.length}", "\code{valid.century}".
#'    Default is \code{FALSE} which returns no diagnostic information.
#' @param as.factor Makes fields "\code{sex}", "\code{p.num}",
#' "\code{ctrl.char}" and "\code{century}" into factors for slightly reduced
#' memory footprint. Default is FALSE.
#' @return Finnish personal identity code data.frame,
#'     or if extract parameter is set, the requested part of the
#'	   information as a vector. Returns an error or \code{NA} if the given
#'	   character vector is not a valid Finnish personal identity code.
#' \item{hetu}{Finnish personal identity code as a character vector.
#'     A correct pin should be in the form DDMMYYCZZZQ, where DDMMYY stands for
#'     date, C for century sign, ZZZ for personal number and Q for control
#'     character.}
#' \item{sex}{sex of the person as a character vector ("Male" or "Female")}
#' \item{p.num}{Personal number (individual number) part of the identity code}
#' \item{ctrl.char}{Control character for the personal identity code}
#' \item{date}{Birthdate}
#' \item{day}{Day of the birthdate}
#' \item{month}{Month of the birthdate}
#' \item{year}{Year of the birthdate}
#' \item{century}{Century character determining the century (1800s, 1900s or
#' 2000s) of the person's birth. See details for more information}
#' \item{valid.pin}{Does the personal identity code pass all validity
#'    checks: (\code{TRUE} or \code{FALSE})}
#'
#' @details
#'
#' Starting from 1st of January 2023, an amendment to the government decree on
#' the Population Information System (128/2010) has expanded the
#' number of available century markers (See references: Valtioneuvoston asetus
#' VM/2022/124) and scrapped some old practices.
#'
#' For the users of this package the most visible change will be that
#' people born in the 1900s can now be assigned with "Y", "X", "W", "V" or "U",
#' in addition to the old "-" (slash) marker. People born in the 2000s can be
#' assigned with "B", "C", "D", E" or "F", in addition to the old marker, "A".
#' For people born in the 1800s "+" (plus sign) remains the only valid marker.
#' The amendment does not affect already existing personal identity codes.
#'
#' The change was done to mitigate for the diminishing pool of available, unique
#' identity codes. For historical reasons, the century marker of
#' the code was not always taken into account when determining the uniqueness
#' of the number. This meant that individual number parts were not recycled
#' between people born in different centuries, diminishing the amount of
#' available numbers for people born in the new century.
#' For example, if a female born in the 1st of January 1901
#' was assigned with the personal identity code "010101-0101" (individual code
#' part "010"), a female born in 1st of January 2001 could not be assigned with
#' the code "010101A0101" because it would contain the same individual code
#' as the person born in 1901 and individual codes could not be recycled. With
#' the amended decree the uniqueness of the personal identity code is considered
#' by looking at the personal identity code as a whole. This means that from now
#' on it would be permissible to have personal identity codes such as
#' "100190-999P" and "100190Y999P" at the same time, denoting two different
#' individuals (see references: Digital and population data services agency
#' announcement).
#'
#' In practice, codes with new separators will be issued only when the ranges
#' ranges with currently used separators run out. This means that it might
#' take a while until we see people born in the 2000s assigned with the century
#' marker "C" or people born in the 1900s assigned with the century marker "X",
#' as there are still plenty of numbers in ranges "B" and "Y" as well, in
#' addition to some numbers being left in the original ranges of "A" and "-".
#' The first personal identity code with a new separator "Y" was assigned
#' in December 2023 (see Digi- ja väestötietovirasto 2023).
#'
#' The result of all this is that the hetu package may now give "unrealistic"
#' personal identity codes in the sense that some codes are not yet actually
#' in use. However, it is not the aim of this package to simulate the
#' actual distributions of personal identity codes and their century markers in
#' the population (the actually used and unused codes are unknown to us),
#' but to provide a tool that can be used to extract data from these codes,
#' should the user encounter them at some point. Writing further sanity checks
#' is probably a good idea for people who are interested in detecting unusual
#' patterns in their databases and registries.
#'
#' @references
#'
#' Valtioneuvoston asetus VM/2022/124 \href{https://vm.fi/paatos?decisionId=0900908f807c5f3c}{Valtioneuvoston asetus VM/2022/124}
#'
#' Digi- ja väestötietovirasto. (2023). \href{https://dvv.fi/-/uudet-valimerkit-takaavat-henkilotunnusten-riittavyyden-ensimmainen-uudenlainen-henkilotunnus-myonnettiin-talla-viikolla}{Uudet välimerkit takaavat henkilötunnusten riittävyyden - ensimmäinen uudenlainen henkilötunnus myönnettiin tällä viikolla}
#'
#' Digital and Population Data Services Agency. \href{https://dvv.fi/en/reform-of-personal-identity-code}{Reform of the separators in the personal identity code}
#'
#' @author Pyry Kantanen, Jussi Paananen
#' @seealso
#' \code{\link{pin_ctrl}} Validating Finnish personal identity codes.
#' \code{\link{rhetu}} Generating random Finnish personal identity codes.
#' @examples
#' hetu("111111-111C")
#' hetu("111111-111C")$date
#' hetu("111111-111C")$sex
#' # Same as previous, but using extract argument
#' hetu("111111-111C", extract="sex")
#' # Process a vector of hetu's
#' hetu(c("010101-0101", "111111-111C"))
#' # Process a vector of hetu's and extract sex information from each
#' hetu(c("010101-0101", "111111-111C"), extract="sex")
#' # Process codes with new century markers
#' new_codes <- c("010594Y9032", "010594Y9021", "020594X903P")
#' hetu(new_codes)
#'
#' @importFrom checkmate assert_choice
#'
#' @export
hetu <- function(pin, extract = NULL, allow.temp = FALSE, diagnostic = FALSE,
                 as.factor = FALSE) {

  if (!is.null(extract)) {
    valid_choices <- c("hetu", "sex", "p.num", "ctrl.char",
                       "date", "day", "month", "year", "century",
                       "valid.pin")
    if (allow.temp == FALSE) {
      checkmate::assert_choice(extract, valid_choices)
    } else if (allow.temp == TRUE) {
      valid_choices <- c(valid_choices, "is.temp")
      checkmate::assert_choice(extract, valid_choices)
    }
  }

  # Convert to character vector if necessary
  if (!is.character(pin)) {
    pin <- as.character(pin)
  }

  # Check day
  extracted_day <- as.numeric(substr(pin, start = 1, stop = 2))
  valid_day_test <- ((extracted_day >= 1) & (extracted_day <= 31))
  extracted_day[!valid_day_test] <- NA

  # Check month
  extracted_month <- as.numeric(substr(pin, start = 3, stop = 4))
  valid_month_test <- ((extracted_month >= 1) & (extracted_month <= 12))
  extracted_month[!valid_month_test] <- NA

  # Check year
  year <- as.numeric(substr(pin, start = 5, stop = 6))
  valid_year_test <- ((year >= 0) & (year <= 99))

  # Construct complete year from century character and 2-digit year
  ## Pad leading zero to a 2-digit year if needed
  year <- formatC(year, flag = 0, width = 2)
  year[!valid_year_test] <- NA

  # Check century
  extracted_century_marker <- substr(pin, start = 7, stop = 7)
  valid_century_test <- extracted_century_marker %in% c("+", "-", "A", "B", "C",
                                                        "D", "E", "F", "Y", "X",
                                                        "W", "V", "U")

  # Construct a full year based on century marker
  full_year_function <- function(pin) {
    extracted_century_marker <- substr(pin, start = 7, stop = 7)
    year <- as.character(substr(pin, start = 5, stop = 6))
    if (extracted_century_marker %in% c("+")) {
      res <- paste0("18", year)
    } else if (extracted_century_marker %in% c("-", "Y", "X", "W", "V", "U")) {
      res <- paste0("19", year)
    } else if (extracted_century_marker %in% c("A", "B", "C", "D", "E", "F")) {
      res <- paste0("20", year)
    } else {
      res <- NA
    }
    as.numeric(res)
  }
  # full_year_function takes 1 pin at a time so using vapply
  full_year <- vapply(pin,
                      FUN = full_year_function,
                      FUN.VALUE = double(1),
                      USE.NAMES = FALSE)

  # Check if date exists
  extracted_date <- as.Date(paste(extracted_day, "/",
                                  extracted_month, "/",
                                  full_year, sep = ""), "%d/%m/%Y")
  extracted_date[is.na(extracted_date)] <- NA
  valid_date_test <- !is.na(extracted_date)

  # Check if control character is valid
  extracted_ctrl_char <- substr(pin, start = 11, stop = 11)
  checklist <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
                 "A", "B", "C", "D", "E", "F", "H", "J", "K", "L",
                 "M", "N", "P", "R", "S", "T", "U", "V", "W", "X", "Y")
  names(checklist) <- 0:30
  valid_ctrl_char_test <- extracted_ctrl_char %in% checklist

  # Get personal identity code
  extracted_personal_number <- substr(pin, start = 8, stop = 10)
  extracted_personal_number <- formatC(extracted_personal_number,
                                       width = 3, format = "d", flag = "0")
  valid_p_num_test <- (as.numeric(extracted_personal_number) >= 2)

  # Check if control character is correct
  mod <- as.numeric(paste0(substr(pin, start = 1, stop = 6),
                           substr(pin, start = 8, stop = 10))) %% 31
  ctrl_char_test <- extracted_ctrl_char == checklist[as.character(mod)]
  names(ctrl_char_test) <- NULL
  ctrl_char_test[is.na(ctrl_char_test)] <- FALSE

  # Check sex
  extracted_sex <- ifelse(((as.numeric(extracted_personal_number) %% 2) == 0),
                          "Female", "Male")

  # Check if personal identity code is artificial or temporary
  is_temp_test <- as.numeric(extracted_personal_number) >= 900

  # Check pin number of characters
  valid_length_test <- (nchar(pin) == 11)

  # Produce a logical test value for overall validity of PIN
  test_matrix <- cbind(valid_p_num_test, valid_ctrl_char_test, ctrl_char_test,
                       valid_date_test, valid_day_test, valid_month_test,
                       valid_year_test, valid_length_test, valid_century_test)
  valid_pin_test <- apply(test_matrix, 1, all)

  # Create hetu-object
  if (as.factor == TRUE) {
    object <- list(hetu = pin,
                   sex = as.factor(extracted_sex),
                   p.num = as.factor(extracted_personal_number),
                   ctrl.char = as.factor(extracted_ctrl_char),
                   date = extracted_date,
                   day = extracted_day,
                   month = extracted_month,
                   year = full_year,
                   century = as.factor(extracted_century_marker),
                   valid.pin = valid_pin_test)
  } else {
    object <- list(hetu = pin,
                   sex = extracted_sex,
                   p.num = extracted_personal_number,
                   ctrl.char = extracted_ctrl_char,
                   date = extracted_date,
                   day = extracted_day,
                   month = extracted_month,
                   year = full_year,
                   century = extracted_century_marker,
                   valid.pin = valid_pin_test)
  }

  if (diagnostic == TRUE) {
    # create hetu-object with diagnostics
    diagnostic_list <- list(valid.p.num = valid_p_num_test,
                            valid.ctrl.char = valid_ctrl_char_test,
                            correct.ctrl.char = ctrl_char_test,
                            valid.date = valid_date_test,
                            valid.day = valid_day_test,
                            valid.month = valid_month_test,
                            valid.year = valid_year_test,
                            valid.length = valid_length_test,
                            valid.century = valid_century_test)
    object <- append(object, diagnostic_list)
  }

  # Return full object or only requested part
  # First produce a dataframe that leaves out temp pins
  # if they are not explicitly allowed

  if (allow.temp == FALSE) {
    if (is.null(extract)) {
      object <- subset(quickdf(object), is_temp_test == FALSE)
      #Remove temporary PINs
      if (dim(object)[1] == 0) {
        return(NA) #If all PINs were temporary, return NA
      } else {
        #If there were at least some allowed pins, return data frame
        return(object)
      }
    } else {
      object <- subset(quickdf(object), is_temp_test == FALSE)
      if (dim(object)[1] == 0) {
        return(NA)
      } else {
        return(unname(do.call("c", object[extract])))
      }
    }
  } else if (allow.temp == TRUE) {
    # If temporary PINs are allowed, print the whole data frame normally
    # create separate diagnostics column for is.temp, append it to object
    temp_diag <- list(is.temp = is_temp_test)
    object <- append(object, temp_diag)
    if (is.null(extract)) {
      return(quickdf(object))
    } else {
      return(unname(do.call("c", object[extract])))
    }
  }
}
