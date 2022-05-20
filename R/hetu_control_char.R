#' @title Calculate Control Character for Personal Identity Code
#' @description Calculate a valid control character for an incomplete
#'    Finnish personal identity codes (hetu).
#' @param pin An incomplete PIN that ONLY has a date, century marker (optional,
#'    see parameter with.century) and personal number
#' @param with.century If TRUE (default), the function assumes that the PIN
#'    input contains a century marker (DDMMYYQZZZ). If FALSE, the function
#'    assumes that the PIN contains only date and personal number (DDMMYYZZZ).
#' @details This method of calculating the control character was devised by
#'    mathematician Erkki Pale (1962) to detect input errors but also to
#'    detect errors produced by early punch card machines. The long number
#'    produced by writing the birth date and the personal number together are
#'    divided by 31 and the remainder is used to look up the control character
#'    from a separate table containing alphanumeric characters except letters
#'    G, I, O, Q and Z.
#'
#'    The method of calculating the control character does not need century
#'    character and therefore the function has an option to omit it.
#' @return Control character, either a number 0-9 or a letter.
#' @author Pyry Kantanen
#' @seealso \code{\link{hetu}} For extracting information from Finnish personal
#'    identity codes.
#' @examples
#' hetu_control_char("010101-010")
#' hetu_control_char("010101010", with.century = FALSE)
#' @export
hetu_control_char <- function(pin, with.century = TRUE) {

  if (length(pin) > 1) {
    x <- vapply(pin,
                FUN = hetu_control_char,
                with.century = with.century,
                FUN.VALUE = character(1),
                USE.NAMES = FALSE)
    return(x)
  }

  checklist <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
                 "A", "B", "C", "D", "E", "F", "H", "J", "K", "L",
                 "M", "N", "P", "R", "S", "T", "U", "V", "W", "X", "Y")
  names(checklist) <- 0:30

  if (with.century == TRUE) {
    if (nchar(pin) != 10) {
      stop("Input PINs that only have 10 characters: birthdate, century marker
and personal numbers (DDMMYYQZZZ)")
    }
    if (!(substr(pin, start = 7, stop = 7) %in% c("-", "+", "A"))) {
      stop("7th character of your PIN needs to be a century marker (-, + or A).
If your PIN does not have it use parameter with.century == FALSE")
    }
    pin_ddmmyy <- substr(pin, 1, 6)
    pin_zzz <- substr(pin, 8, 10)
  } else if (with.century == FALSE) {
    if (nchar(pin) != 9) {
      stop("Input PINs that only have 9 characters: birthdate and personal
numbers (DDMMYYZZZ)")
    }
    pin_ddmmyy <- substr(pin, 1, 6)
    pin_zzz <- substr(pin, 7, 9)
  }

  mod <- as.numeric(paste0(pin_ddmmyy, pin_zzz)) %% 31
  extracted_control_char <- checklist[as.character(mod)]
  names(extracted_control_char) <- NULL
  extracted_control_char

}

#' @title Finnish Unique Identification Number Control Character Calculator
#' @description Calculate a valid control character for an incomplete
#'    Finnish Unique Identification Number (FINUID, or sähköinen asiointitunnus
#'    SATU).
#' @param pin An incomplete FINUID that has 8 first numbers.
#' @param print.full Should the function print only the whole FINUID-number 
#' (TRUE) or only the control character (FALSE). Default is FALSE.
#' @details This method of calculating the control character was devised by
#'    mathematician Erkki Pale (1962) to detect input errors but also to
#'    detect errors produced by early punch card machines. The long number
#'    produced by writing the birth date and the personal number together are
#'    divided by 31 and the remainder is used to look up the control character
#'    from a separate table containing alphanumeric characters except letters
#'    G, I, O, Q and Z.
#'
#'    The method of calculating the control character does not need century
#'    character and therefore the function has an option to omit it.
#' @return Control character, either a number 0-9 or a letter (length 1 
#'    character). If parameter print.full is set to TRUE, the function returns
#'    a complete FINUID / SATU number (length 9 characters).
#' @seealso
#' For more detailed information about FINUID, see Finnish Digital and
#' population data services agency website:
#' \url{https://dvv.fi/en/citizen-certificate-and-electronic-identity}
#' @author Pyry Kantanen
#' @examples
#' # The first assigned FINUID number, 10000001N.
#' satu_control_char("10000001")
#' @export
satu_control_char <- function(pin, print.full = FALSE) {

  if (length(pin) > 1) {
    x <- vapply(pin,
                FUN = satu_control_char,
                print.full = print.full,
                FUN.VALUE = character(1),
                USE.NAMES = FALSE)
    return(x)
  }

  checklist <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
                 "A", "B", "C", "D", "E", "F", "H", "J", "K", "L",
                 "M", "N", "P", "R", "S", "T", "U", "V", "W", "X", "Y")
  names(checklist) <- 0:30

  if (nchar(pin) != 8) {
    stop("Input FINUIDs that have 8 numbers")
  }
  if (suppressWarnings(is.na(as.numeric(pin))) == TRUE) {
    stop("Input FINUIDs that only have numbers")
  }

  mod <- as.numeric(pin) %% 31
  extracted_control_char <- checklist[as.character(mod)]
  names(extracted_control_char) <- NULL
  extracted_control_char

  if (print.full == TRUE) {
    paste0(pin, extracted_control_char)
  } else {
    extracted_control_char
  }
}
