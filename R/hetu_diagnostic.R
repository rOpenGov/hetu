## hetu_diagnostic.R
#' @title Diagnostics Tool for Personal Identity Codes
#' @description Prints information on the tests that are used 
#'    to confirm or reject the validity of each personal identity code.
#' @param pin Finnish personal identification number as a character vector,
#'    or vector of identification numbers as a character vectors
#' @param extract Extract only selected part of the diagnostic information.
#'    Valid values are "\code{hetu}", "\code{is.temp}", "\code{valid.p.num}",
#'    "\code{valid.ctrl.char}", "\code{correct.ctrl.char}", "\code{valid.date}",
#'    "\code{valid.day}", "\code{valid.month}", "\code{valid.length}",
#'    "\code{valid.century}". If \code{NULL} (default), returns all information.
#' @return A data.frame containing diagnostic checks about PINs.
#' @examples
#' diagnosis_example <- c("010101-0102", "111111-111Q",
#' "010101B0101", "320101-0101", "011301-0101",
#' "010101-01010", "010101-0011")
#' ## Print all diagnostics for various fake personal identity codes
#' hetu_diagnostic(diagnosis_example)
#' # Extract century-related checks
#' hetu_diagnostic(diagnosis_example, extract = "valid.century")
#' @seealso \code{\link{hetu}} for the main function on which 
#'    \code{hetu_diagnostic} relies on.
#'
#' @export
hetu_diagnostic <- function(pin, extract = NULL) {

  diagnostic_params <- c("hetu", "is.temp", "valid.p.num", "valid.ctrl.char",
            "correct.ctrl.char", "valid.date", "valid.day", "valid.month",
            "valid.year", "valid.length", "valid.century")

  if (!is.null(extract)) {
    if (!all(extract %in% diagnostic_params)) {
      stop("Trying to extract invalid diagnostic(s)")
    }
  }

  diagnostic_table <- hetu(pin, allow.temp = TRUE, diagnostic = TRUE)

  if (is.null(extract)) {
    output <- diagnostic_table[, diagnostic_params]
  } else {
    output <- diagnostic_table[, c("hetu", extract)]
  }
  return(output)
}

#' @rdname hetu_diagnostic
#' @examples
#' diagnosis_example <- c("010101-0102", "111111-111Q",
#' "010101B0101", "320101-0101", "011301-0101",
#' "010101-01010", "010101-0011")
#' ## Print all diagnoses
#' pin_diagnostic(diagnosis_example)
#' @export
pin_diagnostic <- hetu_diagnostic
