## hetu_diagnostic.R
#' @title Diagnostics Tool for HETU
#' @description Produce a data frame of PINs that may require closer scrutiny.
#' @param pin Finnish personal identification number as a character vector, 
#' 	  or vector of identification numbers as a character vectors
#' @param extract Extract only selected part of the diagnostic information.
#'   Valid values are "\code{hetu}", "\code{is.temp}", "\code{valid.p.num}",
#'   "\code{valid.checksum}", "\code{correct.checksum}", "\code{valid.date}",
#'   "\code{valid.day}", "\code{valid.month}", "\code{valid.length}", 
#'  "\code{valid.century}". If \code{NULL} (default), returns all information.
#' @param subsetting Print only PINs where the validity check chosen in \code{extract} returns \code{FALSE}.
#' @return A data.frame containing diagnostic checks about PINs.
#' @examples
#' diagnosis_example <- c("010101-0102", "111111-111Q", 
#' "010101B0101", "320101-0101", "011301-0101", 
#' "010101-01010", "010101-0011")
#' ## Print all diagnoses
#' hetu_diagnostic(diagnosis_example)
#' # Extract century-related checks
#' hetu_diagnostic(diagnosis_example, extract = "valid.century")
#' # Extract only rows where invalid.checksum = TRUE
#' hetu_diagnostic(diagnosis_example, subsetting = TRUE, extract = "valid.checksum") 
#'
#' @export
hetu_diagnostic <- function(pin, extract = NULL, subsetting = FALSE) {
  
  diagnostic_params <- c("hetu", "is.temp", "valid.p.num", "valid.checksum", 
            "correct.checksum", "valid.date", "valid.day", "valid.month", 
            "valid.year", "valid.length", "valid.century")
  
  if (!is.null(extract)) {
    if (!all(extract %in% diagnostic_params)) {
      stop("Trying to extract invalid diagnostic(s)")
    }
  }
  
  if (is.null(extract)) {
    output <- subset(hetu(pin, allow.temp = TRUE, diagnostic = TRUE), select = diagnostic_params)
  } else {
      if (subsetting == TRUE) {
        output <- hetu(pin, allow.temp = TRUE, diagnostic = TRUE)
        output <- dplyr::filter(output, eval(parse(text = paste(extract, "== FALSE"))))
      }
      else {
        output <- subset(hetu(pin, allow.temp = TRUE, diagnostic = TRUE), select = c("hetu", extract))
      }
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