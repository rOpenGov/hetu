#' @title Extract Sex from Personal Identity Code
#' @description Extract sex (as binary) from Finnish personal identification 
#'    code.
#' @inheritParams hetu
#' @return Factor with label 'Male' and 'Female'.
#' @author Pyry Kantanen, Leo Lahti
#' @seealso \code{\link{hetu}} For general information extraction
#' @examples
#' pin_sex("010101-010A")
#' @export
pin_sex <- function(pin, allow.temp = TRUE) {

  return(hetu(pin, extract = "sex", allow.temp = allow.temp))

}

#' @rdname pin_sex
#' @examples
#' hetu_sex("010101-010A")
#' @export
hetu_sex <- pin_sex
