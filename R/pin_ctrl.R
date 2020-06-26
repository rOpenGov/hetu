#' @title Finnish personal identification number validator
#' @description Validate Finnish personal identification numbers (hetu).
#' @param hetu Finnish personal identification number as a character vector, 
#' 	  or vector of identification numbers as a character vectors.
#' @return Is the given string a valid Finnish personal identification number, 
#' 	   \code{TRUE} or \code{FALSE}.
#' @author Jussi Paananen
#' @seealso \code{\link{hetu}} For extracting information from Finnish 
#' 	    personal identification numbers. 
#' @examples
#' pin_ctrl("010101-0101") # TRUE
#' pin_ctrl("010101-010A") # FALSE
#' @aliases pin_ctrl
#' @export 
pin_ctrl <- function(hetu) {

  # Try to create hetu-object from the given hetu, check if created object 
  # is of the correct class 
  if (length(hetu) > 1) {
    return(sapply(hetu, FUN=pin_ctrl))
  }

  return(class(hetu(hetu)) == "data.frame")
}


