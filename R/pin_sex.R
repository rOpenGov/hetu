#' @title Sex From ID
#' @description Extract sex from Finnish personal identification number.
#' @inheritParams hetu 
#' @return Factor with label 'Male' and 'Female'.
#' @author Leo Lahti, Pyry Kantanen
#' @seealso \code{\link{hetu}} For general information extraction
#' @examples
#' pin_sex("010101-010A") 
#' @export 
pin_sex <- function(pin) {

    sex <- hetu(pin, extract = "sex", allow.temp = TRUE)
    output <- factor(sex, levels = c("Female", "Male"))
    return(output)
  
}

#' @rdname pin_sex
#' @examples
#' hetu_sex("010101-010A") 
#' @export
hetu_sex <- pin_sex

