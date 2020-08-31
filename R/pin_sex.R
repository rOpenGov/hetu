#' @title Sex from ID
#' @description Extract sex from Finnish personal identification number.
#' @inheritParams hetu 
#' @return Factor with label 'Male' and 'Female'.
#' @author Leo Lahti \email{leo.lahti@iki.fi}
#' @seealso \code{\link{hetu}} For general information extraction
#' @examples
#' pin_sex("010101-010A") 
#' @export 
pin_sex <- function(pin) {

    sex <- hetu(pin, extract = "gender", allow.temp = TRUE)
    output <- factor(sex, levels = c("Female", "Male"))
    return(output)
  
}



