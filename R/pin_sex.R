#' @title Sex From ID
#' @description Extract sex from Finnish personal identification number.
#' @inheritParams hetu 
#' @return Factor with label 'Male' and 'Female'.
#' @author Pyry Kantanen, Leo Lahti
#' @seealso \code{\link{hetu}} For general information extraction
#' @examples
#' pin_sex("010101-010A") 
#' @export 
pin_sex <- function(pin, allow.temp = TRUE) {
  
  return(hetu(pin, extract = "sex", allow.temp = allow.temp))

# Alternative way without relying on hetu extract:
#  sex_marker <- as.numeric(substr(pin, start = 10, stop = 10))
#  sex <- ifelse(((sex_marker %% 2) == 0), "Female", "Male")
#  output <- factor(sex, levels = c("Female", "Male"))
#  return(output)

}

#' @rdname pin_sex
#' @examples
#' hetu_sex("010101-010A") 
#' @export
hetu_sex <- pin_sex

