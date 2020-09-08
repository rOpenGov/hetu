#' @title Finnish personal identification number extraction
#' @description Extract information from Finnish personal identification numbers (hetu).
#' @param pin Finnish personal identification number as a character vector, 
#' 	  or vector of identification numbers as a character vectors
#' @param extract Extract only selected part of the information. 
#'    Valid values are "\code{hetu}", "\code{sex}", "\code{personal.number}",
#'    "\code{checksum}", "\code{date}", "\code{day}", "\code{month}", 
#'    "\code{year}", "\code{century}", "\code{is.temp}".
#'    If \code{NULL} (default), returns all information. 
#' @param allow.temp Allow artificial or temporary PINs (personal numbers 900-999). 
#'    If \code{FALSE} (default), only PINs intended for official use (personal numbers 002-899) are allowed.
#' @return Finnish personal identification number data.frame,
#'     or if extract parameter is set, the requested part of the 
#'	   information as a vector. Returns \code{NA} if the given character 
#'	   vector is not a valid Finnish personal identification number.
#' \item{hetu}{Finnish personal identification number as a character vector. 
#'     A correct pin should be in the form DDMMYYCZZZQ, where DDMMYY stands for date, C for century sign, 
#'     ZZZ for personal number and Q for checksum character.}
#' \item{sex}{sex of the person as a character vector ("Male" or "Female").}
#' \item{personal.number}{Personal number part of the identification number.}
#' \item{checksum}{Checksum for the personal identification number.}
#' \item{date}{Birthdate.}
#' \item{day}{Day of the birthdate.}
#' \item{month}{Month of the birthdate.}
#' \item{year}{Year of the birthdate.}
#' \item{century}{Century character of the birthdate: 
#'            + (1800), - (1900) or A (2000). }
#' \item{is.temp}{Is the personal identification number an artificial number intended for temporary use: (\code{TRUE} or \code{FALSE})}
#' 
#' @author Jussi Paananen \email{louhos@@googlegroups.com} 
#' @seealso \code{\link{pin_ctrl}} For validating Finnish personal 
#' 	    identification numbers.
#' @examples
#' hetu("111111-111C")
#' hetu("111111-111C")$date
#' hetu("111111-111C")$sex
#' # Same as previous, but using extract argument
#' hetu("111111-111C", extract="sex")
#' 
#' # Process a vector of hetu's
#' hetu(c("010101-0101", "111111-111C"))
#' 
#' # Process a vector of hetu's and extract sex information from each
#' hetu(c("010101-0101", "111111-111C"), extract="sex")
#' @export
hetu <- function(pin, extract = NULL, allow.temp = FALSE) {

  if (!is.null(extract)) {
    if (!extract %in% c("hetu", "sex", "personal.number", "checksum", 
       		        "date", "day", "month", "year", "century", "is.temp")) {
      stop("Trying to extract invalid part of hetu")
    }
  }
  
  # Check if the input parameter is a vector
  if (length(pin) > 1) {
    if (is.null(extract)) {
      res <- lapply(pin, FUN=hetu, extract = extract, allow.temp = allow.temp)
      # Remove possible NAs
      res <- res[!is.na(res)]
      # Prevent using 0 length vector
      if (length(res) == 0) {stop("Input valid PINs or change allow.temp = TRUE")}
      # Convert dates to characters to avoid conversion problems
      for (i in 1:length(res)) {res[[i]]$date <- as.character(res[[i]]$date)}
      # Convert list to data.frame
      res <- do.call(rbind.data.frame, res) 
      # dates back to dates
      res$date <- as.Date(as.character(res$date))
      # Return
      return(res)
    } else {
      return(unname(do.call("c", lapply(pin, FUN=hetu, extract = extract, allow.temp = allow.temp))))
    }    
  }
  
  # Convert to character vector if necessary
  if(!is.character(pin)) pin <- as.character(pin)
  
  # Check day
  day <- as.numeric(substr(pin, start=1, stop=2))
  if (!((day >= 1) && (day <= 31))) {
    warning(paste("Invalid day in hetu", pin))
    return(NA)
  }
  
  # Check month
  month <- as.numeric(substr(pin, start=3, stop=4))
  if (!((month >= 1) && (month <= 12))) {
    warning(paste("Invalid month in hetu", pin))
    return(NA)
  }
  
  # Check year
  year <- as.numeric(substr(pin, start=5, stop=6))
  if (!((year >= 00) && (year <= 99))) {
    return(NA)
  }
  
  # Check century
  century <- substr(pin, start=7, stop=7)
  if (!century %in% c("+", "-", "A")) {
    warning(paste0("Invalid century character '", century, "' in hetu", hetu))
    return(NA)
  }
  
  # Construct complete year from century character and 2-digit year
  
  ## Pad leading zero to a 2-digit year if needed
  year <- formatC(year, flag=0, width=2) 
  
  if (century == "+") {
    full.year <- as.numeric(paste("18", year, sep=""))
  }
  if (century == "-") {
    full.year <- as.numeric(paste("19", year, sep=""))
  }
  if (century == "A") {
    full.year <- as.numeric(paste("20", year, sep=""))
  }
  
  # Check if date exists
  date <- as.Date(paste(day, "/", month, "/", full.year, sep=""), "%d/%m/%Y")
  if (is.na(date)) {
    return(NA)
  }
  
  # Check if checksum character is valid
  check <- substr(pin, start=11, stop=11)
  checklist <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E", "F", "H", "J", "K", "L", "M", "N", "P", "R", "S", "T", "U", "V", "W", "X", "Y")
  names(checklist) <- 0:30
  if (!check %in% checklist) {
    warning(paste0("Invalid checksum character '", check, "' in hetu ", pin))
    return(NA)
  }
  
  # Get personal identification number
  personal <- as.numeric(substr(pin, start=8, stop=10))
  if (personal == 000) {
    warning(paste("Invalid individual number 000 in hetu", pin))
    return(NA)
  } else if (personal == 001) {
    warning(paste("Invalid individual number 001 in hetu", pin))
    return(NA)
  }
  
  # Check if checksum character is correct
  mod <- as.numeric(paste(substr(pin, start=1, stop=6), 
      	 		substr(pin, start=8, stop=10), sep="")) %% 31
  if (check != checklist[as.character(mod)]) {
    warning(paste0("Incorrect checksum character '", check, "' in hetu ", pin))
    return(NA)
  }
  
  # Check sex
  if ((personal %% 2) == 0) {
    sex <- "Female"
  } else {
    sex <- "Male"
  }

  # Check if personal identification number is artificial or temporary
  if ((personal >= 900)) {
    is.temp <- TRUE
  } else {
    is.temp <- FALSE
  }
  
  # Check pin number of characters
  if (nchar(pin) != 11) {
    warning(paste("Invalid number of character in hetu", pin))
    return(NA)
  }
  
  # Create hetu-object
  object <- list(hetu = pin, sex=sex, 
  	         personal.number=formatC(personal, width = 3, format = "d", flag = "0"), 
  	         checksum=check, date=date, day=day, month=month, 
		 year=full.year, century=century, is.temp=is.temp)
  
  # Return full object or only requested part
  # First produce a dataframe that leaves out temp pins if they are not explicitly allowed

  if (allow.temp == FALSE) {
    if (is.null(extract)) {
      object <- subset(as.data.frame(object), is.temp == FALSE) #Remove temporary PINs
        if (dim(object)[1] == 0) {return(NA)} #If all PINs were temporary, return NA
        else {return(object)} #If there were at least some allowed pins, return data frame
    } else {
      object <- subset(as.data.frame(object), is.temp == FALSE)
        if (dim(object)[1] == 0) {return(NA)}
        else {return(unname(do.call("c", object[extract])))}
    }
  } else if (allow.temp == TRUE) { #If temporary PINs are allowed, print the whole data frame normally
    if (is.null(extract)) {
      return (as.data.frame(object))
    }
    else {
      return(unname(do.call("c", object[extract])))
    }
  }
}