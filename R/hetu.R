#' @title Finnish personal identification number extraction
#' @description Extract information from Finnish personal identification numbers (hetu).
#' @param pin Finnish personal identification number as a character vector, 
#' 	  or vector of identification numbers as a character vectors
#' @param extract Extract only selected part of the information. 
#'    Valid values are "\code{hetu}", "\code{sex}", "\code{p.num}",
#'    "\code{checksum}", "\code{date}", "\code{day}", "\code{month}", 
#'    "\code{year}", "\code{century}", "\code{is.temp}".
#'    If \code{NULL} (default), returns all information. 
#' @param allow.temp Allow artificial or temporary PINs (personal numbers 900-999). 
#'    If \code{FALSE} (default), only PINs intended for official use (personal numbers 002-899) are allowed.
#' @param diagnostic Print additional information about possible problems in PINs. The checks are 
#'   "\code{invalid.p.num}", "\code{invalid.checksum}", "\code{incorrect.checksum}" 
#'   "\code{invalid.date}", "\code{invalid.day}", "\code{invalid.month}, "\code{invalid.length}, 
#'  "\code{invalid.century}". Default is \code{FALSE} which returns no diagnostic information.
#' @return Finnish personal identification number data.frame,
#'     or if extract parameter is set, the requested part of the 
#'	   information as a vector. Returns an error or \code{NA} if the given character 
#'	   vector is not a valid Finnish personal identification number.
#' \item{hetu}{Finnish personal identification number as a character vector. 
#'     A correct pin should be in the form DDMMYYCZZZQ, where DDMMYY stands for date, C for century sign, 
#'     ZZZ for personal number and Q for checksum character.}
#' \item{sex}{sex of the person as a character vector ("Male" or "Female").}
#' \item{p.num}{Personal number part of the identification number.}
#' \item{checksum}{Checksum for the personal identification number.}
#' \item{date}{Birthdate.}
#' \item{day}{Day of the birthdate.}
#' \item{month}{Month of the birthdate.}
#' \item{year}{Year of the birthdate.}
#' \item{century}{Century character of the birthdate: 
#'            + (1800), - (1900) or A (2000). }
#' \item{is.temp}{Is the personal identification number an artificial number intended for temporary use: (\code{TRUE} or \code{FALSE})}
#' 
#' @author Pyry Kantanen, Jussi Paananen
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
hetu <- function(pin, extract = NULL, allow.temp = FALSE, diagnostic = FALSE) {

  if (!is.null(extract)) {
    if (allow.temp == FALSE) {
      if (!extract %in% c("hetu", "sex", "p.num", "checksum", 
                          "date", "day", "month", "year", "century", "valid.pin")) {
        stop("Trying to extract invalid part of hetu")
      }
    } else if (allow.temp == TRUE) {
      if (!extract %in% c("hetu", "sex", "p.num", "checksum", 
                          "date", "day", "month", "year", "century", "is.temp", "valid.pin")) {
        stop("Trying to extract invalid part of hetu")
      }
    }
  }

  # Convert to character vector if necessary
  if(!is.character(pin)) {pin <- as.character(pin)}
  
  # Check day
  day <- as.numeric(substr(pin, start=1, stop=2))
  valid.day <- ((day >= 1) & (day <= 31))
  day[!valid.day] <- NA

  # Check month
  month <- as.numeric(substr(pin, start=3, stop=4))
  valid.month <- ((month >= 1) & (month <= 12))
  month[!valid.month] <- NA
  
  # Check year
  year <- as.numeric(substr(pin, start=5, stop=6))
  valid.year <- ((year >= 0) & (year <= 99))
  
  # Construct complete year from century character and 2-digit year
  ## Pad leading zero to a 2-digit year if needed
  year <- formatC(year, flag=0, width=2) 
  year[!valid.year] <- NA
  
  # Check century
  century <- substr(pin, start=7, stop=7)
  valid.century <- century %in% c("+", "-", "A") 

  # Construct a full year based on century marker
  full_year_function <- function(pin) {
    century <- substr(pin, start=7, stop=7)
    switch(century,
           "+" = as.numeric(paste0("18",as.character(substr(pin, start=5, stop=6)))),
           "-" = as.numeric(paste0("19",as.character(substr(pin, start=5, stop=6)))),
           "A" = as.numeric(paste0("20",as.character(substr(pin, start=5, stop=6)))),
           NA
    )
  }
  
  full.year <- sapply(pin, FUN = full_year_function, USE.NAMES = FALSE)
  
  
  # Check if date exists
  # date <- rep(NA, length(pin))
  date <- as.Date(paste(day, "/", month, "/", full.year, sep=""), "%d/%m/%Y")
  date[is.na(date)] <- NA
  valid.date <- !is.na(date)
  
  # Check if checksum character is valid
  check <- substr(pin, start=11, stop=11)
  checklist <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", 
                 "A", "B", "C", "D", "E", "F", "H", "J", "K", "L", 
                 "M", "N", "P", "R", "S", "T", "U", "V", "W", "X", "Y")
  names(checklist) <- 0:30
  valid.checksum <- check %in% checklist 

  # Get personal identification number
  personal <- (substr(pin, start=8, stop=10))
  valid.p.num <- as.numeric(personal) >= 2
  
  # Check if checksum character is correct
  mod <- as.numeric(paste0(substr(pin, start=1, stop=6), 
      	 		substr(pin, start=8, stop=10))) %% 31
  correct.checksum <- check == checklist[as.character(mod)]
  correct.checksum[is.na(correct.checksum)] <- FALSE
  
  # Check sex
  sex <- ifelse(((as.numeric(personal) %% 2) == 0), "Female", "Male")

  # Check if personal identification number is artificial or temporary
  is.temp <- as.numeric(personal) >= 900
  
  # Check pin number of characters
  valid.length <- nchar(pin) == 11
  
  # Produce a logical test value for overall validity of PIN
  valid.pin <- rep(NA, length(pin))
  for(i in 1:length(pin)) {
    valid.pin[i] <- all(c(valid.p.num[i], valid.checksum[i], correct.checksum[i],
                          valid.date[i], valid.day[i], valid.month[i], valid.year[i], valid.length[i], valid.century[i])) == TRUE
  }
  
  # Create hetu-object
  object <- list(hetu = pin, sex=sex, 
  	         p.num=formatC(personal, width = 3, format = "d", flag = "0"), 
  	         checksum=check, date=date, day=day, month=month, 
		 year=full.year, century=century, valid.pin=valid.pin)
  
  if (diagnostic == TRUE) {
    # create hetu-object with diagnostics
    diagnostics <- list(valid.p.num = valid.p.num, 
                        valid.checksum=valid.checksum, correct.checksum=correct.checksum, 
                        valid.date=valid.date, valid.day=valid.day, valid.month=valid.month, 
                        valid.year=valid.year, valid.length=valid.length, valid.century=valid.century)
    object <- append(object, diagnostics)
  }
  
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
    # create separate diagnostics column for is.temp, append it to object 
    temp_diag <- list(is.temp=is.temp)
    object <- append(object, temp_diag)
    if (is.null(extract)) {
      return (as.data.frame(object))
    }
    else {
      return(unname(do.call("c", object[extract])))
    }
  }
}

