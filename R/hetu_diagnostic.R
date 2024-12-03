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
#' "010101-01010", "010101-0011", "010101-9011", "010101-901S")
#' ## Print all diagnostics for various fake personal identity codes
#' hetu_diagnostic(diagnosis_example)
#' # Extract century-related checks
#' hetu_diagnostic(diagnosis_example, extract = "valid.century")
#' # Print a summary in natural language
#' summary(hetu_diagnostic(diagnosis_example))
#' @seealso \code{\link{hetu}} for the main function on which
#'    \code{hetu_diagnostic} relies on.
#'
#' @export
hetu_diagnostic <- function(pin, extract = NULL) {

  diagnostic_params <- c("hetu", "is.temp", "valid.p.num", "valid.ctrl.char",
                         "correct.ctrl.char", "valid.date", "valid.day",
                         "valid.month", "valid.year", "valid.length",
                         "valid.century")

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
  class(output) <- c("diagnostic", "data.frame")
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

new_diagnostic <- function(x) {
  structure(x, class = c("diagnostic", "data.frame"))
}

#' @export
#' @noRd
`[.diagnostic` <- function(x, ...) {
  new_diagnostic(NextMethod())
}

#' @title Is an Object from Class "diagnostic"?
#' @description Returns TRUE if the object has class "diagnostic"
#' @param object Object to be tested
#' @return TRUE or FALSE
#' @export
is.diagnostic <- function(object) {
  inherits(object, "diagnostic")
}

#' @export
#' @noRd
summary.diagnostic <- function(object, ...) {

  diag_params <- c("valid.p.num", "valid.ctrl.char",
                   "correct.ctrl.char", "valid.date", "valid.day",
                   "valid.month", "valid.year", "valid.length", "valid.century")

  res <- list()

  res$n_cases <- nrow(object)
  res$n_temp_cases <- sum(object$is.temp)

  res$n_regular_and_valid <- sum(
    rowSums(
      object[which(object$is.temp == FALSE), ][diag_params]
    ) == 9
  )

  res$n_regular_and_invalid <- (res$n_cases - res$n_temp_cases) - res$n_regular_and_valid

  res$n_temp_and_valid <- sum(
    rowSums(
      object[which(object$is.temp == TRUE), ][diag_params]
    ) == 9
  )

  res$n_temp_and_invalid <- res$n_temp_cases - res$n_temp_and_valid

  res$n_total_valid_cases <- res$n_regular_and_valid + res$n_temp_and_valid
  res$n_total_invalid_cases <- res$n_regular_and_invalid + res$n_temp_and_invalid

  object$is.valid <- (rowSums(object[diag_params]) == 9)
  class(res) <- "summary.diagnostic"
  res
}

#' @export
#' @noRd
print.summary.diagnostic <- function(x, ...) {
  cat("Diagnostics for", x$n_cases, "hetu objects: \n")
  cat("Number of valid hetu objects:", x$n_total_valid_cases, "\n")
  cat("Number of valid and non-temporary* hetu objects:",
      x$n_regular_and_valid,
      "\n")
  if (x$n_temp_and_valid > 0) {
    cat("Number of valid and temporary** hetu objects:",
        x$n_temp_and_valid,
        "\n\n")
  }
  cat("Number of invalid hetu objects:",
      x$n_total_invalid_cases,
      "\n")
  if (x$n_regular_and_invalid > 0) {
    cat("Number of invalid and non-temporary* hetu objects:",
        x$n_regular_and_invalid,
        "\n")
  }
  if (x$n_temp_and_invalid > 0) {
    cat("Number of invalid and temporary** hetu objects:",
        x$n_temp_and_invalid,
        "\n")
  }

  if (x$n_regular_and_valid > 0 || x$n_regular_and_invalid > 0) {
    cat("\n",
        "* non-temporary: p.num in range [002-899]\n")
  }
  if (x$n_temp_and_invalid > 0 || x$n_temp_and_valid > 0) {
    cat("",
        "** temporary: p.num in range [900-999]\n\n")
  }

  if ((length(x$valid_hetu) - sum(x$valid_hetu)) > 0) {
    cat("\n",
      "See table output by hetu_diagnostic() for more detailed information",
      "\n"
    )
  }
}

#' @title Plotting method for diagnostic class objects
#' @description Creates a concise plot that visualizes TRUE and FALSE cases
#' in a diagnostics data frame
#' @details There seems to be no canonical answer on what to call this type of
#' plot. Some of the names that can be found online when describing a plot for
#' binary response value on an axis are: a one-dimensional scatterplot,
#' a sparkline, a rug plot, or a strip plot / strip chart.
#' @param x a "summary.diagnostic" object
#' @param labels include column labels on y-axis, default is TRUE
#' @param negate.logicals negate TRUE and FALSE logicals, default is FALSE.
#' Sometimes it may be beneficial to emphasize FALSE cases instead of TRUE
#' @param ... Arguments to be passed to methods, such as graphical parameters.
#' For example:
#'    \itemize{
#'      \item{\strong{type} what type of plot should be drawn.
#'      Default is "h" for histogram / high density vertical lines.}
#'      \item{\strong{lwd} line width as double Default is 1.0}
#'    }
#'    See \code{\link[base]{plot}} and \code{\link[graphics]{par}} for
#'    more options
#' @importFrom graphics par text
#' @importFrom methods hasArg
#' @importFrom utils hasName
#'
#' @export
plot.diagnostic <- function(x,
                            negate.logicals = FALSE,
                            labels = TRUE,
                            ...) {

  ellipsis_args <- list(...)

  if (hasName(ellipsis_args, "type")) {
    type <- ellipsis_args$type
  } else {
    type <- "h"
  }
  if (hasName(ellipsis_args, "lwd")) {
    lwd <- ellipsis_args$lwd
  } else {
    lwd <- 1.0
  }
  if (!hasArg(labels)) {
    labels <- TRUE
  }

  def.par <- par(no.readonly = TRUE)
  on.exit(par(def.par))

  if (negate.logicals == TRUE) {
    logicals <- (unname(sapply(x, class)) == "logical")
    x[logicals] <- !x[logicals]
  }

  if (labels) {
    # Marginals with labels
    par(mar = c(1, 7.5, 1, 1))
  } else {
    # Marginals without labels
    par(mar = c(1, 4, 1, 1))
  }
  par(mfrow = c(ncol(x), 1))
  par(las = 1)
  for (i in 2:ncol(x)) {
    testiobjekti <- x[i]
    testiobjekti <- unclass(testiobjekti)
    class(testiobjekti) <- "data.frame"
    # Plots the box
    plot(NA,
         xlim = c(min(attributes(testiobjekti)$row.names),
                  max(attributes(testiobjekti)$row.names)),
         ylim = c(-0.25, 1.25),
         ylab = "",
         xlab = letters[i],
         xaxt = "n",
         yaxt = "n")

    # Ensure that every element is added on top of the last one
    par(new = TRUE)

    # Plots the values inside the box, axis ticks, axis labels
    plot(testiobjekti,
         xlim = c(min(attributes(testiobjekti)$row.names),
                  max(attributes(testiobjekti)$row.names)),
         ylim = c(-0.25, 1.25),
         type = type,
         lwd = lwd,
         lend = 1,
         ylab = "",
         xlab = "",
         yaxt = "n",
         cex = 1.0)

    x_min <- par("usr")[1]

    # Plot y-axis labels outside
    text(x = (x_min - 0.01),
         # In the middle of the bar, outside
         y = 0.5,
         # Label is the name of the column
         # Add empty spaces to give some room between box and label
         labels = ifelse(test = negate.logicals,
                         yes = paste0("!", names(testiobjekti), " "),
                         no = paste0(names(testiobjekti), " ")),
         # Rotation 0 degrees (default is 90)
         srt = 0,
         # Change clipping region to none
         xpd = NA,
         # 100 % right-justified
         adj = 1,
         # Slightly larger text
         cex = 1.1)

  }
}
