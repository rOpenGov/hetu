#' @title Quick Data Frame
#' @description Speedups in data.frame creation.
#' @param x named list of equal length vectors
#' @return data.frame
#' @details This is 20x faster than as.data.frame.
#' @references Idea borrowed from \url{http://adv-r.had.co.nz/Profiling.html}.
#' @author HW/LL
#' @keywords internal
#' @export
quickdf <- function(x) {
  class(x) <- "data.frame"
  attr(x, "row.names") <- .set_row_names(length(x[[1]]))
  x
}
