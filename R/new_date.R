#' Date generator
#'
#' This function allows the quick creation of Date. It is based on the internal
#'   `.Date()` function.
#'
#' @param x A double vector representing the number of days since the UNIX
#'   "epoch", 1970-01-01.
#'
#' @return a ([Date]) object.
#'
#' @examples
#' new_date(0)
#'
#' @keywords internal
#' @export
new_date <- function(x = double()) {
  class(x) <- "Date"
  x
}
