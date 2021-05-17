# CONSTANTS ---------------------------------------------------------------
delayedAssign("ORIGIN", new_date(0))

delayedAssign(
  "DAYS_BEFORE_MONTH",
  c(0L, 31L, 59L, 90L, 120L, 151L, 181L, 212L, 243L, 273L, 304L, 334L)
)

# The following is based on functions of Davis Vaughan in
# https://github.com/DavisVaughan/datea/blob/master/R/ymon-as.R.
# It is quicker than doing as.POSIXlt.Date and will work with
# all date and grate objects.
as_utc_posixlt_from_int <- function(x) {
  attributes(x) <- NULL
  x <- x * 86400 # multiply by seconds in day (24 * 60 * 60)
  as.POSIXlt(x, tz = "UTC", origin = new_posixct(x = 0, tzone = "UTC"))
}

#' POSIXct generator
#'
#' This function allows the quick creation of Date. It is based on the internal
#'   `.Date()` function.
#' @param x A double vector representing the number of seconds since the UNIX
#'   "epoch", 1970-01-01.
#' @param tzone A character vector representing the desired time zone.  Defaults
#'   to "" for the local time zone.  Possible values can be found with
#'   [OlsonNames()].
#'
#' @return a ([POSIXct]) object
#' @keywords internal
new_posixct <- function(x = double(), tzone = "") {
  class(x) <- c("POSIXct", "POSIXt")
  attr(x, "tzone") <- tzone
  x
}

# check for suggested packages
check_suggests <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    abort(sprintf("Suggested package '%s' not present.", package))
  }
}

# check if entries of a vector are whole numbers
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
  abs(x - round(x)) < tol
}
