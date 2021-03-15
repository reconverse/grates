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


# check for suggested packages --------------------------------------------
check_suggests <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    msg <- sprintf("Suggested package '%s' not present.", package)
    stop(msg, call. = FALSE)
  }
}


# check if entries of a vector are whole numbers
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
  abs(x - round(x)) < tol
}

# cast a vector to an integer
int_cast <- function(x) {
  x <- unclass(x)
  if (!all(is.wholenumber(x) | is.na(x))) {
    msg <- paste(deparse1(substitute(x)), "must be a vector of whole numbers")
    stop(msg, call. = FALSE)
  }
  res <- as.integer(x)
  names(res) <- names(x)
  res
}

# pull out tzone (timezone) of object (returns "" if it does not exist)
tzone <- function(x) {
  tz <- attr(x, "tzone")
  if(is.null(tz)) "" else tz
}

is_leap_year <- function(year) {
  ((((year) %% 4) == 0 & ((year) %% 100) != 0) | ((year) %% 400) == 0)
}

# check if vector is Date
is.Date <- function(x) inherits(x, "Date")


# The following is based on a functions of Davis Vaughan in
# https://github.com/DavisVaughan/datea/blob/master/R/ymon-as.R.
# It is quicker than doing as.POSIXct.Date and will work with
# all date and grate objects.
as_utc_posixct_from_int <- function(x) {
  attributes(x) <- NULL
  x <- x * 86400 # multiply by seconds in day (24 * 60 * 60)
  structure(x, tzone = "UTC", class = c("POSIXct", "POSIXt"))
}

as_zoned_posixct_from_int <- function(x, tz) {
  attributes(x) <- NULL
  x <- as.character(new_date(x))
  as.POSIXct(x, tz = tz)
}

# The following is based on a functions of Davis Vaughan in
# https://github.com/DavisVaughan/datea/blob/master/R/ymon-as.R.
# It is quicker than doing as.POSIXlt.Date and will work with
# all date and grate objects.
as_utc_posixlt_from_int <- function(x) {
  attributes(x) <- NULL
  x <- x * 86400 # multiply by seconds in day (24 * 60 * 60)
  as.POSIXlt(x, tz = "UTC", origin = new_posixct(x = 0, tzone = "UTC"))
}

as_zoned_posixlt_from_int <- function(x, tz) {
  attributes(x) <- NULL
  x <- as.character(new_date(x))
  as.POSIXlt(x, tz = tz)
}
