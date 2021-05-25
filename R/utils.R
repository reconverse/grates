# The following is based on functions of Davis Vaughan in
# https://github.com/DavisVaughan/datea/blob/master/R/ymon-as.R.
# It is quicker than doing as.POSIXlt.Date and will work with
# all date and grate objects.
as_utc_posixlt_from_int <- function(x) {
  attributes(x) <- NULL
  x <- x * 86400 # multiply by seconds in day (24 * 60 * 60)
  as.POSIXlt(x, tz = "UTC", origin = new_datetime(0, tzone = "UTC"))
}

# check for suggested packages
check_suggests <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    abort(sprintf("Suggested package '%s' not present.", package))
  }
}
