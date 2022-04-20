#' @importFrom stats quantile setNames
NULL

# ------------------------------------------------------------------------- #
# check for suggested packages
check_suggests <- function(package) {
    if (!requireNamespace(package, quietly = TRUE))
        stop(sprintf("Suggested package '%s' not present.", package))
}

# ------------------------------------------------------------------------- #
.is_boolean <- function(x) is.logical(x) && length(x) == 1L && !is.na(x)

# ------------------------------------------------------------------------- #
.is_scalar_whole <- function(x, tol = .Machine$double.eps^0.5) {
    if (is.integer(x) && length(x) == 1L)
        return(TRUE)
    if (is.double(x) && length(x) == 1L && (abs(x - round(x)) < tol))
        return(TRUE)
    FALSE
}

# ------------------------------------------------------------------------- #
.is_whole <- function(x, tol = .Machine$double.eps^0.5) {
    is.integer(x) || (is.double(x) && all(abs(x - round(x)) < tol, na.rm = TRUE))
}

# ------------------------------------------------------------------------- #
.as_date <- function(x, ...) {
    tz <- attr(x, "tzone")
    if (is.null(tz))
        tz <- "" # current time zone (used for POSIXt transformations)
    as.Date(x, tz = tz)
}

# ------------------------------------------------------------------------- #
# The following is based on functions of Davis Vaughan in
# https://github.com/DavisVaughan/datea/blob/master/R/ymon-as.R.
# It is quicker than doing as.POSIXlt.Date and will work with
# all date and grate objects.
.as_utc_posixlt_from_int <- function(x) {
    x <- as.double(x)
    x <- x * 86400 # multiply by seconds in day (24 * 60 * 60)
    as.POSIXlt(x, tz = "UTC", origin = .POSIXct(xx = 0, tz = "UTC"))
}
