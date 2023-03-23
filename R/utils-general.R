#' @importFrom stats quantile setNames
NULL

# ------------------------------------------------------------------------- #
stopf <- function(fmt, ..., .use_call = TRUE, .call = sys.call(-1L)) {
    .call <- if (isTRUE(.use_call)) .call[1L] else NULL
    msg <- sprintf(fmt, ...)
    err <- simpleError(msg, .call)
    stop(err)
}

# ------------------------------------------------------------------------- #
# check for suggested packages
.check_suggests <- function(package) {
    if (!requireNamespace(package, quietly = TRUE))
        stopf("Suggested package '%s' not present.", package)
}

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

# -------------------------------------------------------------------------
.recycle <- function(x, y) {
    lx <- length(x)
    ly <- length(y)
    if (lx == ly)
        return(list(x,y))

    if (lx == 0 || ly == 0L) {
        x <- deparse(substitute(x))
        y <- deparse(substitute(y))
        msg <- "Cannot recycle a vector of length 0:"
        msgxy <- sprintf("`%s` is of length %d and `%s` is of length %d." , x, lx, y, ly)
        msg <- paste(msg, msgxy, sep = "\n")
        stopf(msg, .call = sys.call(-1L))
    }

    if (lx == 1L) {
        x <- rep.int(x, ly)
    } else if (ly == 1L) {
        y <- rep.int(y, lx)
    } else {
        x <- deparse(substitute(x))
        y <- deparse(substitute(y))
        msg <- "Can only recycle vectors of length 1:"
        msgxy <- sprintf("`%s` is of length %d and `%s` is of length %d." , x, lx, y, ly)
        msg <- paste(msg, msgxy, sep = "\n")
        stopf(msg, .call = sys.call(-1L))
    }

    list(x, y)
}
