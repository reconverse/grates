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
.assert_not_missing <- function(x, arg, call) {
    if (missing(x))
        stopf("argument `%s` is missing, with no default.", arg, .call = call)
}

# ------------------------------------------------------------------------- #
.assert_scalar_date <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    .assert_not_missing(x = x, arg = arg, call = call)

    if (length(x) != 1L || !inherits(x, "Date"))
        stopf("`%s` must be a Date vector of length 1.", arg, .call = call)
}

# ------------------------------------------------------------------------- #
.assert_grate <- function(x, arg = deparse(substitute(x)), call = sys.call(-1L)) {
    .assert_not_missing(x = x, arg = arg, call = call)

    # nolint start: commas_linter. Aesthetics
    grates_classes <- c(
        "grates_yearweek" , "grates_isoweek", "grates_epiweek",
        "grates_yearmonth", "grates_month"  , "grates_yearquarter",
        "grates_year"     , "grates_period"
    )
    # nolint end

    if (!inherits(x, grates_classes))
        stopf("`%s` must be a <grates> object.", arg, .call = call)
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

# -------------------------------------------------------------------------
.recycle <- function(x, y) {
    lx <- length(x)
    ly <- length(y)
    if (lx == ly)
        return(list(x, y))

    if (lx == 0 || ly == 0L) {
        x <- deparse(substitute(x))
        y <- deparse(substitute(y))
        msg <- "Cannot recycle a vector of length 0:"
        msgxy <- sprintf("`%s` is of length %d and `%s` is of length %d.", x, lx, y, ly)
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
        msgxy <- sprintf("`%s` is of length %d and `%s` is of length %d.", x, lx, y, ly)
        msg <- paste(msg, msgxy, sep = "\n")
        stopf(msg, .call = sys.call(-1L))
    }

    list(x, y)
}


# -------------------------------------------------------------------------
.make_floored_integer <- function(x, .call = sys.call(-1L)) {

    if (is.vector(x, "double"))
        return(as.integer(floor(x)))

    if (is.integer(x))
        return(x)

    # otherwise error
    msg <- gettextf("`%s` must be integer.", deparse(substitute(x)))
    cond <- errorCondition(msg, call = .call[1L])
    stop(cond)
}
