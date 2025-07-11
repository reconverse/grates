# -------------------------------------------------------------------------
#' Yearmonth class
#'
# -------------------------------------------------------------------------
#' @description
#'
#' `<grates_yearmonth>` objects represent, unsurprisingly, years and associated months.
#' Internally they are stored as the number of months (starting at 0) since the
#' Unix Epoch (1970-01-01). Precision is only to the month level (i.e. the day
#' of the month is always dropped).
#'
# -------------------------------------------------------------------------
#' @details
#'
#' `yearmonth()` is a constructor for `<grates_yearmonth>` objects. It takes a
#' vector of year and a vector of month values as inputs. Length 1 inputs will
#' be recycled to the length of the other input and `double` vectors will
#' be converted to integer via `as.integer(floor(x))`.
#'
#' `as_yearmonth()` is a generic for coercing input into `<grates_yearmonth>`.
#' - Character input is first parsed using `as.Date()`.
#' - POSIXct and POSIXlt are converted with their timezone respected.
#'
#' `new_yearmonth()` is a minimal constructor for `<grates_yearmonth>` objects
#' aimed at developers. It takes, as input, the number of months (starting at 0)
#' since the Unix Epoch, that you wish to represent. `double` vectors will again
#' be converted to integer via `as.integer(floor(x))`.
#'
# -------------------------------------------------------------------------
#' @param x,xx
#'
#' \R objects.
#'
#' @param year `[integer]`
#'
#' Vector representing the year associated with `month`.
#'
#' `double` vectors will be converted via `as.integer(floor(x))`.
#'
#' @param month `[integer]`
#'
#' Vector representing the month associated with `year`.
#'
#' `double` vectors will be converted via `as.integer(floor(x))`.
#'
#' @param ...
#'
#' Only used for character input where additional arguments are passed through
#' to `as.Date()`.
#'
# -------------------------------------------------------------------------
#' @references
#'
#' The algorithm to convert between dates and months relative to the UNIX Epoch
#' comes from the work of Davis Vaughan in the unreleased
#' [datea](https://github.com/DavisVaughan/datea/) package
#'
# -------------------------------------------------------------------------
#' @return
#' A `<grates_yearmonth>` object.
#'
# -------------------------------------------------------------------------
#' @seealso
#'  `new_month()` and `as_month()` and for grouping of consecutive months.
#'
# -------------------------------------------------------------------------
#' @examples
#'
#' # date coercion
#' as_yearmonth(Sys.Date())
#'
#' # POSIXt coercion
#' as_yearmonth(as.POSIXct("2019-03-04 01:01:01", tz = "America/New_York"))
#'
#' # character coercion
#' as_yearmonth("2019-05-03")
#'
#' # construction
#' yearmonth(year = 2000, month = 3)
#'
#' # direct construction
#' d <- seq.Date(from = as.Date("1970-01-01"), by = "month", length.out = 10)
#' stopifnot(
#'     identical(
#'         as_yearmonth(d),
#'         new_yearmonth(0:9)
#'     )
#' )
#'
# -------------------------------------------------------------------------
#' @name yearmonth_class
NULL


# -------------------------------------------------------------------------
#' @rdname yearmonth_class
#' @export
yearmonth <- function(year = integer(), month = integer()) {

    # check year is integerish
    if (is.vector(year, "double")) {
        year <- as.integer(floor(year))
    } else if (!is.integer(year)) {
        stop("`year` must be integer.")
    }

    # check month is integerish
    if (is.vector(month, "double")) {
        month <- as.integer(floor(month))
    } else if (!is.integer(month)) {
        stop("`month` must be integer.")
    }

    # check month bounded above and below
    idx <- month < 1L | month > 12L
    if (any(idx, na.rm = TRUE)) {
        first <- which.max(idx)
        stopf(
            "Months must be integer and between 1 and 12 (inclusive) or NA. Entry %d is not (it equals %d).",
            first, month[first]
        )
    }

    # check compatible lengths
    tmp <- .recycle(year, month)
    year <- tmp[[1L]]
    month <- tmp[[2L]]

    .yearmonth(year = year, month = month)
}


# -------------------------------------------------------------------------
#' @rdname yearmonth_class
#' @export
as_yearmonth <- function(x, ...) {
    UseMethod("as_yearmonth")
}

# -------------------------------------------------------------------------
#' @rdname yearmonth_class
#' @export
as_yearmonth.default <- function(x, ...) {
    stopf("Not implemented for class [%s].", toString(class(x)))
}

# -------------------------------------------------------------------------
#' @importFrom fastymd get_ymd
#' @rdname yearmonth_class
#' @export
as_yearmonth.Date <- function(x, ...) {

    # get year, month and day
    x <- get_ymd(x)

    # calculate the month relative to unix epoch
    mon <- (x$year - 1970L) * 12L + (x$month - 1L)

    .new_yearmonth(mon)
}

# -------------------------------------------------------------------------
#' @rdname yearmonth_class
#' @export
as_yearmonth.POSIXt <- function(x, ...) {
    x <- .as_date(x)
    as_yearmonth.Date(x)
}

# -------------------------------------------------------------------------
#' @rdname yearmonth_class
#' @export
as_yearmonth.character <- function(x, ...) {
    out <- as.Date(x, ...)
    if (all(is.na(out)))
        stop("Unable to parse any entries of `x` as Dates.")
    as_yearmonth.Date(x = out, ...)
}

# -------------------------------------------------------------------------
#' @rdname yearmonth_class
#' @export
as_yearmonth.factor <- function(x, ...) {
    x <- as.character(x)
    as_yearmonth.character(x, ...)
}


# -------------------------------------------------------------------------
#' @rdname yearmonth_class
#' @export
new_yearmonth <- function(x = integer()) {
    if (is.vector(x, "double")) {
        x <- as.integer(floor(x))
    } else if (!is.integer(x)) {
        stop("`x` must be integer.")
    }
    .new_yearmonth(x = x)
}


# -------------------------------------------------------------------------
#' @rdname yearmonth_class
#' @export
is_yearmonth <- function(xx) {
    inherits(xx, "grates_yearmonth")
}

# -------------------------------------------------------------------------
#' Format and print a yearmonth object
#'
# -------------------------------------------------------------------------
#' @param x
#'
#' A `<grates_yearmonth>` object.
#'
#' @param format `[character]`
#'
#' The format to use for printing.
#'
#' @param ...
#'
#' Not currently used.
#'
# -------------------------------------------------------------------------
#' @return
#'
#' For `format()`, a character vector representing the formatted input.
#' `print()` is called for the side effect of printing to screen and thus
#' returns the input `<grates_yearmonth>` object invisibly.
#'
# -------------------------------------------------------------------------
#' @export
print.grates_yearmonth <- function(x, format = "%Y-%b", ...) {
    # replicate the header as in vctrs
    n <- length(x)
    cat("<grates_yearmonth[", n, "]>\n", sep = "")
    if (n)
        print(format.grates_yearmonth(x, format = format))
    invisible(x)
}

# -------------------------------------------------------------------------
#' @rdname print.grates_yearmonth
#' @export
format.grates_yearmonth <- function(x, format = "%Y-%b", ...) {
    if (length(x) == 0L)
        return(character(0L))
    out <- format.Date(as.Date(x), format = format)
    out[is.na(x)] <- NA_character_
    out
}

# -------------------------------------------------------------------------
#' @exportS3Method vctrs::vec_ptype_abbr
vec_ptype_abbr.grates_yearmonth <- function(x, ...) {"yrmon"}

#' @exportS3Method vctrs::vec_ptype_full
vec_ptype_full.grates_yearmonth <- function(x, ...) {"grates_yearmonth"}



#' @export
`[.grates_yearmonth` <- function(x, ..., drop = FALSE) {
    out <- NextMethod()
    class(out) <- class(x)
    out
}

# -------------------------------------------------------------------------
#' @export
`[[.grates_yearmonth` <- function(x, ..., drop = TRUE) {
    out <- NextMethod()
    class(out) <- class(x)
    out
}

# -------------------------------------------------------------------------
#' @export
`[<-.grates_yearmonth` <- function(x, ..., value) {
    if (!inherits(value, "grates_yearmonth"))
        stop("Can only assign a <grates_yearmonth> object into a <grates_yearmonth> object.")
    out <- NextMethod()
    class(out) <- class(x)
    out
}

# -------------------------------------------------------------------------
#' @export
`[[<-.grates_yearmonth` <- `[<-.grates_yearmonth`

# -------------------------------------------------------------------------
#' @export
rep.grates_yearmonth <- function(x, ...) {
    out <- NextMethod()
    class(out) <- class(x)
    out
}

# -------------------------------------------------------------------------
#' @export
unique.grates_yearmonth <- function(x, incomparables = FALSE, ...) {
    out <- NextMethod()
    class(out) <- class(x)
    out
}

# -------------------------------------------------------------------------
#' @export
c.grates_yearmonth <- function(..., recursive = FALSE, use.names = TRUE) {
    dots <- list(...)
    if (!all(vapply(dots, inherits, TRUE, what = "grates_yearmonth")))
        stop("Unable to combine <grates_yearmonth> objects with other classes.")
    res <- NextMethod()
    .new_yearmonth(res)
}

# -------------------------------------------------------------------------
#' @export
seq.grates_yearmonth <- function(from, to, by = 1L, ...) {

    if (!inherits(to, "grates_yearmonth") || length(to) != 1L)
        stop("`to` must be a <grates_yearmonth> object of length 1.")

    if (!.is_scalar_whole(by))
        stop("`by` must be an integer of length 1.")

    from <- as.integer(from)
    to <- as.integer(to)
    out <- seq.int(from = from, to = to, by = by)

    # Ensure integer as we cannot rely on seq.int (may return double)
    out <- as.integer(out)
    .new_yearmonth(out)
}

# -------------------------------------------------------------------------
#' @export
as.integer.grates_yearmonth <- function(x, ...) {
    unclass(x)
}

# -------------------------------------------------------------------------
#' @export
as.double.grates_yearmonth <- function(x, ...) {
    as.double(unclass(x))
}

# -------------------------------------------------------------------------
#' @export
as.Date.grates_yearmonth <- function(x, ...) {
    days <- .month_to_days(unclass(x))
    .Date(as.double(days))
}

# -------------------------------------------------------------------------
#' @export
as.POSIXct.grates_yearmonth <- function(x, tz = "UTC", ...) {
    if (tz != "UTC")
        stop(
            "<grates_yearmonth> objects can only be converted to UTC. ",
            "If other timezones are required, first convert to <Date> and then proceed as desired."
        )
    x <- .month_to_days(unclass(x))
    .POSIXct(x * 86400, tz = "UTC")
}

# -------------------------------------------------------------------------
#' @export
as.POSIXlt.grates_yearmonth <- function(x, tz = "UTC", ...) {
    if (tz != "UTC")
        stop(
            "<grates_yearmonth> objects can only be converted to UTC. ",
            "If other timezones are required, first convert to <Date> and then proceed as desired."
        )
    x <- .month_to_days(unclass(x))
    as.POSIXlt(.POSIXct(x * 86400, tz = "UTC"), tz = "UTC")
}

# -------------------------------------------------------------------------
#' @export
as.character.grates_yearmonth <- function(x, ...) {
    format.grates_yearmonth(x)
}

# -------------------------------------------------------------------------
#' @export
as.list.grates_yearmonth <- function(x, ...) {
    lapply(unclass(x), `class<-`, class(x))
}

# -------------------------------------------------------------------------
#' @export
as.data.frame.grates_yearmonth <- as.data.frame.vector

# -------------------------------------------------------------------------
#' @export
min.grates_yearmonth <- function(x, ..., na.rm = FALSE) {
    out <- NextMethod()
    class(out) <- class(x)
    out
}

# -------------------------------------------------------------------------
#' @export
max.grates_yearmonth <- function(x, ..., na.rm = FALSE) {
    out <- NextMethod()
    class(out) <- class(x)
    out
}

# -------------------------------------------------------------------------
#' @export
range.grates_yearmonth <- function(x, ..., na.rm = FALSE) {
    out <- NextMethod()
    class(out) <- class(x)
    out
}

# -------------------------------------------------------------------------
#' @export
Summary.grates_yearmonth <- function(..., na.rm = FALSE) {
    stopf("`%s()` is not supported for <grates_yearmonth> objects.", .Generic)
}

# -------------------------------------------------------------------------
#' @export
Math.grates_yearmonth <- function(x, ...) {
    stopf("`%s()` is not supported for <grates_yearmonth> objects.", .Generic)
}

# -------------------------------------------------------------------------
#' @export
quantile.grates_yearmonth <- function(x, type = 1, ...) {
    x <- unclass(x)
    x <- as.integer(quantile(x, type = type, ...))
    .new_yearmonth(x)
}

# -------------------------------------------------------------------------
#' @export
Ops.grates_yearmonth <- function(e1, e2) {
    op <- .Generic
    if (op %in% c("==", "!=", "<", ">", "<=", ">=")) {
        if (inherits(e2, "grates_yearmonth")) {
            return(NextMethod())
        }
        stop("Can only compare <grates_yearmonth> objects with <grates_yearmonth> objects.")
    }

    switch(
        op,
        "+" = {
            if (missing(e2)) {
                return(e1)
            } else if (inherits(e1, "grates_yearmonth") && inherits(e2, "grates_yearmonth")) {
                stop("Cannot add <grates_yearmonth> objects to each other.")
            } else if (inherits(e1, "grates_yearmonth") && (.is_whole(e2))) {
                return(.new_yearmonth(unclass(e1) + as.integer(e2)))
            } else if (inherits(e2, "grates_yearmonth") && (.is_whole(e1))) {
                return(.new_yearmonth(unclass(e2) + as.integer(e1)))
            }
            stop("Can only add integers to <grates_yearmonth> objects.")
        },
        "-" = {
            if (missing(e2)) {
                stop("Cannot negate a <grates_yearmonth> object.")
            } else if (inherits(e2, "grates_yearmonth")) {
                if (!inherits(e1, "grates_yearmonth")) {
                    stop("Can only subtract from a <grates_yearmonth> object, not vice-versa.")
                }
                return(unclass(e1) - unclass(e2))
            } else if (inherits(e1, "grates_yearmonth") && is.integer(e2)) {
                return(.new_yearmonth(unclass(e1) - e2))
            } else if (inherits(e1, "grates_yearmonth") && .is_whole(e2)) {
                return(.new_yearmonth(unclass(e1) - as.integer(e2)))
            }
            stop("Can only subtract whole numbers and other <grates_yearmonth> objects from <grates_yearmonth> objects.")
        },
        stopf("%s is not compatible with <grates_yearmonth> objects.", op)
    )
}

# -------------------------------------------------------------------------
#' @export
is.numeric.grates_yearmonth <- function(x) {
    FALSE
}

# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------------------- INTERNALS ------------------------------ #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

.new_yearmonth <- function(x = integer()) {
    class(x) <- "grates_yearmonth"
    x
}

.yearmonth <- function(year, month) {
    out <- (year - 1970L) * 12L + (month - 1L)
    .new_yearmonth(out)
}
