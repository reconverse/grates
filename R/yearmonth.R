#' Construct a yearmonth object
#'
#' @description
#' `new_yearmonth()` is a constructor for `<grates_yearmonth>` objects aimed at
#' developers.
#'
#' @details
#' `<grates_yearmonth>` objects are stored as the number of months (starting at
#' 0) since the Unix Epoch (1970-01-01). Precision is only to the month level
#' (i.e. the day of the month is always dropped).
#'
#' @param x `[integer]`
#'
#' Vector representing the number of months.
#'
#' `double` vectors will be converted via `as.integer(floor(x))`.
#'
#' @param xx
#'
#' \R object
#'
#' @references
#' The algorithm to convert between dates and months relative to the UNIX Epoch
#' comes from the work of Davis Vaughan in the unreleased
#' [datea](https://github.com/DavisVaughan/datea/) package
#'
#' @return
#' A `<grates_yearmonth>` object.
#'
#' @examples
#' new_yearmonth(1:10)
#'
#' @export
new_yearmonth <- function(x = integer()) {
    if (!is.integer(x)) {
        if (is.double(x) && is.vector(x)) {
            x <- as.integer(floor(x))
        } else {
            stop("`x` must be integer.")
        }
    }

    .new_yearmonth(x = x)
}

# -------------------------------------------------------------------------
#' @rdname new_yearmonth
#' @export
is_yearmonth <- function(xx) inherits(xx, "grates_yearmonth")

# -------------------------------------------------------------------------
#' Print a year-month object
#'
#' @param x
#'
#' A `<grates_yearmonth>` object.
#'
#' @param format
#'
#' The format to use for printing.
#'
#' @param ...
#'
#' Not currently used.
#'
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
    if (length(x) == 0)
        return(character(0))
    out <- format.Date(as.Date(x), format = format)
    out[is.na(x)] <- NA_character_
    out
}

# -------------------------------------------------------------------------
vec_ptype_abbr.grates_yearmonth <- function(x, ...) "yrmon"
vec_ptype_full.grates_yearmonth <- function(x, ...) "grates_yearmonth"

# -------------------------------------------------------------------------
#' Coerce an object to year-month
#'
#' @description
#' `as_yearmonth()` is a generic for coercing input in to `<grates_yearmonth>`.
#' Character input is first parsed using `as.Date()`. POSIXct and POSIXlt are
#' all converted, with the timezone respected.
#'
#' @param x
#'
#' \R object.
#'
#' @param ...
#'
#' Only used For character input where additional arguments are passed through
#' to `as.Date()`.
#'
#' @return
#' A `<grates_yearmonth>` object.
#'
#' @examples
#' as_yearmonth(Sys.Date())
#' as_yearmonth(as.POSIXct("2019-03-04 01:01:01", tz = "America/New_York"), interval = 2)
#' as_yearmonth("2019-05-03")
#'
#' @note
#' Internally `<grates_yearmonth>` objects are stored as the number of
#' months (starting at 0) since the Unix Epoch (1970-01-01). Precision is only
#' to the month level (i.e. the day of the month is always dropped).
#'
#' @references The algorithm to convert between dates and months relative to the
#' UNIX Epoch comes from the work of Davis Vaughan in the unreleased
#' [datea](https://github.com/DavisVaughan/datea/) package.
#'
#' @seealso
#' `as.Date()`
#'
#' @export
as_yearmonth <- function(x, ...) UseMethod("as_yearmonth")

# -------------------------------------------------------------------------
#' @rdname as_yearmonth
#' @export
as_yearmonth.default <- function(x, ...) {
    stop(
        sprintf(
            "Not implemented for class [%s].",
            paste(class(x), collapse = ", ")
        )
    )
}

# -------------------------------------------------------------------------
#' @rdname as_yearmonth
#' @export
as_yearmonth.Date <- function(x, ...) {

    # convert to posixlt (this will always be UTC when called on a date)
    x <- as.POSIXlt(x)

    # calculate the year
    yr <- x$year + 1900L

    # calculate the month relative to unix epoch
    mon <- (yr - 1970L) * 12L + x$mon

    # TODO - could mon ever be double here? Maybe call as_yearmonth again?
    .new_yearmonth(mon)
}

# -------------------------------------------------------------------------
#' @rdname as_yearmonth
#' @export
as_yearmonth.POSIXt <- function(x, ...) {
    x <- .as_date(x)
    as_yearmonth.Date(x)
}

# -------------------------------------------------------------------------
#' @rdname as_yearmonth
#' @export
as_yearmonth.character <- function(x, ...) {
    out <- as.Date(x, ...)
    if (all(is.na(out)))
        stop("Unable to parse any entries of `x` as Dates.")
    as_yearmonth.Date(x = out, ...)
}

# -------------------------------------------------------------------------
#' @rdname as_yearmonth
#' @export
as_yearmonth.factor <- function(x, ...) {
    x <- as.character(x)
    as_yearmonth.character(x, ...)
}

#' @export
`[.grates_yearmonth` <- function(x, ..., drop = FALSE) {
    out <- NextMethod()
    class(out) <- oldClass(x)
    out
}

# -------------------------------------------------------------------------
#' @export
`[[.grates_yearmonth` <- function(x, ..., drop = TRUE) {
    out <- NextMethod()
    class(out) <- oldClass(x)
    out
}

# -------------------------------------------------------------------------
#' @export
`[<-.grates_yearmonth` <- function(x, ..., value) {
    if (!inherits(value, "grates_yearmonth"))
        stop("Can only assign <grates_yearmonth> objects in to an <grates_yearmonth> object.")
    out <- NextMethod()
    class(out) <- oldClass(x)
    out
}

# -------------------------------------------------------------------------
#' @export
`[[<-.grates_yearmonth` <- `[<-.grates_yearmonth`

# -------------------------------------------------------------------------
#' @export
rep.grates_yearmonth <- function(x, ...) {
    out <- NextMethod()
    class(out) <- oldClass(x)
    out
}

# -------------------------------------------------------------------------
#' @export
unique.grates_yearmonth <- function(x, incomparables = FALSE, ...) {
    out <- NextMethod()
    class(out) <- oldClass(x)
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
as.integer.grates_yearmonth <- function(x, ...) unclass(x)

# -------------------------------------------------------------------------
#' @export
as.double.grates_yearmonth <- function(x, ...) as.double(unclass(x))

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
        stop("<grates_yearmonth> objects can only be converted to UTC. If other timezones are required, first convert to <Date> and then proceed as desired.")
    x <- .month_to_days(unclass(x))
    .POSIXct(x * 86400, tz = "UTC")
}

# -------------------------------------------------------------------------
#' @export
as.POSIXlt.grates_yearmonth <- function(x, tz = "UTC", ...) {
    if (tz != "UTC")
        stop("<grates_yearmonth> objects can only be converted to UTC. If other timezones are required, first convert to <Date> and then proceed as desired.")
    x <- .month_to_days(unclass(x))
    as.POSIXlt(x * 86400, tz = "UTC", origin = .POSIXct(0, tz = "UTC"))
}

# -------------------------------------------------------------------------
#' @export
as.character.grates_yearmonth <- function(x, ...) format.grates_yearmonth(x)

# -------------------------------------------------------------------------
#' @export
as.list.grates_yearmonth <- function(x, ...) lapply(unclass(x), `class<-`, class(x))

# -------------------------------------------------------------------------
#' @export
as.data.frame.grates_yearmonth <- as.data.frame.vector

# -------------------------------------------------------------------------
#' @export
min.grates_yearmonth <- function(x, ..., na.rm = FALSE) {
    out <- NextMethod()
    class(out) <- oldClass(x)
    out
}

# -------------------------------------------------------------------------
#' @export
max.grates_yearmonth <- function(x, ..., na.rm = FALSE) {
    out <- NextMethod()
    class(out) <- oldClass(x)
    out
}

# -------------------------------------------------------------------------
#' @export
range.grates_yearmonth <- function(x, ..., na.rm = FALSE) {
    out <- NextMethod()
    class(out) <- oldClass(x)
    out
}

# -------------------------------------------------------------------------
#' @export
Summary.grates_yearmonth <- function(..., na.rm = FALSE) {
    stop(
        sprintf(
            "`%s()` is not supported for <grates_yearmonth> objects.",
            .Generic
        )
    )
}

# -------------------------------------------------------------------------
#' @export
Math.grates_yearmonth <- function(x, ...) {
    stop(
        sprintf(
            "`%s()` is not supported for <grates_yearmonth> objects.",
            .Generic
        )
    )
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
        if (!inherits(e2, "grates_yearmonth"))
            stop("Can only compare <grates_yearmonth> objects with <grates_yearmonth> objects.")
        return(NextMethod())
    }

    switch(
        op,
        "+" = {
            if (missing(e2)) {
                return(e1)
            } else if (inherits(e1, "grates_yearmonth") && inherits(e2, "grates_yearmonth")) {
                stop("Cannot add <grates_yearmonth> objects to each other.")
            } else if (inherits(e1, "grates_yearmonth") && (.is_whole(e2))) {
                .new_yearmonth(unclass(e1) + as.integer(e2))
            } else if (inherits(e2, "grates_yearmonth") && (.is_whole(e1))) {
                .new_yearmonth(unclass(e2) + as.integer(e1))
            } else {
                stop("Can only add integers to <grates_yearmonth> objects.")
            }
        },
        "-" = {
            if (missing(e2)) {
                stop("Cannot negate a <grates_yearmonth> object.")
            } else if (inherits(e2, "grates_yearmonth")) {
                if (!inherits(e1, "grates_yearmonth"))
                    stop("Can only subtract from a <grates_yearmonth> object, not vice-versa.")
                unclass(e1) - unclass(e2)
            } else if (inherits(e1, "grates_yearmonth") && is.integer(e2)) {
                .new_yearmonth(unclass(e1) - e2)
            } else if (inherits(e1, "grates_yearmonth") && .is_whole(e2)) {
                .new_yearmonth(unclass(e1) - as.integer(e2))
            } else {
                stop("Can only subtract whole numbers and other <grates_yearmonth> objects from <grates_yearmonth> objects.")
            }
        },
        stop(
            sprintf("%s is not compatible with <grates_yearmonth> objects.", op)
        )
    )
}

# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------------------- INTERNALS ------------------------------ #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

.new_yearmonth <- function(x = integer()) structure(x, class = "grates_yearmonth")
