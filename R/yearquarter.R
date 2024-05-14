# -------------------------------------------------------------------------
#' Minimal constructor for a yearquarter object
#'
# -------------------------------------------------------------------------
#' `new_yearquarter()` is a constructor for `<grates_yearquarter>` objects aimed
#' at developers.
#'
# -------------------------------------------------------------------------
#' `<yearquarter>` objects are stored as the number of quarters (starting at 0)
#' since the Unix Epoch (1970-01-01).
#'
# -------------------------------------------------------------------------
#' @param x `[integer]`
#'
#' Vector representing the number of quarters.
#'
#' `double` vectors will be converted via `as.integer(floor(x))`.
#'
#' @param xx
#'
#' \R object.
#'
# -------------------------------------------------------------------------
#' @return
#' A `<grates_yearquarter>` object.
#'
# -------------------------------------------------------------------------
#' @examples
#' new_yearquarter(1:10)
#'
# -------------------------------------------------------------------------
#' @export
new_yearquarter <- function(x = integer()) {
    if (is.vector(x, "double")) {
        x <- as.integer(floor(x))
    } else if (!is.integer(x)) {
        stop("`x` must be integer.")
    }

    .new_yearquarter(x = x)
}

# -------------------------------------------------------------------------
#' Constructor for yearquarter objects
#'
# -------------------------------------------------------------------------
#' `yearquarter()` is a constructor for `<grates_yearquarter>` objects.
#'
# -------------------------------------------------------------------------
#' `<grates_yearquarter>` objects are stored as the number of quarters (starting
#' at 0) since the Unix Epoch (1970-01-01).
#'
# -------------------------------------------------------------------------
#' @param year `[integer]`
#'
#' Vector representing the year associated with `quarter`.
#'
#' `double` vectors will be converted via `as.integer(floor(x))`.
#'
#' @param quarter `[integer]`
#'
#' Vector representing the quarter associated with `year.
#'
#' `double` vectors will be converted via `as.integer(floor(x))`.
#'
# -------------------------------------------------------------------------
#' @return
#' A `<grates_yearquarter>` object.
#'
# -------------------------------------------------------------------------
#' @examples
#' yearquarter(year = 2000L, quarter = 3L)
#'
# -------------------------------------------------------------------------
#' @seealso
#' `as_yearquarter()` and `new_yearquarter()`.
#'
# -------------------------------------------------------------------------
#' @export
yearquarter <- function(year = integer(), quarter = integer()) {

    # check year is integerish
    if (is.vector(year, "double")) {
        year <- as.integer(floor(year))
    } else if (!is.integer(year)) {
        stop("`year` must be integer.")
    }

    # check quarter is integerish
    if (is.vector(quarter, "double")) {
        quarter <- as.integer(floor(quarter))
    } else if (!is.integer(quarter)) {
        stop("`quarter` must be integer.")
    }

    # check quarter bounded above and below
    idx <- quarter < 1L | quarter > 4L
    if (any(idx, na.rm = TRUE)) {
        first <- which.max(idx)
        stopf(
            "quarters must be integer and between 1 and 4 (inclusive) or NA. Entry %d is not (it equals %d).",
            first, quarter[first]
        )
    }

    # check compatible lengths
    tmp <- .recycle(year, quarter)
    year <- tmp[[1L]]
    quarter <- tmp[[2L]]

    .yearquarter(year = year, quarter = quarter)
}

# -------------------------------------------------------------------------
#' @rdname new_yearquarter
#' @export
is_yearquarter <- function(xx) {
    inherits(xx, "grates_yearquarter")
}

# -------------------------------------------------------------------------
#' Print a year-quarter object
#'
#' @param x A `<grates_yearquarter>` object.
#' @param ... Not currently used.
#'
#' @export
print.grates_yearquarter <- function(x, ...) {
    # replicate the header as in vctrs
    n <- length(x)
    cat("<grates_yearquarter[", n, "]>\n", sep = "")
    if (n)
        print(format.grates_yearquarter(x))
    invisible(x)
}

# -------------------------------------------------------------------------
#' @rdname print.grates_yearquarter
#' @export
format.grates_yearquarter <- function(x, ...) {
    if (length(x) == 0L)
        return(character(0L))
    out <- as.POSIXlt(x)
    out <- sprintf("%04d-Q%d", out$year + 1900L, out$mon %/% 3L + 1)
    out[is.na(x)] <- NA_character_
    out
}

# -------------------------------------------------------------------------
#' @exportS3Method vctrs::vec_ptype_abbr
vec_ptype_abbr.grates_yearquarter <- function(x, ...) {"yearquarter"}

#' @exportS3Method vctrs::vec_ptype_full
vec_ptype_full.grates_yearquarter <- function(x, ...) {"yearquarter"}

# -------------------------------------------------------------------------
#' Coerce an object to year-quarter
#'
#' @description
#' `as_yearquarter()` is a generic for coercing input in to `<grates_yearquarter>`.
#' Character input is first parsed using `as.Date()`. POSIXct and POSIXlt are
#' all converted, with the timezone respected.
#'
#' @param x
#'
#' \R object
#'
#' @param ...
#'
#' Only used For character input where additional arguments are passed through
#' to `as.Date()`.
#'
#' @return
#' A `<grates_yearquarter>` object.
#'
#' @examples
#' as_yearquarter(Sys.Date())
#' as_yearquarter(as.POSIXct("2019-03-04 01:01:01", tz = "America/New_York"), interval = 2)
#' as_yearquarter("2019-05-03")
#'
#' @note
#' Internally `<grates_yearquarter>` objects are stored as the number of
#' quarters (starting at 0) since the Unix Epoch (1970-01-01).
#'
#' @seealso
#' `as.Date()`
#'
#' @export
as_yearquarter <- function(x, ...) {
    UseMethod("as_yearquarter")
}

# -------------------------------------------------------------------------
#' @rdname as_yearquarter
#' @export
as_yearquarter.default <- function(x, ...) {
    stopf("Not implemented for class [%s].", paste(class(x), collapse = ", "))
}

# -------------------------------------------------------------------------
#' @rdname as_yearquarter
#' @export
as_yearquarter.Date <- function(x, ...) {

    # convert to posixlt (this will always be UTC when called on a date)
    x <- as.POSIXlt(x)

    # calculate the year
    yr <- x$year + 1900L

    # calculate the month relative to unix epoch
    mon <- (yr - 1970L) * 12L + x$mon

    # TODO - could mon ever be double here? Maybe call as_yearquarter again?
    .new_yearquarter(mon %/% 3L)
}

# -------------------------------------------------------------------------
#' @rdname as_yearquarter
#' @export
as_yearquarter.POSIXt <- function(x, ...) {
    x <- .as_date(x)
    as_yearquarter.Date(x)
}

# -------------------------------------------------------------------------
#' @rdname as_yearquarter
#' @export
as_yearquarter.character <- function(x, ...) {
    out <- as.Date(x, ...)
    if (all(is.na(out)))
        stop("Unable to parse any entries of `x` as Dates.")
    as_yearquarter.Date(x = out, ...)
}

# -------------------------------------------------------------------------
#' @rdname as_yearquarter
#' @export
as_yearquarter.factor <- function(x, ...) {
    x <- as.character(x)
    as_yearquarter.character(x, ...)
}

#' @export
`[.grates_yearquarter` <- function(x, ..., drop = FALSE) {
    out <- NextMethod()
    class(out) <- class(x)
    out
}

# -------------------------------------------------------------------------
#' @export
`[[.grates_yearquarter` <- function(x, ..., drop = TRUE) {
    out <- NextMethod()
    class(out) <- class(x)
    out
}

# -------------------------------------------------------------------------
#' @export
`[<-.grates_yearquarter` <- function(x, ..., value) {
    if (!inherits(value, "grates_yearquarter"))
        stop("Can only assign <grates_yearquarter> objects in to an <grates_yearquarter> object.")
    out <- NextMethod()
    class(out) <- class(x)
    out
}

# -------------------------------------------------------------------------
#' @export
`[[<-.grates_yearquarter` <- `[<-.grates_yearquarter`

# -------------------------------------------------------------------------
#' @export
rep.grates_yearquarter <- function(x, ...) {
    out <- NextMethod()
    class(out) <- class(x)
    out
}

# -------------------------------------------------------------------------
#' @export
unique.grates_yearquarter <- function(x, incomparables = FALSE, ...) {
    out <- NextMethod()
    class(out) <- class(x)
    out
}

# -------------------------------------------------------------------------
#' @export
c.grates_yearquarter <- function(..., recursive = FALSE, use.names = TRUE) {
    dots <- list(...)
    if (!all(vapply(dots, inherits, TRUE, what = "grates_yearquarter")))
        stop("Unable to combine <grates_yearquarter> objects with other classes.")
    res <- NextMethod()
    .new_yearquarter(res)
}

# -------------------------------------------------------------------------
#' @export
seq.grates_yearquarter <- function(from, to, by = 1L, ...) {

    if (!inherits(to, "grates_yearquarter") || length(to) != 1L)
        stop("`to` must be a <grates_yearquarter> object of length 1.")

    if (!.is_scalar_whole(by))
        stop("`by` must be an integer of length 1.")

    from <- as.integer(from)
    to <- as.integer(to)
    out <- seq.int(from = from, to = to, by = by)

    # Ensure integer as we cannot rely on seq.int (may return double)
    out <- as.integer(out)
    .new_yearquarter(out)
}

# -------------------------------------------------------------------------
#' @export
as.integer.grates_yearquarter <- function(x, ...) {
    unclass(x)
}

# -------------------------------------------------------------------------
#' @export
as.double.grates_yearquarter <- function(x, ...) {
    as.double(unclass(x))
}

# -------------------------------------------------------------------------
#' @export
as.Date.grates_yearquarter <- function(x, ...) {
    days <- .month_to_days(unclass(x) * 3L)
    .Date(as.double(days))
}

# -------------------------------------------------------------------------
#' @export
as.POSIXct.grates_yearquarter <- function(x, tz = "UTC", ...) {
    if (tz != "UTC") {
        stop(
            "<grates_yearquarter> objects can only be converted to UTC. ",
            "If other timezones are required, first convert to <Date> and then proceed as desired."
        )
    }
    x <- .month_to_days(unclass(x) * 3L)
    .POSIXct(x * 86400, tz = "UTC")
}

# -------------------------------------------------------------------------
#' @export
as.POSIXlt.grates_yearquarter <- function(x, tz = "UTC", ...) {
    if (tz != "UTC") {
        stop(
            "<grates_yearquarter> objects can only be converted to UTC. ",
            "If other timezones are required, first convert to <Date> and then proceed as desired."
        )
    }
    x <- .month_to_days(unclass(x) * 3L)
    as.POSIXlt(x * 86400, tz = "UTC", origin = .POSIXct(0, tz = "UTC"))
}

# -------------------------------------------------------------------------
#' @export
as.character.grates_yearquarter <- function(x, ...) {
    format.grates_yearquarter(x)
}

# -------------------------------------------------------------------------
#' @export
as.list.grates_yearquarter <- function(x, ...) {
    lapply(unclass(x), `class<-`, class(x))
}

# -------------------------------------------------------------------------
#' @export
as.data.frame.grates_yearquarter <- as.data.frame.vector

# -------------------------------------------------------------------------
#' @export
min.grates_yearquarter <- function(x, ..., na.rm = FALSE) {
    out <- NextMethod()
    class(out) <- class(x)
    out
}

# -------------------------------------------------------------------------
#' @export
max.grates_yearquarter <- function(x, ..., na.rm = FALSE) {
    out <- NextMethod()
    class(out) <- class(x)
    out
}

# -------------------------------------------------------------------------
#' @export
range.grates_yearquarter <- function(x, ..., na.rm = FALSE) {
    out <- NextMethod()
    class(out) <- class(x)
    out
}

# -------------------------------------------------------------------------
#' @export
Summary.grates_yearquarter <- function(..., na.rm = FALSE) {
    stopf("`%s()` is not supported for <grates_yearquarter> objects.", .Generic)
}

# -------------------------------------------------------------------------
#' @export
Math.grates_yearquarter <- function(x, ...) {
    stopf("`%s()` is not supported for <grates_yearquarter> objects.", .Generic)
}

# -------------------------------------------------------------------------
#' @export
quantile.grates_yearquarter <- function(x, type = 1, ...) {
    x <- unclass(x)
    x <- as.integer(quantile(x, type = type, ...))
    .new_yearquarter(x)
}

# -------------------------------------------------------------------------
#' @export
Ops.grates_yearquarter <- function(e1, e2) {
    op <- .Generic
    if (op %in% c("==", "!=", "<", ">", "<=", ">=")) {
        if (inherits(e2, "grates_yearquarter")) {
            return(NextMethod())
        }
        stop("Can only compare <grates_yearquarter> objects with <grates_yearquarter> objects.")
    }

    switch(
        op,
        "+" = {
            if (missing(e2)) {
                return(e1)
            } else if (inherits(e1, "grates_yearquarter") && inherits(e2, "grates_yearquarter")) {
                stop("Cannot add <grates_yearquarter> objects to each other.")
            } else if (inherits(e1, "grates_yearquarter") && (.is_whole(e2))) {
                return(.new_yearquarter(unclass(e1) + as.integer(e2)))
            } else if (inherits(e2, "grates_yearquarter") && (.is_whole(e1))) {
                return(.new_yearquarter(unclass(e2) + as.integer(e1)))
            }
            stop("Can only add integers to <grates_yearquarter> objects.")
        },
        "-" = {
            if (missing(e2)) {
                stop("Cannot negate a <grates_yearquarter> object.")
            } else if (inherits(e2, "grates_yearquarter")) {
                if (!inherits(e1, "grates_yearquarter")) {
                    stop("Can only subtract from a <grates_yearquarter> object, not vice-versa.")
                }
                return(unclass(e1) - unclass(e2))
            } else if (inherits(e1, "grates_yearquarter") && is.integer(e2)) {
                return(.new_yearquarter(unclass(e1) - e2))
            } else if (inherits(e1, "grates_yearquarter") && .is_whole(e2)) {
                return(.new_yearquarter(unclass(e1) - as.integer(e2)))
            }
            stop("Can only subtract whole numbers and other <grates_yearquarter> objects from <grates_yearquarter> objects.")
        },
        stopf("%s is not compatible with <grates_yearquarter> objects.", op)
    )
}

# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------------------- INTERNALS ------------------------------ #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

.new_yearquarter <- function(x = integer()) {
    structure(x, class = "grates_yearquarter")
}

.yearquarter <- function(year, quarter) {
    month <- (quarter * 3L) - 2L
    tmp <- (year - 1970L) * 12L + (month - 1L)
    .new_yearquarter(tmp %/% 3L)
}
