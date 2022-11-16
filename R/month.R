#' Construct a month object
#'
#' @description
#' `month()` is a constructor for `<grates_month>` objects.
#'
#' @details
#' `grates_month` objects are stored as the integer number (starting at 0), of
#' n-month groups since the Unix Epoch (1970-01-01). Here n-months is taken to
#' mean a 'grouping of n consecutive months'.
#'
#' @param x `[integer]`
#'
#' Vector representing the number of n-months since the Unix Epoch (1970-01-01).
#'
#' `double` vectors will be converted via `as.integer(floor(x))`.
#'
#' @param n `[integer]`
#'
#' Number of months that are being grouped. Must be greater than 1 (use
#' `yearmonth()` for this case).
#'
#' @param xx
#'
#' \R object.
#'
#' @references
#' The algorithm to convert between dates and months relative to the UNIX Epoch
#' comes from the work of Davis Vaughan in the unreleased
#' [datea](https://github.com/DavisVaughan/datea/) package.
#'
#' @return
#' A `<grates_month>` object.
#'
#' @examples
#' month(1:10, 2L)
#'
#' @export
month <- function(x = integer(), n) {
    if (!is.integer(x)) {
        if (is.double(x) && is.vector(x)) {
            x <- as.integer(floor(x))
        } else {
            stop("`x` must be integer.")
        }
    }

    # trigger warning for missing n at top level
    n <- n
    if (!.is_scalar_whole(n))
        stop("`n` must be an integer of length 1.")
    n <- as.integer(n)
    if (n == 1L)
        stop("`n` must be greater than 1. If single month groupings are required please use `yearmonth()`.")

    .new_month(x = x, n = n)
}

# -------------------------------------------------------------------------
#' @rdname month
#' @export
is_month <- function(xx) inherits(xx, "grates_month")

# -------------------------------------------------------------------------
#' Print a month object
#'
#' @param x
#'
#' A `<grates_month>` object.
#'
#' @param format `[character]`
#'
#' The format to use for the bounds of each value.
#'
#' @param sep `[character]`
#'
#' Where more than one month is grouped with others, `sep` is placed between the
#' upper and lower bounds when printing.
#'
#' @param ...
#'
#' Not currently used.
#'
#' @export
print.grates_month <- function(x, format = "%Y-%b", sep = "to", ...) {
    # replicate the header as in vctrs
    n <- length(x)
    cat("<grates_yearmonth[", n, "]>\n", sep = "")
    if (n)
        print(format.grates_month(x, format = format, sep = sep))
    invisible(x)
}

# -------------------------------------------------------------------------
#' @rdname print.grates_month
#' @export
format.grates_month <- function(x, format = "%Y-%b", sep = "to", ...) {
    if (length(x) == 0) return(character(0))
    out <- sprintf(
        "%s %s %s",
        format.Date(as.Date(x), format = format),
        sep,
        format.Date(as.Date(x + 1) - 1, format = format)
    )
    out[is.na(x)] <- NA_character_
    out
}

# -------------------------------------------------------------------------
vec_ptype_abbr.grates_month <- function(x, ...) "month"
vec_ptype_full.grates_month <- function(x, ...) "grates_month"

# -------------------------------------------------------------------------
#' Coerce an object to month
#'
#' @description
#' `as_month()` is a generic for coercing input in to `<grates_month>`.
#'
#' @param x An \R object.
#'
#' Character input is first parsed using `as.Date()`.
#'
#' POSIXt inputs are converted with the timezone respected.
#'
#' @param n `[integer]`
#'
#' Number of months that are being grouped. Must be greater than 1 (use
#' `as_yearmonth()` for this case).

#' @param ...
#'
#' Only used For character input where additional arguments are passed through
#' to `as.Date()`.
#'
#' @return
#' A `<grates_month>` object.
#'
#' @examples
#' as_month("2019-05-03", n = 4L)
#' as_month(as.POSIXct("2019-03-04 01:01:01", tz = "America/New_York"), n = 2)
#'
#' @note
#' Internally `grates_month` objects are stored as the position, starting at 0,
#' of n-month groups since the Unix Epoch (1970-01-01). Here n-months is taken
#' to mean a 'grouping of n consecutive months'. Precision is only to the month
#' level (i.e. the day of the  month is always dropped).
#'
#' @references
#' The algorithm to convert between dates and months relative to the UNIX Epoch
#' comes from the work of Davis Vaughan in the unreleased
#' [datea](https://github.com/DavisVaughan/datea/) package.
#'
#' @seealso
#' `as.Date()`
#'
#' @export
as_month <- function(x, n, ...) UseMethod("as_month")

# -------------------------------------------------------------------------
#' @rdname as_month
#' @export
as_month.default <- function(x, n, ...) {
    stop(
        sprintf(
            "Not implemented for class [%s].",
            paste(class(x), collapse = ", ")
        )
    )
}

# -------------------------------------------------------------------------
#' @rdname as_month
#' @export
as_month.Date <- function(x, n, ...) {

    # trigger warning for missing n at top level
    n <- n

    if (!.is_scalar_whole(n))
        stop("`n` must be an integer of length 1.")
    n <- as.integer(n)
    if (n == 1L)
        stop("`n` must be greater than 1. If single month groupings are required please use `as_yearmonth()`.")

    # convert to posixlt (this will always be UTC when called on a date)
    x <- as.POSIXlt(x)

    # calculate the year
    yr <- x$year + 1900L

    # calculate the month relative to unix epoch
    mon <- (yr - 1970L) * 12L + x$mon

    # scale month by n
    mon <- (mon %/% n)

    # TODO - could mon ever be double here? Is as.integer needed or superfluous?
    .new_month(x = as.integer(mon), n = n)
}

# -------------------------------------------------------------------------
#' @rdname as_month
#' @export
as_month.POSIXt <- function(x, n, ...) {

    # trigger warning for missing n at top level
    n <- n

    x <- .as_date(x)
    as_month.Date(x = x, n = n)
}

# -------------------------------------------------------------------------
#' @rdname as_month
#' @export
as_month.character <- function(x, n, ...) {

    # trigger warning for missing n at top level
    n <- n

    out <- as.Date(x, ...)
    if (all(is.na(out)))
        stop("Unable to parse any entries of `x` as Dates.")
    as_month.Date(x = out, n = n)
}

# -------------------------------------------------------------------------
#' @rdname as_month
#' @export
as_month.factor <- function(x, n, ...) {

    # trigger warning for missing n at top level
    n <- n

    x <- as.character(x)
    as_month.character(x, n = n, ...)
}

#' @export
`[.grates_month` <- function(x, ..., drop = FALSE) {
    out <- NextMethod()
    class(out) <- oldClass(x)
    attr(out, "n") <- attr(x, "n")
    out
}

# -------------------------------------------------------------------------
#' @export
`[[.grates_month` <- function(x, ..., drop = TRUE) {
    out <- NextMethod()
    class(out) <- oldClass(x)
    attr(out, "n") <- attr(x, "n")
    out
}

# -------------------------------------------------------------------------
#' @export
`[<-.grates_month` <- function(x, ..., value) {
    if (!inherits(value, "grates_month"))
        stop("Can only assign <grates_month> objects in to an <grates_month> object.")
    nx <- attr(x, "n")
    nv <- attr(value, "n")
    if (isTRUE(nx != nv))
        stop("Incompatible month groupings.")
    out <- NextMethod()
    class(out) <- oldClass(x)
    attr(out, "n") <- nx
    out
}

# -------------------------------------------------------------------------
#' @export
`[[<-.grates_month` <- `[<-.grates_month`

# -------------------------------------------------------------------------
#' @export
rep.grates_month <- function(x, ...) {
    out <- NextMethod()
    class(out) <- oldClass(x)
    attr(out, "n") <- attr(x, "n")
    out
}

# -------------------------------------------------------------------------
#' @export
unique.grates_month <- function(x, incomparables = FALSE, ...) {
    out <- NextMethod()
    class(out) <- oldClass(x)
    attr(out, "n") <- attr(x, "n")
    out
}

# -------------------------------------------------------------------------
#' @export
c.grates_month <- function(..., recursive = FALSE, use.names = TRUE) {
    dots <- list(...)
    if (!all(vapply(dots, inherits, TRUE, what = "grates_month")))
        stop("Unable to combine <grates_month> objects with other classes.")
    ns <- vapply(dots, function(x) attr(x, "n"), 1L)
    if (length(unique(ns)) != 1L)
        stop("Unable to combine <grates_month> objects with different groupings.")
    res <- NextMethod()
    .new_month(x = res, n = ns[[1]])
}

# -------------------------------------------------------------------------
#' @export
seq.grates_month <- function(from, to, by = 1L, ...) {

    if (!inherits(to, "grates_month") || length(to) != 1L)
        stop("`to` must be a <grates_month> object of length 1.")

    if (!.is_scalar_whole(by))
        stop("`by` must be an integer of length 1.")

    fn <- attr(from, "n")
    tn <- attr(to, "n")
    if (fn != tn)
        stop("`to` must have the same month grouping as `from`")

    from <- as.integer(from)
    to <- as.integer(to)
    out <- seq.int(from = from, to = to, by = by)

    # Ensure integer as we cannot rely on seq.int (may return double)
    out <- as.integer(out)
    .new_month(x = out, n = tn)
}

# -------------------------------------------------------------------------
#' @export
as.Date.grates_month <- function(x, ...) {
    n <- attr(x, "n")
    x <- as.integer(x)
    days <- .month_to_days(x * n)
    .Date(days)
}

# -------------------------------------------------------------------------
#' @export
as.POSIXct.grates_month <- function(x, tz = "UTC", ...) {
    if (tz != "UTC")
        stop("<grates_month> objects can only be converted to UTC. If other timezones are required, first convert to <Date> and then proceed as desired.")
    n <- attr(x, "n")
    x <- as.integer(x)
    x <- .month_to_days(x * n)
    .POSIXct(x * 86400, tz = "UTC")
}

# -------------------------------------------------------------------------
#' @export
as.POSIXlt.grates_month <- function(x, tz = "UTC", ...) {
    if (tz != "UTC")
        stop("<grates_month> objects can only be converted to UTC. If other timezones are required, first convert to <Date> and then proceed as desired.")
    n <- attr(x, "n")
    x <- as.integer(x)
    x <- .month_to_days(x * n)
    as.POSIXlt(x * 86400, tz = "UTC", origin = .POSIXct(0, tz = "UTC"))
}

# -------------------------------------------------------------------------
#' @export
as.character.grates_month <- function(x, ...) format.grates_month(x)

# -------------------------------------------------------------------------
#' @export
as.list.grates_month <- function(x, ...) {
    lapply(
        as.integer(x),
        function(y) {
            class(y) <- oldClass(x)
            attr(y, "n") <- attr(x, "n")
            y
        }
    )
}

# -------------------------------------------------------------------------
#' @export
as.data.frame.grates_month <- as.data.frame.vector

# -------------------------------------------------------------------------

#' @export
min.grates_month <- function(x, ..., na.rm = FALSE) {
    out <- NextMethod()
    class(out) <- oldClass(x)
    attr(out, "n") <- attr(x, "n")
    out
}

# -------------------------------------------------------------------------
#' @export
max.grates_month <- function(x, ..., na.rm = FALSE) {
    out <- NextMethod()
    class(out) <- oldClass(x)
    attr(out, "n") <- attr(x, "n")
    out
}

# -------------------------------------------------------------------------
#' @export
range.grates_month <- function(x, ..., na.rm = FALSE) {
    out <- NextMethod()
    class(out) <- oldClass(x)
    attr(out, "n") <- attr(x, "n")
    out
}

# -------------------------------------------------------------------------
#' @export
Summary.grates_month <- function(..., na.rm = FALSE) {
    stop(
        sprintf(
            "`%s()` is not supported for <grates_month> objects.",
            .Generic
        )
    )
}

# -------------------------------------------------------------------------
#' @export
Math.grates_month <- function(x, ...) {
    stop(
        sprintf(
            "`%s()` is not supported for <grates_month> objects.",
            .Generic
        )
    )
}

# -------------------------------------------------------------------------
#' @export
quantile.grates_month <- function(x, type = 1, ...) {
    n <- attr(x, "n")
    months <- as.integer(quantile(as.integer(x), type = type, ...))
    .new_month(x = months, n = n)
}

# -------------------------------------------------------------------------
#' @export
Ops.grates_month <- function(e1, e2) {
    op <- .Generic
    if (op %in% c("==", "!=", "<", ">", "<=", ">=")) {
        if (inherits(e2, "grates_month")) {
            n1 <- attr(e1, "n")
            n2 <- attr(e2, "n")
            if (isTRUE(all.equal(n1, n2))) {
                return(NextMethod())
            } else if (op == "==") {
                return(FALSE)
            } else if (op == "!=") {
                return(TRUE)
            } else {
                stop("Can only compare <grates_month> objects with the same month grouping.")
            }
        } else {
            stop("Can only compare <grates_month> objects with <grates_month> objects.")
        }
    }

    switch(
        op,
        "+" = {
            if (missing(e2)) {
                return(e1)
            } else if (inherits(e1, "grates_month") && inherits(e2, "grates_month")) {
                stop("Cannot add <grates_month> objects to each other.")
            } else if (inherits(e1, "grates_month") && (.is_whole(e2))) {
                n <- attr(e1, "n")
                .new_month(as.integer(e1) + as.integer(e2), n = n)
            } else if (inherits(e2, "grates_month") && (.is_whole(e1))) {
                n <- attr(e2, "n")
                .new_month(as.integer(e2) + as.integer(e1), n = n)
            } else {
                stop("Can only add integers to <grates_month> objects.")
            }
        },
        "-" = {
            if (missing(e2)) {
                stop("Cannot negate a <grates_month> object.")
            } else if (inherits(e2, "grates_month")) {
                if (inherits(e1, "grates_month")) {
                    n1 <- attr(e1, "n")
                    n2 <- attr(e2, "n")
                    if (isTRUE(all.equal(n1, n2))) {
                        (as.integer(e1) - as.integer(e2))
                    } else {
                        stop("<grates_month> objects must have the same month grouping to perform subtraction.")
                    }
                } else {
                    stop("Can only subtract from a <grates_month> object, not vice-versa.")
                }
            } else if (inherits(e1, "grates_month") && .is_whole(e2)) {
                n <- attr(e1, "n")
                .new_month(as.integer(e1) - e2, n = n)
            } else {
                stop("Can only subtract whole numbers and other <grates_month> objects from <grates_month> objects.")
            }
        },
        stop(sprintf("%s is not compatible with <grates_month> objects.", op))
    )
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------------------- INTERNALS ------------------------------ #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

.new_month <- function(x = integer(), n) structure(x, n = n, class = "grates_month")
