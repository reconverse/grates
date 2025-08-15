# -------------------------------------------------------------------------
#' Period class
#'
# -------------------------------------------------------------------------
#' @description
#'
#' `<grates_period>` objects represent groupings of `n` consecutive days
#' calculated relative to an `offset`. It is useful for when you wish to group
#' an arbitrary number of dates together (e.g. 10 days).
#'
# -------------------------------------------------------------------------
#' @details
#'
#' Internally `grates_period` objects are stored as the integer number, starting
#' at 0, of  periods since the Unix Epoch (1970-01-01) and a specified offset. Here
#' periods are taken to mean groupings of `n` consecutive days. For storage and
#' calculation purposes, `offset` is scaled relative to `n`
#' (i.e. `offset <- offset %% n`) and values of `x` stored relative to this
#' scaled offset.
#'
#' `as_period()` is a generic for coercing input into `<grates_period>` objects.
#' It is the recommended way for constructing period objects as it allows the
#' `offset` to be specified as a `date` (rather than an integer value relative to
#' the Unix Epoch).
#' - Character input is first parsed using `as.Date()`.
#' - POSIXct and POSIXlt are converted with their timezone respected.
#'
#' `new_period()` is a minimal constructor for `<grates_period>`
#' objects aimed at developers. It takes, as input, the number of periods since
#' the Unix Epoch and the specified `offset`. `double` vectors will
#' be converted via `as.integer(floor(x))`.
#'
#' @param n `[integer]`
#'
#' Number of days that are being grouped by. the number of quarters
#' (starting at 0) since the Unix Epoch, that you wish to represent.
#' `double` vectors will again be converted to integer via `as.integer(floor(x))`.
#'
# -------------------------------------------------------------------------
#' @param x,xx
#'
#' \R objects.
#'
#' For `as_period()` this is the object to be coerced.
#'
#' For `new_period()` this represents the number of periods since the Unix
#' Epoch (1970-01-01) and a specified offset.
#'
#' @param n `[integer]`
#'
#' Number of days that are being grouped.
#'
#' @param offset `[integer]` or, for `as_period()`, a `[date]`.
#'
#' Value you wish to start counting periods from relative to the Unix Epoch:
#' - For integer values this is stored scaled by `n`
#'   (`offset <- as.integer(offset) %% n`).
#' - For date values this is first converted to an integer offset
#'   (`offset <- floor(as.numeric(offset))`) and then scaled via `n` as above.
#'
#' @param ...
#'
#' Only used for character input where additional arguments are passed through
#' to `as.Date()`.
#'
# -------------------------------------------------------------------------
#' @return
#' A `<grates_period>` object.
#'
# -------------------------------------------------------------------------
#' @examples
#'
#' # coercion from date
#' dat <- as.Date("2012-12-01")
#' as_period(dat + 0:3, n = 2, offset = dat)
#'
#' # direct construction
#' new_period(1:10)
#'
# -------------------------------------------------------------------------
#' @name period_class
NULL

# -------------------------------------------------------------------------
#' @rdname period_class
#' @export
as_period <- function(x, n, ...) {
    UseMethod("as_period")
}

# -------------------------------------------------------------------------
#' @rdname period_class
#' @export
as_period.default <- function(x, n = 1L, offset, ...) {
    stopf("Not implemented for class [%s].", toString(class(x)))
}

# -------------------------------------------------------------------------
#' @rdname period_class
#' @export
as_period.Date <- function(x, n = 1L, offset = min(x, na.rm = TRUE), ...) {

    if (...length()) {
        dot_names <- names(list(...))
        if (any(dot_names == "origin"))
            stop("The `origin` argument is now defunct. Please use `offset`.")
    }

    x <- as.integer(floor(unclass(x)))
    if (!.is_scalar_whole(n))
        stop("`n` must be an integer of length 1.")
    n <- as.integer(n)

    if (inherits(offset, "Date"))
        offset <- floor(as.numeric(offset))

    if (!.is_scalar_whole(offset))
        stop("`offset` must be an integer or date of length 1.")

    offset <- as.integer(offset) %% n
    x <- (x - offset) %/% n
    .new_period(x = x, n = n, offset = offset)
}

# -------------------------------------------------------------------------
#' @rdname period_class
#' @export
as_period.POSIXt <- function(x, n = 1L,  offset = min(x, na.rm = TRUE), ...) {

    if (...length()) {
        dot_names <- names(list(...))
        if (any(dot_names == "origin"))
            stop("The `origin` argument is now defunct. Please use `offset`.")
    }

    x <- .as_date(x)
    as_period.Date(x = x, n = n, offset = offset)
}

# -------------------------------------------------------------------------
#' @rdname period_class
#' @export
as_period.character <- function(x, n = 1L, offset, ...) {

    if (...length()) {
        dot_names <- names(list(...))
        if (any(dot_names == "origin"))
            stop("The `origin` argument is now defunct. Please use `offset`.")
    }

    out <- as.Date(x, ...)
    if (all(is.na(out)))
        stop("Unable to parse any entries of `x` as Dates.")
    as_period.Date(x = out, n = n, offset = offset)
}

# -------------------------------------------------------------------------
#' @rdname period_class
#' @export
as_period.factor <- function(x, n = 1L, offset, ...) {
    if (...length()) {
        dot_names <- names(list(...))
        if (any(dot_names == "origin"))
            stop("The `origin` argument is now defunct. Please use `offset`.")
    }
    x <- as.character(x)
    as_period.character(x, n = n, offset = offset, ...)
}


# -------------------------------------------------------------------------
#' @rdname period_class
#' @export
new_period <- function(x = integer(), n = 1L, offset = 0L) {
    x <- .make_floored_integer(x)

    if (!.is_scalar_whole(n))
        stop("`n` must be an integer of length 1.")
    n <- as.integer(n)
    if (n < 1L)
        stop("`n` must be an integer >= 1.")

    if (!.is_scalar_whole(offset))
        stop("`offset` must be an integer of length 1.")
    offset <- as.integer(offset) %% n

    .new_period(x = x, n = n, offset = offset)
}

# -------------------------------------------------------------------------
#' @rdname period_class
#' @export
is_period <- function(xx) {
    inherits(xx, "grates_period")
}

# -------------------------------------------------------------------------
#' Print a period object
#'
# -------------------------------------------------------------------------
#' @param x
#'
#' A `<grates_period>` object.
#'
#' @param format `[character]`
#'
#' The format to use for the bounds of each value.
#'
#' @param sep `[character]`
#'
#' Where more than one day is grouped with others, `sep` is placed between the
#' upper and lower bounds when printing.
#'
#' @param ...
#' Not currently used.
#'
#' @return
#' For `format()`, a character vector representing the formatted input.
#' `print()` is called for the side effect of printing to screen and thus
#' returns the input `<grates_period>` object invisibly.
#'
# -------------------------------------------------------------------------
#' @export
print.grates_period <- function(x, format = "%Y-%m-%d", sep = "to", ...) {
    # replicate the header as in vctrs
    n <- length(x)
    cat("<grates_period[", n, "]>\n", sep = "")
    if (n)
        print(format.grates_period(x, format = format, sep = sep))
    invisible(x)
}

# -------------------------------------------------------------------------
#' @rdname print.grates_period
#' @export
format.grates_period <- function(x, format = "%Y-%m-%d", sep = "to", ...) {
    if (length(x) == 0L) return(character(0L))
    n <- attr(x, "n")
    offset <- attr(x, "offset")
    if (n > 1L) {
        out <- sprintf(
            "%s %s %s",
            format.Date(as.Date(x), format = format),
            sep,
            format.Date(as.Date(x + 1L) - 1L, format = format)
        )
    } else {
        out <- format.Date(as.Date(x + offset), format = format)
    }
    out[is.na(x)] <- NA_character_
    out
}

# -------------------------------------------------------------------------
#' @exportS3Method vctrs::vec_ptype_abbr
vec_ptype_abbr.grates_period <- function(x, ...) {"period"}

#' @exportS3Method vctrs::vec_ptype_full
vec_ptype_full.grates_period <- function(x, ...) {"grates_period"}




#' @export
`[.grates_period` <- function(x, ..., drop = FALSE) {
    out <- NextMethod()
    class(out) <- class(x)
    attr(out, "n") <- attr(x, "n")
    attr(out, "offset") <- attr(x, "offset")
    out
}

# -------------------------------------------------------------------------
#' @export
`[[.grates_period` <- function(x, ..., drop = TRUE) {
    out <- NextMethod()
    class(out) <- class(x)
    attr(out, "n") <- attr(x, "n")
    attr(out, "offset") <- attr(x, "offset")
    out
}

# -------------------------------------------------------------------------
#' @export
`[<-.grates_period` <- function(x, ..., value) {
    if (!inherits(value, "grates_period"))
        stop("Can only assign a <grates_period> object into a <grates_period> object.")

    nx <- attr(x, "n")
    nv <- attr(value, "n")
    if (isTRUE(nx != nv))
        stop("Incompatible period groupings.")

    offsetx <- attr(x, "offset")
    offsetv <- attr(value, "offset")
    if (isTRUE(offsetx != offsetv))
        stop("Incompatible offsets.")

    out <- NextMethod()
    class(out) <- class(x)
    attr(out, "n") <- nx
    attr(out, "offset") <- offsetx
    out
}

# -------------------------------------------------------------------------
#' @export
`[[<-.grates_period` <- `[<-.grates_period`

# -------------------------------------------------------------------------
#' @export
rep.grates_period <- function(x, ...) {
    out <- NextMethod()
    class(out) <- class(x)
    attr(out, "n") <- attr(x, "n")
    attr(out, "offset") <- attr(x, "offset")
    out
}

# -------------------------------------------------------------------------
#' @export
unique.grates_period <- function(x, incomparables = FALSE, ...) {
    out <- NextMethod()
    class(out) <- class(x)
    attr(out, "n") <- attr(x, "n")
    attr(out, "offset") <- attr(x, "offset")
    out
}

# -------------------------------------------------------------------------
#' @export
c.grates_period <- function(..., recursive = FALSE, use.names = TRUE) {
    dots <- list(...)
    if (!all(vapply(dots, inherits, TRUE, what = "grates_period")))
        stop("Unable to combine <grates_period> objects with other classes.")

    ns <- vapply(dots, attr, 1L, "n")
    if (length(unique(ns)) != 1L)
        stop("Unable to combine <grates_period> objects with different groupings.")

    offsets <- vapply(dots, attr, 1L, "offset")
    if (length(unique(offsets)) != 1L)
        stop("Unable to combine <grates_period> objects with different offsets.")

    res <- NextMethod()
    .new_period(x = res, n = ns[[1L]], offset = offsets[[1L]])
}

# -------------------------------------------------------------------------
#' @export
seq.grates_period <- function(from, to, by = 1L, ...) {

    if (!inherits(to, "grates_period") || length(to) != 1L)
        stop("`to` must be a <grates_period> object of length 1.")

    if (!.is_scalar_whole(by))
        stop("`by` must be an integer of length 1.")

    fn <- attr(from, "n")
    tn <- attr(to, "n")
    if (fn != tn)
        stop("`to` must have the same period grouping as `from`.")

    foffset <- attr(from, "offset")
    toffset <- attr(to, "offset")
    if (foffset != toffset)
        stop("`to` must have the same offset as `from`.")

    from <- as.integer(from)
    to <- as.integer(to)
    out <- seq.int(from = from, to = to, by = by)

    # Ensure integer as we cannot rely on seq.int (may return double)
    out <- as.integer(out)
    .new_period(x = out, n = tn, offset = toffset)
}

# -------------------------------------------------------------------------
#' @export
as.Date.grates_period <- function(x, ...) {
    n <- attr(x, "n")
    offset <- attr(x, "offset")
    days <- as.integer(x) * n + offset
    .Date(as.double(days))
}

# -------------------------------------------------------------------------
#' @export
as.POSIXct.grates_period <- function(x, tz = "UTC", ...) {
    if (tz != "UTC")
        stop(
            "<grates_period> objects can only be converted to UTC. ",
            "If other timezones are required, first convert to <Date> and then proceed as desired."
        )
    n <- attr(x, "n")
    offset <- attr(x, "offset")
    days <- as.integer(x) * n + offset
    .POSIXct(days * 86400, tz = "UTC")
}

# -------------------------------------------------------------------------
#' @export
as.POSIXlt.grates_period <- function(x, tz = "UTC", ...) {
    if (tz != "UTC")
        stop(
            "<grates_period> objects can only be converted to UTC. ",
            "If other timezones are required, first convert to <Date> and then proceed as desired."
        )
    n <- attr(x, "n")
    offset <- attr(x, "offset")
    days <- as.integer(x) * n + offset
    as.POSIXlt(.POSIXct(days * 86400, tz = "UTC"), tz = "UTC")
}

# -------------------------------------------------------------------------
#' @export
as.character.grates_period <- function(x, ...) {
    format.grates_period(x)
}

# -------------------------------------------------------------------------
#' @export
as.list.grates_period <- function(x, ...) {
    lapply(
        as.integer(x),
        function(y) {
            class(y) <- class(x)
            attr(y, "n") <- attr(x, "n")
            attr(y, "offset") <- attr(x, "offset")
            y
        }
    )
}

# -------------------------------------------------------------------------
#' @export
as.data.frame.grates_period <- as.data.frame.vector

# -------------------------------------------------------------------------
#' @export
min.grates_period <- function(x, ..., na.rm = FALSE) {
    out <- NextMethod()
    class(out) <- class(x)
    attr(out, "n") <- attr(x, "n")
    attr(out, "offset") <- attr(x, "offset")
    out
}

# -------------------------------------------------------------------------
#' @export
max.grates_period <- function(x, ..., na.rm = FALSE) {
    out <- NextMethod()
    class(out) <- class(x)
    attr(out, "n") <- attr(x, "n")
    attr(out, "offset") <- attr(x, "offset")
    out
}

# -------------------------------------------------------------------------
#' @export
range.grates_period <- function(x, ..., na.rm = FALSE) {
    out <- NextMethod()
    class(out) <- class(x)
    attr(out, "n") <- attr(x, "n")
    attr(out, "offset") <- attr(x, "offset")
    out
}

# -------------------------------------------------------------------------
#' @export
Summary.grates_period <- function(..., na.rm = FALSE) {
    stopf("`%s()` is not supported for <grates_period> objects.", .Generic)
}

# -------------------------------------------------------------------------
#' @export
Math.grates_period <- function(x, ...) {
    stopf("`%s()` is not supported for <grates_period> objects.", .Generic)
}

# -------------------------------------------------------------------------
#' @export
quantile.grates_period <- function(x, type = 1, ...) {
    n <- attr(x, "n")
    offset <- attr(x, "offset")
    periods <- as.integer(quantile(as.integer(x), type = type, ...))
    .new_period(x = periods, n = n, offset = offset)
}

# -------------------------------------------------------------------------
#' @export
Ops.grates_period <- function(e1, e2) {
    op <- .Generic
    if (op %in% c("==", "!=", "<", ">", "<=", ">=")) {
        if (inherits(e2, "grates_period")) {
            n1 <- attr(e1, "n")
            n2 <- attr(e2, "n")
            offset1 <- attr(e1, "offset")
            offset2 <- attr(e2, "offset")
            if (isTRUE(all.equal(n1, n2)) && isTRUE(all.equal(offset1, offset2))) {
                return(NextMethod())
            } else if (op == "==") {
                return(FALSE)
            } else if (op == "!=") {
                return(TRUE)
            }
            stop("Can only compare <grates_period> objects with the same period grouping and offset.") # nolint: line_length_linter
        }
        stop("Can only compare <grates_period> objects with <grates_period> objects.")
    }

    switch(
        op,
        "+" = {
            if (missing(e2)) {
                return(e1)
            } else if (inherits(e1, "grates_period") && inherits(e2, "grates_period")) {
                stop("Cannot add <grates_period> objects to each other.")
            } else if (inherits(e1, "grates_period") && (.is_whole(e2))) {
                n <- attr(e1, "n")
                offset <- attr(e1, "offset")
                return(.new_period(as.integer(e1) + as.integer(e2), n = n, offset = offset))
            } else if (inherits(e2, "grates_period") && (.is_whole(e1))) {
                n <- attr(e2, "n")
                offset <- attr(e2, "offset")
                return(.new_period(as.integer(e2) + as.integer(e1), n = n, offset = offset))
            }
            stop("Can only add integers to <grates_period> objects.")
        },
        "-" = {
            if (missing(e2)) {
                stop("Cannot negate a <grates_period> object.")
            } else if (inherits(e2, "grates_period")) {
                if (inherits(e1, "grates_period")) {
                    n1 <- attr(e1, "n")
                    n2 <- attr(e2, "n")
                    offset1 <- attr(e1, "offset")
                    offset2 <- attr(e2, "offset")
                    if (isTRUE(all.equal(n1, n2)) && isTRUE(all.equal(offset1, offset2))) {
                        return((as.integer(e1) - as.integer(e2)))
                    }
                    stop("<grates_period> objects must have the same period grouping and offset to perform subtraction.") # nolint: line_length_linter.
                }
                stop("Can only subtract from a <grates_period> object, not vice-versa.")
            } else if (inherits(e1, "grates_period") && .is_whole(e2)) {
                n <- attr(e1, "n")
                offset <- attr(e1, "offset")
                return(.new_period(as.integer(e1) - e2, n = n, offset = offset))
            }
            stop("Can only subtract whole numbers and other <grates_period> objects from <grates_period> objects.") # nolint: line_length_linter.
        },
        stopf("%s is not compatible with <grates_period> objects.", op)
    )
}

# -------------------------------------------------------------------------
#' @export
is.numeric.grates_period <- function(x) {
    FALSE
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------------------- INTERNALS ------------------------------ #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

.new_period <- function(x = integer(), n, offset) {
    structure(x, n = n, offset = offset, class = "grates_period")
}
