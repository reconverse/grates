# -------------------------------------------------------------------------
#' Integer-period class (Experimental)
#'
# -------------------------------------------------------------------------
#' @description
#'
#' `<grates_int_period>` objects represent groupings of `n` consecutive integers
#' from `0`.
#'
# -------------------------------------------------------------------------
#' @details
#'
#' `as_int_period()` is a generic for coercing input into `<grates_int_period>`
#' objects. For numeric input it coerces it's input `x` first via
#' `x <- as.integer(floor(x))` and then via integer division by `n` (i.e.
#' `x %/% n`).
#'
#' `new_int_period()` is a minimal constructor for `<grates_period>`
#' objects aimed at developers. It takes, as input, the number of integer
#' periods and the value of `n`.
#'
# -------------------------------------------------------------------------
#' @param x,xx
#'
#' \R objects.
#'
#' For `as_int_period()` this is the object to be coerced.
#'
#' For `new_int_period()` this represents the number of `n` integer periods from 0.
#'
#' `double` vectors will be converted via `as.integer(floor(x))`.
#'
#' @param n `[integer]`
#'
#' Number of integers that are being grouped. Must be greater than 0.
#'
#' @param ...
#'
#' Not currently used.
#'
# -------------------------------------------------------------------------
#' @return
#' A `<grates_int_period>` object.
#'
# -------------------------------------------------------------------------
#' @examples
#'
#' # coercion
#' as_int_period(1:10, n = 3)
#'
#' # direct construction
#' stopifnot(
#'     identical(
#'         as_int_period(1:10, n = 3),
#'         new_int_period(c(0, 0, 1, 1, 1, 2, 2, 2, 3, 3), n = 3)
#'     )
#' )
#'
#'
#'
# -------------------------------------------------------------------------
#' @name int_period_class
NULL

# -------------------------------------------------------------------------
#' @rdname int_period_class
#' @export
as_int_period.default <- function(x, n = 1L, ...) {
    stopf("Not implemented for class [%s].", toString(class(x)))
}

# -------------------------------------------------------------------------
#' @rdname int_period_class
#' @export
as_int_period.integer <- function(x, n = 1L, ...) {

    # trigger warning for missing n at top level
    n <- n

    if (!.is_scalar_whole(n))
        stop("`n` must be an integer of length 1.")
    n <- as.integer(n)
    if (n < 1L)
        stop("`n` must be greater than 0.")

    # scale by n
    x <- (x %/% n)

    .new_int_period(x = x, n = n)
}

# -------------------------------------------------------------------------
#' @rdname int_period_class
#' @export
as_int_period.double <- function(x, n = 1L, ...) {

    x <- as.integer(floor(x))

    # trigger warning for missing n at top level
    n <- n

    if (!.is_scalar_whole(n))
        stop("`n` must be an integer of length 1.")
    n <- as.integer(n)
    if (n < 1L)
        stop("`n` must be greater than 0.")

    # scale by n
    x <- (x %/% n)

    .new_int_period(x = x, n = n)
}


# -------------------------------------------------------------------------
#' @rdname int_period_class
#' @export
new_int_period <- function(x = integer(), n = 1L) {
    if (is.vector(x, "double")) {
        x <- as.integer(floor(x))
    } else if (!is.integer(x)) {
        stop("`x` must be integer.")
    }

    # trigger warning for missing n at top level
    n <- n
    if (!.is_scalar_whole(n))
        stop("`n` must be an integer of length 1.")
    n <- as.integer(n)
    if (n < 1L)
        stop("`n` must be greater than 0.")

    .new_int_period(x = x, n = n)
}

# -------------------------------------------------------------------------
#' @rdname int_period_class
#' @export
is_int_period <- function(xx) {
    inherits(xx, "grates_int_period")
}


# -------------------------------------------------------------------------
#' @export
print.grates_int_period <- function(x, ...) {
    # replicate the header as in vctrs
    n <- length(x)
    cat("<grates_int_period[", n, "]>\n", sep = "")
    if (n)
        print(format.grates_int_period(x), quote = FALSE)
    invisible(x)
}

# -------------------------------------------------------------------------
#' @export
format.grates_int_period <- function(x, ...) {
    if (length(x) == 0L) return(character(0L))
    n <- get_n(x)
    if (n != 1) {
        out <- sprintf("[%d, %d]", as.integer(x), as.integer(x + 1L) - 1L)
    } else {
        out <- as.character(as.integer(x))
    }
    out[is.na(x)] <- NA_character_
    out
}

# -------------------------------------------------------------------------
#' @exportS3Method vctrs::vec_ptype_abbr
vec_ptype_abbr.grates_int_period <- function(x, ...) {"intper"}

#' @exportS3Method vctrs::vec_ptype_full
vec_ptype_full.grates_int_period <- function(x, ...) {"grates_int_period"}


# -------------------------------------------------------------------------
#' @rdname int_period_class
#' @export
as_int_period <- function(x, n, ...) {
    UseMethod("as_int_period")
}


# -------------------------------------------------------------------------
#' @export
`[.grates_int_period` <- function(x, ..., drop = FALSE) {
    out <- NextMethod()
    class(out) <- class(x)
    attr(out, "n") <- attr(x, "n")
    out
}

# -------------------------------------------------------------------------
#' @export
`[[.grates_int_period` <- function(x, ..., drop = TRUE) {
    out <- NextMethod()
    class(out) <- class(x)
    attr(out, "n") <- attr(x, "n")
    out
}

# -------------------------------------------------------------------------
#' @export
`[<-.grates_int_period` <- function(x, ..., value) {
    if (!inherits(value, "grates_int_period"))
        stop("Can only assign a <grates_int_period> object into a <grates_int_period> object.")
    nx <- attr(x, "n")
    nv <- attr(value, "n")
    if (isTRUE(nx != nv))
        stop("Incompatible integer groupings.")
    out <- NextMethod()
    class(out) <- class(x)
    attr(out, "n") <- nx
    out
}

# -------------------------------------------------------------------------
#' @export
`[[<-.grates_int_period` <- `[<-.grates_int_period`

# -------------------------------------------------------------------------
#' @export
rep.grates_int_period <- function(x, ...) {
    out <- NextMethod()
    class(out) <- class(x)
    attr(out, "n") <- attr(x, "n")
    out
}

# -------------------------------------------------------------------------
#' @export
unique.grates_int_period <- function(x, incomparables = FALSE, ...) {
    out <- NextMethod()
    class(out) <- class(x)
    attr(out, "n") <- attr(x, "n")
    out
}

# -------------------------------------------------------------------------
#' @export
c.grates_int_period <- function(..., recursive = FALSE, use.names = TRUE) {
    dots <- list(...)
    if (!all(vapply(dots, inherits, TRUE, what = "grates_int_period")))
        stop("Unable to combine <grates_int_period> objects with other classes.")
    ns <- vapply(dots, attr, 1L, "n")
    if (length(unique(ns)) != 1L)
        stop("Unable to combine <grates_int_period> objects with different groupings.")
    res <- NextMethod()
    .new_int_period(x = res, n = ns[[1L]])
}

# -------------------------------------------------------------------------
#' @export
seq.grates_int_period <- function(from, to, by = 1L, ...) {

    if (!inherits(to, "grates_int_period") || length(to) != 1L)
        stop("`to` must be a <grates_int_period> object of length 1.")

    if (!.is_scalar_whole(by))
        stop("`by` must be an integer of length 1.")

    fn <- attr(from, "n")
    tn <- attr(to, "n")
    if (fn != tn)
        stop("`to` must have the same integer grouping as `from`")

    from <- unclass(from)
    to <- unclass(to)
    out <- seq.int(from = from, to = to, by = by)

    # Ensure integer as we cannot rely on seq.int (may return double)
    out <- as.integer(out)
    .new_int_period(x = out, n = tn)
}

# -------------------------------------------------------------------------
#' @export
as.integer.grates_int_period <- function(x, ...) {
    n <- attr(x, "n")
    x <- unclass(x)
    as.integer(x * n)
}

# -------------------------------------------------------------------------
#' @export
as.Date.grates_int_period <- function(x, ...) {
    stopf("`%s()` is not supported for <grates_int_period> objects.", .Generic)
}

# -------------------------------------------------------------------------
#' @export
as.POSIXct.grates_int_period <- function(x, tz = "", ...) {
    stopf("`%s()` is not supported for <grates_int_period> objects.", .Generic)
}

# -------------------------------------------------------------------------
#' @export
as.POSIXlt.grates_int_period <- as.POSIXct.grates_int_period

# -------------------------------------------------------------------------
#' @export
as.character.grates_int_period <- function(x, ...) {
    format.grates_int_period(x)
}

# -------------------------------------------------------------------------
#' @export
as.list.grates_int_period <- function(x, ...) {
    lapply(
        unclass(x),
        function(y) {
            class(y) <- class(x)
            attr(y, "n") <- attr(x, "n")
            y
        }
    )
}

# -------------------------------------------------------------------------
#' @export
as.data.frame.grates_int_period <- as.data.frame.vector

# -------------------------------------------------------------------------

#' @export
min.grates_int_period <- function(x, ..., na.rm = FALSE) {
    out <- NextMethod()
    class(out) <- class(x)
    attr(out, "n") <- attr(x, "n")
    out
}

# -------------------------------------------------------------------------
#' @export
max.grates_int_period <- function(x, ..., na.rm = FALSE) {
    out <- NextMethod()
    class(out) <- class(x)
    attr(out, "n") <- attr(x, "n")
    out
}

# -------------------------------------------------------------------------
#' @export
range.grates_int_period <- function(x, ..., na.rm = FALSE) {
    out <- NextMethod()
    class(out) <- class(x)
    attr(out, "n") <- attr(x, "n")
    out
}

# -------------------------------------------------------------------------
#' @export
Summary.grates_int_period <- function(..., na.rm = FALSE) {
    stopf("`%s()` is not supported for <grates_int_period> objects.", .Generic)
}

# -------------------------------------------------------------------------
#' @export
Math.grates_int_period <- function(x, ...) {
    stopf("`%s()` is not supported for <grates_int_period> objects.", .Generic)
}

# -------------------------------------------------------------------------
#' @export
quantile.grates_int_period <- function(x, type = 1, ...) {
    n <- attr(x, "n")
    x <- as.integer(quantile(unclass(x), type = type, ...))
    .new_int_period(x = x, n = n)
}

# -------------------------------------------------------------------------
#' @export
Ops.grates_int_period <- function(e1, e2) {
    op <- .Generic
    if (op %in% c("==", "!=", "<", ">", "<=", ">=")) {
        if (inherits(e2, "grates_int_period")) {
            n1 <- attr(e1, "n")
            n2 <- attr(e2, "n")
            if (isTRUE(all.equal(n1, n2))) {
                return(NextMethod())
            } else if (op == "==") {
                return(FALSE)
            } else if (op == "!=") {
                return(TRUE)
            }
            stop("Can only compare <grates_int_period> objects with the same integer grouping.")
        }
        stop("Can only compare <grates_int_period> objects with <grates_int_period> objects.")
    }

    switch(
        op,
        "+" = {
            if (missing(e2)) {
                return(e1)
            } else if (inherits(e1, "grates_int_period") && inherits(e2, "grates_int_period")) {
                stop("Cannot add <grates_int_period> objects to each other.")
            } else if (inherits(e1, "grates_int_period") && (.is_whole(e2))) {
                n <- attr(e1, "n")
                e2 <- as.integer(e2)
                return(.new_int_period(unclass(e1) + unclass(e2), n = n))
            } else if (inherits(e2, "grates_int_period") && (.is_whole(e1))) {
                n <- attr(e2, "n")
                e1 <- as.integer(e1)
                return(.new_int_period(unclass(e2) + unclass(e1), n = n))
            }
            stop("Can only add integers to <grates_int_period> objects.")
        },
        "-" = {
            if (missing(e2)) {
                stop("Cannot negate a <grates_int_period> object.")
            } else if (inherits(e2, "grates_int_period")) {
                if (inherits(e1, "grates_int_period")) {
                    n1 <- attr(e1, "n")
                    n2 <- attr(e2, "n")
                    if (isTRUE(all.equal(n1, n2))) {
                        return(c(unclass(e1) - unclass(e2)))
                    }
                    stop("<grates_int_period> objects must have the same integer grouping to perform subtraction.") # nolint: line_length_linter.
                }
                stop("Can only subtract from a <grates_int_period> object, not vice-versa.")
            } else if (inherits(e1, "grates_int_period") && .is_whole(e2)) {
                e2 <- as.integer(e2)
                n <- attr(e1, "n")
                return(.new_int_period(unclass(e1) - e2, n = n))
            }
            stop("Can only subtract whole numbers and other <grates_int_period> objects from <grates_int_period> objects.") # nolint: line_length_linter.
        },
        stopf("%s is not compatible with <grates_int_period> objects.", op)
    )
}

# -------------------------------------------------------------------------
#' @export
is.numeric.grates_int_period <- function(x) {
    FALSE
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------------------- INTERNALS ------------------------------ #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

.new_int_period <- function(x = integer(), n) {
    structure(x, n = n, class = "grates_int_period")
}
