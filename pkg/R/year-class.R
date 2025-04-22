# -------------------------------------------------------------------------
#' Year class
#'
# -------------------------------------------------------------------------
#' @description
#'
#' Years are represented by a `<grates_year>` object.
#'
# -------------------------------------------------------------------------
#' @details
#'
#' `year()` takes as input a vector representing, unsurprisingly, the years.
#' `double` vectors are coerced via `as.integer(floor(x))`.
#'
#' `as_yearquarter()` is a generic for coercing input in to `<grates_year>`.
#' - Character input is first parsed using `as.Date()`.
#' - POSIXct and POSIXlt are converted with their timezone respected.
#'
# -------------------------------------------------------------------------
#' @param x,object
#'
#' \R objects.
#'
#' @param ...
#'
#' Only used For character input where additional arguments are passed through
#' to `as.Date()`.
#'
# -------------------------------------------------------------------------
#' @return
#' A `<grates_year>` object.
#'
# -------------------------------------------------------------------------
#' @examples
#'
#' # date coercion
#' as_year(Sys.Date())
#'
#' # POSIXt coercion
#' as_year(as.POSIXct("2019-03-04 01:01:01", tz = "America/New_York"))
#'
#' # Character coercion
#' as_year("2019-05-03")
#'
#' # direct construction
#' year(2011:2020)
#'
# -------------------------------------------------------------------------
#' @name year_class
NULL

# -------------------------------------------------------------------------
#' @rdname year_class
#' @export
year <- function(x = integer()) {
    if (is.vector(x, "double")) {
        x <- as.integer(floor(x))
    } else if (!is.integer(x)) {
        stop("`x` must be integer.")
    }

    .new_year(x = x)
}


# -------------------------------------------------------------------------
#' @rdname year_class
#' @export
as_year <- function(x, ...) {
    UseMethod("as_year")
}

# -------------------------------------------------------------------------
#' @rdname year_class
#' @export
as_year.default <- function(x, ...) {
    stopf("Not implemented for class [%s].", toString(class(x)))
}

# -------------------------------------------------------------------------
#' @rdname year_class
#' @export
as_year.Date <- function(x, ...) {

    # convert to posixlt (this will always be UTC when called on a date)
    x <- as.POSIXlt(x)

    # calculate the year
    .new_year(x$year + 1900L)
}

# -------------------------------------------------------------------------
#' @rdname year_class
#' @export
as_year.POSIXt <- function(x, ...) {
    x <- .as_date(x)
    as_year.Date(x)
}

# -------------------------------------------------------------------------
#' @rdname year_class
#' @export
as_year.character <- function(x, ...) {
    out <- as.Date(x, ...)
    if (all(is.na(out)))
        stop("Unable to parse any entries of `x` as Dates.")
    as_year.Date(x = out, ...)
}

# -------------------------------------------------------------------------
#' @rdname year_class
#' @export
as_year.factor <- function(x, ...) {
    x <- as.character(x)
    as_year.character(x, ...)
}


# -------------------------------------------------------------------------
#' @rdname year_class
#' @export
is_year <- function(object) {
    inherits(object, "grates_year")
}

# -------------------------------------------------------------------------
#' @export
print.grates_year <- function(x, ...) {
    # replicate the header as in vctrs
    n <- length(x)
    cat("<grates_year[", n, "]>\n", sep = "")
    if (n)
        print(as.integer(x))
    invisible(x)
}

# -------------------------------------------------------------------------
#' @export
format.grates_year <- function(x, ...) {
    if (length(x) == 0)
        return(character(0))
    class(x) <- NULL
    out <- as.character(x)
    out[is.na(x)] <- NA_character_
    out
}

# -------------------------------------------------------------------------
#' @exportS3Method vctrs::vec_ptype_abbr
vec_ptype_abbr.grates_year <- function(x, ...) {"year"}

#' @exportS3Method vctrs::vec_ptype_full
vec_ptype_full.grates_year <- function(x, ...) {"grates_year"}




#' @export
`[.grates_year` <- function(x, ..., drop = FALSE) {
    out <- NextMethod()
    class(out) <- class(x)
    out
}

# -------------------------------------------------------------------------
#' @export
`[[.grates_year` <- function(x, ..., drop = TRUE) {
    out <- NextMethod()
    class(out) <- class(x)
    out
}

# -------------------------------------------------------------------------
#' @export
`[<-.grates_year` <- function(x, ..., value) {
    if (!inherits(value, "grates_year"))
        stop("Can only assign <grates_year> objects in to an <grates_year> object.")
    out <- NextMethod()
    class(out) <- class(x)
    out
}

# -------------------------------------------------------------------------
#' @export
`[[<-.grates_year` <- `[<-.grates_year`

# -------------------------------------------------------------------------
#' @export
rep.grates_year <- function(x, ...) {
    out <- NextMethod()
    class(out) <- class(x)
    out
}

# -------------------------------------------------------------------------
#' @export
unique.grates_year <- function(x, incomparables = FALSE, ...) {
    out <- NextMethod()
    class(out) <- class(x)
    out
}

# -------------------------------------------------------------------------
#' @export
c.grates_year <- function(..., recursive = FALSE, use.names = TRUE) {
    dots <- list(...)
    if (!all(vapply(dots, inherits, TRUE, what = "grates_year")))
        stop("Unable to combine <grates_year> objects with other classes.")
    res <- NextMethod()
    .new_year(res)
}

# -------------------------------------------------------------------------
#' @export
seq.grates_year <- function(from, to, by = 1L, ...) {

    if (!inherits(to, "grates_year") || length(to) != 1L)
        stop("`to` must be a <grates_year> object of length 1.")

    if (!.is_scalar_whole(by))
        stop("`by` must be an integer of length 1.")

    from <- as.integer(from)
    to <- as.integer(to)
    out <- seq.int(from = from, to = to, by = by)

    # Ensure integer as we cannot rely on seq.int (may return double)
    out <- as.integer(out)
    .new_year(out)
}

# -------------------------------------------------------------------------
#' @export
as.integer.grates_year <- function(x, ...) {
    unclass(x)
}

# -------------------------------------------------------------------------
#' @export
as.double.grates_year <- function(x, ...) {
    as.double(unclass(x))
}

# -------------------------------------------------------------------------
#' @export
as.Date.grates_year <- function(x, ...) {
    days <- .month_to_days((unclass(x) - 1970L) * 12L)
    .Date(as.double(days))
}

# -------------------------------------------------------------------------
#' @export
as.POSIXct.grates_year <- function(x, tz = "UTC", ...) {
    if (tz != "UTC")
        stop("<grates_year> objects can only be converted to UTC. If other timezones are required, first convert to <Date> and then proceed as desired.")
    x <- .month_to_days((unclass(x) - 1970L) * 12L)
    .POSIXct(x * 86400, tz = "UTC")
}

# -------------------------------------------------------------------------
#' @export
as.POSIXlt.grates_year <- function(x, tz = "UTC", ...) {
    if (tz != "UTC")
        stop("<grates_year> objects can only be converted to UTC. If other timezones are required, first convert to <Date> and then proceed as desired.")
    x <- .month_to_days((unclass(x) - 1970L) * 12L)
    as.POSIXlt(.POSIXct(x * 86400, tz = "UTC"), tz = "UTC")
}

# -------------------------------------------------------------------------
#' @export
as.character.grates_year <- function(x, ...) {
    format.grates_year(x)
}

# -------------------------------------------------------------------------
#' @export
as.list.grates_year <- function(x, ...) {
    lapply(unclass(x), `class<-`, class(x))
}

# -------------------------------------------------------------------------
#' @export
as.data.frame.grates_year <- as.data.frame.vector

# -------------------------------------------------------------------------
#' @export
min.grates_year <- function(x, ..., na.rm = FALSE) {
    out <- NextMethod()
    class(out) <- class(x)
    out
}

# -------------------------------------------------------------------------
#' @export
max.grates_year <- function(x, ..., na.rm = FALSE) {
    out <- NextMethod()
    class(out) <- class(x)
    out
}

# -------------------------------------------------------------------------
#' @export
range.grates_year <- function(x, ..., na.rm = FALSE) {
    out <- NextMethod()
    class(out) <- class(x)
    out
}

# -------------------------------------------------------------------------
#' @export
Summary.grates_year <- function(..., na.rm = FALSE) {
    stopf("`%s()` is not supported for <grates_year> objects.", .Generic)
}

# -------------------------------------------------------------------------
#' @export
Math.grates_year <- function(x, ...) {
    stopf("`%s()` is not supported for <grates_year> objects.", .Generic)
}

# -------------------------------------------------------------------------
#' @export
quantile.grates_year <- function(x, type = 1, ...) {
    x <- unclass(x)
    x <- as.integer(quantile(x, type = type, ...))
    .new_year(x)
}

# -------------------------------------------------------------------------
#' @export
Ops.grates_year <- function(e1, e2) {
    op <- .Generic
    if (op %in% c("==", "!=", "<", ">", "<=", ">=")) {
        if (inherits(e2, "grates_year")) {
            return(NextMethod())
        }
        stop("Can only compare <grates_year> objects with <grates_year> objects.")
    }

    switch(
        op,
        "+" = {
            if (missing(e2)) {
                return(e1)
            } else if (inherits(e1, "grates_year") && inherits(e2, "grates_year")) {
                stop("Cannot add <grates_year> objects to each other.")
            } else if (inherits(e1, "grates_year") && (.is_whole(e2))) {
                return(.new_year(unclass(e1) + as.integer(e2)))
            } else if (inherits(e2, "grates_year") && (.is_whole(e1))) {
                return(.new_year(unclass(e2) + as.integer(e1)))
            }
            stop("Can only add integers to <grates_year> objects.")
        },
        "-" = {
            if (missing(e2)) {
                stop("Cannot negate a <grates_year> object.")
            } else if (inherits(e2, "grates_year")) {
                if (!inherits(e1, "grates_year"))
                    stop("Can only subtract from a <grates_year> object, not vice-versa.")
                return(unclass(e1) - unclass(e2))
            } else if (inherits(e1, "grates_year") && is.integer(e2)) {
                return(.new_year(unclass(e1) - e2))
            } else if (inherits(e1, "grates_year") && .is_whole(e2)) {
                return(.new_year(unclass(e1) - as.integer(e2)))
            }
            stop("Can only subtract whole numbers and other <grates_year> objects from <grates_year> objects.")
        },
        stopf("%s is not compatible with <grates_year> objects.", op)
    )
}

# -------------------------------------------------------------------------
#' @export
is.numeric.grates_year <- function(x) {
    FALSE
}

# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------------------- INTERNALS ------------------------------ #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

.new_year <- function(x = integer()) {
    class(x) <- "grates_year"
    x
}
