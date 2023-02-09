# -------------------------------------------------------------------------
#' Minimal constructor for an epiweek object
#'
# -------------------------------------------------------------------------
#' `new_epiweek()` is a constructor for `<grates_epiweek>` objects aimed at
#' developers.
#'
# -------------------------------------------------------------------------
#' Epiweeks are defined to start on a Sunday and `<grates_epiweek>` objects are
#' stored as the number of weeks (starting at 0) from the first Sunday after the
#' Unix Epoch (1970-01-01). That is, the number of seven day periods from
#' 1970-01-04.
#'
#' Internally they have the same representation as a `<grates_yearweek_sunday>`
#' object so are akin to an alias but with a marginally more efficient
#' implementation.
#'
# -------------------------------------------------------------------------
#' @param x `[integer]`
#'
#' Vector representing the number of weeks.
#'
#' `double` vectors will be converted via `as.integer(floor(x))`.
#'
#' @param xx
#'
#' \R object.
#'
# -------------------------------------------------------------------------
#' @return
#' A `<grates_epiweek>` object.
#'
# -------------------------------------------------------------------------
#' @seealso
#' `new_yearweek()` and `new_isoweek()`.
#'
# -------------------------------------------------------------------------
#' @examples
#' new_epiweek(1:10)
#'
# -------------------------------------------------------------------------
#' @export
new_epiweek <- function(x = integer()) {
    if (!is.integer(x)) {
        if (is.vector(x, "double")) {
            x <- as.integer(floor(x))
        } else {
            stop("`x` must be integer.")
        }
    }
    .new_epiweek(x)
}


# -------------------------------------------------------------------------
#' Constructor for epiweek objects
#'
# -------------------------------------------------------------------------
#' `epiweek()` is a constructor for `<grates_epiweek>` objects.
#'
# -------------------------------------------------------------------------
#' Epiweeks are defined to start on a Sunday and `<grates_epiweek>` objects are
#' stored as the number of weeks (starting at 0) from the first Sunday after the
#' Unix Epoch (1970-01-01). That is, the number of seven day periods from
#' 1970-01-04.
#'
#' Internally they have the same representation as a `<grates_yearweek_sunday>`
#' object so are akin to an alias but with a marginally more efficient
#' implementation.
#'
# -------------------------------------------------------------------------
#' @param year `[integer]`
#'
#' Vector representing the year associated with `week`.
#'
#' `double` vectors will be converted via `as.integer(floor(x))`.
#'
#' @param week `[integer]`
#'
#' Vector representing the week associated with `year.
#'
#' `double` vectors will be converted via `as.integer(floor(x))`.
#'
# -------------------------------------------------------------------------
#' @return
#' A `<grates_epiweek>` object.
#'
# -------------------------------------------------------------------------
#' @examples
#' epiweek(year = 2000L, week = 3L)
#'
# -------------------------------------------------------------------------
#' @seealso
#' `as_epiweek()` and `new_epiweek()`.
#'
# -------------------------------------------------------------------------
#' @export
epiweek <- function(year = integer(), week = integer()) {

    # check year is integerish
    if (!is.integer(year)) {
        if (is.vector(year, "double")) {
            year <- as.integer(floor(year))
        } else {
            stop("`year` must be integer.")
        }
    }

    # check week is integerish
    if (!is.integer(week)) {
        if (is.vector(week, "double")) {
            week <- as.integer(floor(week))
        } else {
            stop("`week` must be integer.")
        }
    }

    .epiweek(year = year, week = week)
}

# -------------------------------------------------------------------------
#' @rdname new_epiweek
#' @export
is_epiweek <- function(xx) {
    inherits(xx, "grates_epiweek")
}

# -------------------------------------------------------------------------
#' @export
format.grates_epiweek <- function(x, ...) {
    format.grates_yearweek(x, ...)
}

# -------------------------------------------------------------------------
#' @export
print.grates_epiweek <- function(x, ...) {
    print.grates_yearweek(x, ...)
}

# -------------------------------------------------------------------------
vec_ptype_abbr.grates_epiweek <- function(x, ...) {"epiwk"}
vec_ptype_full.grates_epiweek <- function(x, ...) {"epiweek"}

# -------------------------------------------------------------------------
#' Coerce to a epiweek object
#'
# -------------------------------------------------------------------------
#' Generic for conversion to `<grates_epiweek>`
#'
# -------------------------------------------------------------------------
#' - Date, POSIXct, and POSIXlt are converted with the timezone respected.
#' - Character objects are first coerced to date via `as.Date()` unless
#'   `format = "yearweek"` in which case input is assumed to be in the form
#'   "YYYY-Wxx" and parsed accordingly.
#'
# -------------------------------------------------------------------------
#' @param x
#' \R object.
#'
#' @param format `[character]`
#'
#' Passed to as.Date() unless `format = "yearweek"` in which case input is
#' assumed to be in the form "YYYY-Wxx".
#'
#' If not specified, it will try tryFormats one by one on the first non-NA
#' element, and give an error if none works. Otherwise, the processing is via
#' `strptime()` whose help page describes available conversion specifications.
#'
#' @param tryFormats `[character]`
#'
#' Format strings to try if format is not specified.
#'
#' @param ...
#'
#' Other values passed to as.Date().
#'
# -------------------------------------------------------------------------
#' @return
#' A `<grates_epiweek>` object.
#'
# -------------------------------------------------------------------------
#' @examples
#' as_epiweek(Sys.Date())
#' as_epiweek(as.POSIXct("2019-03-04 01:01:01", tz = "America/New_York"))
#' as_epiweek("2019-05-03")
#' as_epiweek("2019-W12", format = "yearweek")
#'
# -------------------------------------------------------------------------
#' @seealso
#' `new_epiweek()` and `as.Date()`.
#'
# -------------------------------------------------------------------------
#' @export
as_epiweek <- function(x, ...) {
    UseMethod("as_epiweek")
}

# -------------------------------------------------------------------------
#' @rdname as_epiweek
#' @export
as_epiweek.default <- function(x, ...) {
    stopf("Not implemented for class [%s].", paste(class(x), collapse = ", "))
}

# -------------------------------------------------------------------------
#' @rdname as_epiweek
#' @export
as_epiweek.Date <- function(x, ...) {
    firstday <- 7L
    x <- as.integer(floor(unclass(x)))
    x <- (x + 4L - firstday) %/% 7L
    .new_epiweek(x = x)
}

# -------------------------------------------------------------------------
#' @rdname as_epiweek
#' @export
as_epiweek.POSIXt <- function(x, ...) {
    x <- .as_date(x)
    as_epiweek.Date(x)
}

# -------------------------------------------------------------------------
#' @rdname as_epiweek
#' @export
as_epiweek.character <- function(
    x,
    format,
    tryFormats = c("%Y-%m-%d", "%Y/%m/%d"),
    ...
) {
    if (!missing(format)) {
        if (length(format) > 1L)
            stop("If specified, `format` must be of length 1.")

        if (format == "yearweek") {
            years <- sub("^([0-9]{4}).*","\\1", x, perl=TRUE)
            years <- suppressWarnings(as.integer(years))
            weeks <- sub(".*-[wW]([0-9]{1,2}$)","\\1", x, perl=TRUE)
            weeks <- suppressWarnings(as.integer(weeks))
            out <- epiweek(year = years, week = weeks)
            return(out)
        }
    }

    out <- as.Date(x, format = format, tryFormats = tryFormats,...)
    as_epiweek.Date(out)
}

# -------------------------------------------------------------------------
#' @rdname as_epiweek
#' @export
as_epiweek.factor <- function(
    x,
    format,
    tryFormats = c("%Y-%m-%d", "%Y/%m/%d"),
    ...
) {
    x <- as.character(x)
    as_epiweek.character(x, format = format, tryFormats = tryFormats, ...)
}

# -------------------------------------------------------------------------
#' @export
`[.grates_epiweek` <- function(x, ..., drop = FALSE) {
    out <- NextMethod()
    class(out) <- class(x)
    out
}

# -------------------------------------------------------------------------
#' @export
`[[.grates_epiweek` <- function(x, ..., drop = TRUE) {
    out <- NextMethod()
    class(out) <- class(x)
    out
}

# -------------------------------------------------------------------------
#' @export
`[<-.grates_epiweek` <- function(x, ..., value) {
    if (!inherits(value, "grates_epiweek"))
        stop("Can only assign <grates_epiweek> objects in to an <grates_epiweek> object.")
    out <- NextMethod()
    class(out) <- class(x)
    out
}

# -------------------------------------------------------------------------
#' @export
`[[<-.grates_epiweek` <- `[<-.grates_epiweek`

# -------------------------------------------------------------------------
#' @export
rep.grates_epiweek <- function(x, ...) {
    out <- NextMethod()
    class(out) <- class(x)
    out
}

# -------------------------------------------------------------------------
#' @export
unique.grates_epiweek <- function(x, incomparables = FALSE, ...) {
    out <- NextMethod()
    class(out) <- class(x)
    out
}

# -------------------------------------------------------------------------
#' @export
c.grates_epiweek <- function(..., recursive = FALSE, use.names = TRUE) {
    dots <- list(...)
    if (!all(vapply(dots, inherits, TRUE, what = "grates_epiweek")))
        stop("Unable to combine <grates_epiweek> objects with other classes.")
    res <- NextMethod()
    .new_epiweek(res)
}

# -------------------------------------------------------------------------
#' @export
seq.grates_epiweek <- function(from, to, by = 1L, ...) {

    if (!inherits(to, "grates_epiweek") || length(to) != 1L)
        stop("`to` must be a <grates_epiweek> object of length 1.")

    if (!.is_scalar_whole(by))
        stop("`by` must be an integer of length 1.")

    from <- as.integer(from)
    to <- as.integer(to)
    out <- seq.int(from = from, to = to, by = by)

    # Ensure integer as we cannot rely on seq.int (may return double)
    out <- as.integer(out)
    .new_epiweek(out)
}

# -------------------------------------------------------------------------
#' @export
as.integer.grates_yearweek <- function(x, ...) {
    unclass(x)
}

# -------------------------------------------------------------------------
#' @export
as.double.grates_yearweek <- function(x, ...) {
    as.double(unclass(x))
}

# -------------------------------------------------------------------------
#' @export
as.Date.grates_epiweek <- function(x, ...) {
    .Date(as.double(unclass(x)) * 7L + 3L)
}

# -------------------------------------------------------------------------
#' @export
as.POSIXct.grates_epiweek <- function(x, tz = "UTC", ...) {
    if (tz != "UTC")
        stop("<grates_epiweek> objects can only be converted to UTC. If other timezones are required, first convert to <Date> and then proceed as desired.")
    x <- as.double(unclass(x)) * 7 + 3L
    .POSIXct(x * 86400, tz = "UTC")
}

# -------------------------------------------------------------------------
#' @export
as.POSIXlt.grates_epiweek <- function(x, tz = "UTC", ...) {
    if (tz != "UTC")
        stop("<grates_epiweek> objects can only be converted to UTC. If other timezones are required, first convert to <Date> and then proceed as desired.")
    x <- as.double(unclass(x)) * 7L + 3L
    as.POSIXlt(x * 86400, tz = "UTC", origin = .POSIXct(0, tz = "UTC"))
}

# -------------------------------------------------------------------------
#' @export
as.character.grates_epiweek <- function(x, ...) {
    format.grates_epiweek(x)
}

# -------------------------------------------------------------------------
#' @export
as.list.grates_epiweek <- function(x, ...) {
    lapply(unclass(x), `class<-`, class(x))
}

# -------------------------------------------------------------------------
#' @export
as.data.frame.grates_epiweek <- as.data.frame.vector

# -------------------------------------------------------------------------
#' @export
min.grates_epiweek <- function(x, ..., na.rm = FALSE) {
    out <- NextMethod()
    class(out) <- class(x)
    out
}

# -------------------------------------------------------------------------
#' @export
max.grates_epiweek <- function(x, ..., na.rm = FALSE) {
    out <- NextMethod()
    class(out) <- class(x)
    out
}

# -------------------------------------------------------------------------
#' @export
range.grates_epiweek <- function(x, ..., na.rm = FALSE) {
    out <- NextMethod()
    class(out) <- class(x)
    out
}

# -------------------------------------------------------------------------
#' @export
Summary.grates_epiweek <- function(..., na.rm = FALSE) {
    stopf("`%s()` is not supported for <grates_epiweek> objects.", .Generic)
}

# -------------------------------------------------------------------------
#' @export
Math.grates_epiweek <- function(x, ...) {
    stopf("`%s()` is not supported for <grates_epiweek> objects.", .Generic)
}

# -------------------------------------------------------------------------
#' @export
quantile.grates_epiweek <- function(x, type = 1, ...) {
    x <- unclass(x)
    x <- as.integer(quantile(x, type = type, ...))
    new_epiweek(x)
}

# -------------------------------------------------------------------------
#' @export
Ops.grates_epiweek <- function(e1, e2) {
    op <- .Generic

    if (op %in% c("==", "!=", "<", ">", "<=", ">=")) {
        if (inherits(e2, "grates_epiweek")) {
            return(NextMethod())
        } else {
            stop("Can only compare <grates_epiweek> objects with <grates_epiweek> objects.")
        }
    }

    switch(
        op,
        "+" = {
            if (missing(e2)) {
                return(e1)
            } else if (inherits(e1, "grates_epiweek") && inherits(e2, "grates_epiweek")) {
                stop("Cannot add <grates_epiweek> objects to each other.")
            } else if (inherits(e1, "grates_epiweek") && (.is_whole(e2))) {
                return(.new_epiweek(unclass(e1) + as.integer(e2)))
            } else if (inherits(e2, "grates_epiweek") && (.is_whole(e1))) {
                return(.new_epiweek(unclass(e2) + as.integer(e1)))
            } else {
                stop("Can only add integers to <grates_epiweek> objects.")
            }
        },
        "-" = {
            if (missing(e2)) {
                stop("Cannot negate a <grates_epiweek> object.")
            } else if (inherits(e2, "grates_epiweek")) {
                if (inherits(e1, "grates_epiweek")) {
                    weekdiff <- (unclass(e1) - unclass(e2))
                    return(as.difftime(weekdiff, units = "weeks"))
                } else {
                    stop("Can only subtract from a <grates_epiweek> object, not vice-versa.")
                }
            } else if (inherits(e1, "grates_epiweek") && is.integer(e2)) {
                .new_epiweek(unclass(e1) - e2)
            } else if (inherits(e1, "grates_epiweek") && .is_whole(e2)) {
                .new_epiweek(unclass(e1) - as.integer(e2))
            } else {
                stop("Can only subtract whole numbers and other <grates_epiweek> objects from <grates_epiweek> objects.")
            }
        },
        stopf("%s is not compatible with <grates_epiweek> objects.", op)
    )
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------------------- INTERNALS ------------------------------ #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

.new_epiweek <- function(x) {
    structure(x, class = c("grates_epiweek"))
}

.epiweek <- function(year, week) {
    out <- .yearweek(year = year, week = week, firstday = 7L)
    class(out) <- "grates_epiweek"
    out
}
