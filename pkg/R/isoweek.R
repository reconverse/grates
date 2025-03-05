# -------------------------------------------------------------------------
#' Isoweek class
#'
# -------------------------------------------------------------------------
#' @description
#'
#' Isoweeks are defined to start on a Monday and span a 7 day period.
#' Where they span calendar years, they are associated to the year which
#' contains the majority of the week's days (i.e. the first isoweek a year
#' is the one with at least four days in said year).
#'
#' Internally, `<grates_isoweek>` objects are stored as the number of weeks
#' (starting at 0) from the first Monday prior to the Unix Epoch (1970-01-01).
#' That is, the number of seven day periods from 1969-12-29.
#'
# -------------------------------------------------------------------------
#' @details
#'
#' `isoweek()` is a constructor for `<grates_isoweek>` objects. It takes a
#' vector of year and vector of week values as inputs. Length 1 inputs will be
#' recycled to the length of the other input and `double` vectors will again be
#' converted to integer via `as.integer(floor(x))`.
#'
#' `as_isoweek()` is a generic for conversion to `<grates_isoweek>`.
#' - Date, POSIXct, and POSIXlt are converted with the timezone respected.
#' - Character objects are first coerced to date via `as.Date()` unless
#' `format = "yearweek"` in which case input is assumed to be in the form
#' "YYYY-Wxx" and parsed accordingly.
#'
#' `new_isoweek()` is a minimal constructor for `<grates_isoweek>` objects
#' aimed at developers. It takes, as input, the number of isoweeks since the
#' Monday prior to the Unix Epoch that you wish to represent. `double` vectors
#' will be converted to integer via `as.integer(floor(x))`.
#'
# -------------------------------------------------------------------------
#' @param x
#'
#' An \R object.
#'
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
#'
#' @param xx
#'
#' An \R object.
#'
# -------------------------------------------------------------------------
#' @return
#' A `<grates_isoweek>` object.
#'
# -------------------------------------------------------------------------
#' @seealso
#' `new_yearweek()` and `new_epiweek()`.
#'
# -------------------------------------------------------------------------
#' @examples
#' isoweek(year = 2000, week = 3)
#' new_isoweek(1:10)
#' as_isoweek(Sys.Date())
#' as_isoweek(as.POSIXct("2019-03-04 01:01:01", tz = "America/New_York"))
#' as_isoweek("2019-05-03")
#' as_isoweek("2019-W12", format = "yearweek")
#'
# -------------------------------------------------------------------------
#' @name isoweek_class
NULL

# -------------------------------------------------------------------------
#' @rdname isoweek_class
#' @export
isoweek <- function(year = integer(), week = integer()) {

    # check year is integerish
    if (is.vector(year, "double")) {
        year <- as.integer(floor(year))
    } else if (!is.integer(year)) {
        stop("`year` must be integer.")
    }

    # check week is integerish
    if (is.vector(week, "double")) {
        week <- as.integer(floor(week))
    } else if (!is.integer(week)) {
        stop("`week` must be integer.")
    }

    # check compatible lengths
    tmp <- .recycle(year, week)
    year <- tmp[[1L]]
    week <- tmp[[2L]]

    .isoweek(year = year, week = week)
}

# -------------------------------------------------------------------------
#' @rdname isoweek_class
#' @export
as_isoweek <- function(x, ...) {
    UseMethod("as_isoweek")
}

# -------------------------------------------------------------------------
#' @rdname isoweek_class
#' @export
as_isoweek.default <- function(x, ...) {
    stopf("Not implemented for class [%s].", toString(class(x)))
}

# -------------------------------------------------------------------------
#' @rdname isoweek_class
#' @export
as_isoweek.Date <- function(x, ...) {
    firstday <- 1L
    x <- as.integer(floor(unclass(x)))
    x <- (x + 4L - firstday) %/% 7L
    .new_isoweek(x = x)
}

# -------------------------------------------------------------------------
#' @rdname isoweek_class
#' @export
as_isoweek.POSIXt <- function(x, ...) {
    x <- .as_date(x)
    as_isoweek.Date(x)
}

# -------------------------------------------------------------------------
#' @rdname isoweek_class
#' @export
as_isoweek.character <- function(
        x,
        format,
        tryFormats = c("%Y-%m-%d", "%Y/%m/%d"),
        ...
) {
    if (!missing(format)) {
        if (length(format) > 1L)
            stop("If specified, `format` must be of length 1.")

        if (format == "yearweek") {
            years <- sub("^([0-9]{4}).*", "\\1", x, perl = TRUE)
            years <- suppressWarnings(as.integer(years))
            weeks <- sub(".*-[wW]([0-9]{1,2}$)", "\\1", x, perl = TRUE)
            weeks <- suppressWarnings(as.integer(weeks))
            out <- isoweek(year = years, week = weeks)
            return(out)
        }
    }
    out <- as.Date(x, format = format, tryFormats = tryFormats, ...)
    as_isoweek.Date(out)
}

# -------------------------------------------------------------------------
#' @rdname isoweek_class
#' @export
as_isoweek.factor <- function(
        x,
        format,
        tryFormats = c("%Y-%m-%d", "%Y/%m/%d"),
        ...
) {
    x <- as.character(x)
    as_isoweek.character(x, format = format, tryFormats = tryFormats, ...)
}

# -------------------------------------------------------------------------
#' @rdname isoweek_class
#' @export
new_isoweek <- function(x = integer()) {
    if (is.vector(x, "double")) {
        x <- as.integer(floor(x))
    } else if (!is.integer(x)) {
        stop("`x` must be integer.")
    }
    .new_isoweek(x)
}


# -------------------------------------------------------------------------
#' @rdname isoweek_class
#' @export
is_isoweek <- function(xx) {
    inherits(xx, "grates_isoweek")
}

# -------------------------------------------------------------------------
#' @export
format.grates_isoweek <- function(x, ...) {
    format.grates_yearweek(x, ...)
}

# -------------------------------------------------------------------------
#' @export
print.grates_isoweek <- function(x, ...) {
    print.grates_yearweek(x, ...)
}

# -------------------------------------------------------------------------
#' @exportS3Method vctrs::vec_ptype_abbr
vec_ptype_abbr.grates_isoweek <- function(x, ...) {"isowk"}

#' @exportS3Method vctrs::vec_ptype_full
vec_ptype_full.grates_isoweek <- function(x, ...) {"isoweek"}



# -------------------------------------------------------------------------
#' @export
`[.grates_isoweek` <- function(x, ..., drop = FALSE) {
    out <- NextMethod()
    class(out) <- class(x)
    out
}

# -------------------------------------------------------------------------
#' @export
`[[.grates_isoweek` <- function(x, ..., drop = TRUE) {
    out <- NextMethod()
    class(out) <- class(x)
    out
}

# -------------------------------------------------------------------------
#' @export
`[<-.grates_isoweek` <- function(x, ..., value) {
    old_class <- class(x)
    if (!inherits(value, "grates_isoweek"))
        stop("Can only assign <grates_isoweek> objects in to an <grates_isoweek> object.")
    out <- NextMethod()
    class(out) <- class(x)
    out
}

# -------------------------------------------------------------------------
#' @export
`[[<-.grates_isoweek` <- `[<-.grates_isoweek`

# -------------------------------------------------------------------------
#' @export
rep.grates_isoweek <- function(x, ...) {
    out <- NextMethod()
    class(out) <- class(x)
    out
}

# -------------------------------------------------------------------------
#' @export
unique.grates_isoweek <- function(x, incomparables = FALSE, ...) {
    out <- NextMethod()
    class(out) <- class(x)
    out
}

# -------------------------------------------------------------------------
#' @export
c.grates_isoweek <- function(..., recursive = FALSE, use.names = TRUE) {
    dots <- list(...)
    if (!all(vapply(dots, inherits, TRUE, what = "grates_isoweek")))
        stop("Unable to combine <grates_isoweek> objects with other classes.")
    res <- NextMethod()
    .new_isoweek(res)
}

# -------------------------------------------------------------------------
#' @export
seq.grates_isoweek <- function(from, to, by = 1L, ...) {

    if (!inherits(to, "grates_isoweek") || length(to) != 1L)
        stop("`to` must be a <grates_isoweek> object of length 1.")

    if (!.is_scalar_whole(by))
        stop("`by` must be an integer of length 1.")

    from <- as.integer(from)
    to <- as.integer(to)
    out <- seq.int(from = from, to = to, by = by)

    # Ensure integer as we cannot rely on seq.int (may return double)
    out <- as.integer(out)
    .new_isoweek(out)
}

# -------------------------------------------------------------------------
#' @export
as.integer.grates_isoweek <- function(x, ...) unclass(x)

# -------------------------------------------------------------------------
#' @export
as.double.grates_isoweek <- function(x, ...) as.double(unclass(x))

# -------------------------------------------------------------------------
#' @export
as.Date.grates_isoweek <- function(x, ...) {
    .Date(as.double(unclass(x)) * 7L - 3L)
}

# -------------------------------------------------------------------------
#' @export
as.POSIXct.grates_isoweek <- function(x, tz = "UTC", ...) {
    if (tz != "UTC") {
        stop(
            "<grates_isoweek> objects can only be converted to UTC. ",
            "If other timezones are required, first convert to <Date> and then proceed as desired."
        )
    }
    x <- as.double(unclass(x)) * 7 - 3
    .POSIXct(x * 86400, tz = "UTC")
}

# -------------------------------------------------------------------------
#' @export
as.POSIXlt.grates_isoweek <- function(x, tz = "UTC", ...) {
    if (tz != "UTC") {
        stop(
            "<grates_isoweek> objects can only be converted to UTC. ",
            "If other timezones are required, first convert to <Date> and then proceed as desired."
        )
    }
    x <- as.double(unclass(x)) * 7 - 3
    as.POSIXlt(x * 86400, tz = "UTC", origin = .POSIXct(0, tz = "UTC"))
}

# -------------------------------------------------------------------------
#' @export
as.character.grates_isoweek <- function(x, ...) {
    format.grates_isoweek(x)
}

# -------------------------------------------------------------------------
#' @export
as.list.grates_isoweek <- function(x, ...) {
    lapply(unclass(x), `class<-`, class(x))
}

# -------------------------------------------------------------------------
#' @export
as.data.frame.grates_isoweek <- as.data.frame.vector

# -------------------------------------------------------------------------
#' @export
min.grates_isoweek <- function(x, ..., na.rm = FALSE) {
    out <- NextMethod()
    class(out) <- class(x)
    out
}

# -------------------------------------------------------------------------
#' @export
max.grates_isoweek <- function(x, ..., na.rm = FALSE) {
    out <- NextMethod()
    class(out) <- class(x)
    out
}

# -------------------------------------------------------------------------
#' @export
range.grates_isoweek <- function(x, ..., na.rm = FALSE) {
    out <- NextMethod()
    class(out) <- class(x)
    out
}

# -------------------------------------------------------------------------
#' @export
Summary.grates_isoweek <- function(..., na.rm = FALSE) {
    stopf("`%s()` is not supported for <grates_isoweek> objects.", .Generic)
}

# -------------------------------------------------------------------------
#' @export
Math.grates_isoweek <- function(x, ...) {
    stopf("`%s()` is not supported for <grates_isoweek> objects.", .Generic)
}

# -------------------------------------------------------------------------
#' @export
quantile.grates_isoweek <- function(x, type = 1, ...) {
    x <- unclass(x)
    x <- as.integer(quantile(x, type = type, ...))
    new_isoweek(x)
}

# -------------------------------------------------------------------------
#' @export
Ops.grates_isoweek <- function(e1, e2) {
    op <- .Generic

    if (op %in% c("==", "!=", "<", ">", "<=", ">=")) {
        if (inherits(e2, "grates_isoweek")) {
            return(NextMethod())
        }
        stop("Can only compare <grates_isoweek> objects with <grates_isoweek> objects.")
    }

    switch(
        op,
        "+" = {
            if (missing(e2)) {
                return(e1)
            } else if (inherits(e1, "grates_isoweek") && inherits(e2, "grates_isoweek")) {
                stop("Cannot add <grates_isoweek> objects to each other.")
            } else if (inherits(e1, "grates_isoweek") && (.is_whole(e2))) {
                return(.new_isoweek(unclass(e1) + as.integer(e2)))
            } else if (inherits(e2, "grates_isoweek") && (.is_whole(e1))) {
                return(.new_isoweek(unclass(e2) + as.integer(e1)))
            }
            stop("Can only add integers to <grates_isoweek> objects.")
        },
        "-" = {
            if (missing(e2)) {
                stop("Cannot negate a <grates_isoweek> object.")
            } else if (inherits(e2, "grates_isoweek")) {
                if (inherits(e1, "grates_isoweek")) {
                    weekdiff <- (unclass(e1) - unclass(e2))
                    return(as.difftime(weekdiff, units = "weeks"))
                } else {
                    stop("Can only subtract from a <grates_isoweek> object, not vice-versa.")
                }
            } else if (inherits(e1, "grates_isoweek") && is.integer(e2)) {
                return(.new_isoweek(unclass(e1) - e2))
            } else if (inherits(e1, "grates_isoweek") && .is_whole(e2)) {
                return(.new_isoweek(unclass(e1) - as.integer(e2)))
            }
            stop("Can only subtract whole numbers and other <grates_isoweek> objects from <grates_isoweek> objects.")
        },
        stopf("%s is not compatible with <grates_isoweek> objects.", op)
    )
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------------------- INTERNALS ------------------------------ #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
.new_isoweek <- function(x) {
    class(x) <- "grates_isoweek"
    x
}

.isoweek <- function(year, week) {
    out <- .yearweek(year = year, week = week, firstday = 1L)
    class(out) <- "grates_isoweek"
    out
}
