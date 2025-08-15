# -------------------------------------------------------------------------
#' Epiweek class
#'
# -------------------------------------------------------------------------
#' @description
#'
#' Epiweeks are defined to start on a Sunday and span a 7 day period.
#' Where they span calendar years, they are associated to the year which
#' contains the majority of the week's days (i.e. the first epiweek a year
#' is the one with at least four days in said year).
#'
#' Internally, `<grates_epiweek>` objects are stored as the number of weeks
#' (starting at 0) from the first Sunday after the Unix Epoch (1970-01-01).
#' That is, the number of seven day periods from 1970-01-04.
#'
# -------------------------------------------------------------------------
#' @details
#'
#' `epiweek()` is a constructor for `<grates_epiweek>` objects. It takes a
#' vector of year and vector of week values as inputs. Length 1 inputs will be
#' recycled to the length of the other input and `double` vectors will again be
#' converted to integer via `as.integer(floor(x))`.
#'
#' `as_epiweek()` is a generic for conversion to `<grates_epiweek>`.
#' - Date, POSIXct, and POSIXlt are converted with the timezone respected.
#' - Character objects are first coerced to date via `as.Date()` unless
#' `format = "yearweek"` in which case input is assumed to be in the form
#' "YYYY-Wxx" and parsed accordingly.
#'
#' `new_epiweek()` is a minimal constructor for `<grates_epiweek>` objects
#' aimed at developers. It takes, as input, the number of epiweeks since the
#' sunday after the Unix Epoch that you wish to represent. `double` vectors will
#' be converted to integer via `as.integer(floor(x))`.
#'
# -------------------------------------------------------------------------
#' @param x,xx
#'
#' \R objects.
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
# -------------------------------------------------------------------------
#' @return
#' A `<grates_epiweek>` object.
#'
# -------------------------------------------------------------------------
#' @seealso
#' The [yearweek][yearweek_class] and [isoweek][isoweek_class] classes.
#'
# -------------------------------------------------------------------------
#' @examples
#'
#' # date coercion
#' as_epiweek(Sys.Date())
#'
#' # POSIXt coercion
#' as_epiweek(as.POSIXct("2019-03-04 01:01:01", tz = "America/New_York"))
#'
#' # character coercion assumes date input by default
#' as_epiweek("2019-05-03")
#'
#' # character coercion can handle YYYY-Wxx format too
#' as_epiweek("2019-W12", format = "yearweek")
#'
#' # construction
#' epiweek(year = 2000, week = 3)
#'
#' # direct construction
#' stopifnot(
#'     identical(
#'         new_epiweek(0:1),
#'         as_epiweek("1970-01-04") + 0:1
#'     )
#' )
# -------------------------------------------------------------------------
#' @name epiweek_class
NULL

# -------------------------------------------------------------------------
#' @rdname epiweek_class
#' @export
epiweek <- function(year = integer(), week = integer()) {

    year <- .make_floored_integer(year)
    week <- .make_floored_integer(week)

    # check compatible lengths
    tmp <- .recycle(year, week)
    year <- tmp[[1L]]
    week <- tmp[[2L]]

    .epiweek(year = year, week = week)
}


# -------------------------------------------------------------------------
#' @rdname epiweek_class
#' @export
as_epiweek <- function(x, ...) {
    UseMethod("as_epiweek")
}

# -------------------------------------------------------------------------
#' @rdname epiweek_class
#' @export
as_epiweek.default <- function(x, ...) {
    stopf("Not implemented for class [%s].", toString(class(x)))
}

# -------------------------------------------------------------------------
#' @rdname epiweek_class
#' @export
as_epiweek.Date <- function(x, ...) {
    firstday <- 7L
    x <- as.integer(floor(unclass(x)))
    x <- (x + 4L - firstday) %/% 7L
    .new_epiweek(x = x)
}

# -------------------------------------------------------------------------
#' @rdname epiweek_class
#' @export
as_epiweek.POSIXt <- function(x, ...) {
    x <- .as_date(x)
    as_epiweek.Date(x)
}

# -------------------------------------------------------------------------
#' @rdname epiweek_class
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
            years <- sub("^([0-9]{4}).*", "\\1", x, perl = TRUE)
            years <- suppressWarnings(as.integer(years))
            weeks <- sub(".*-[wW]([0-9]{1,2}$)", "\\1", x, perl = TRUE)
            weeks <- suppressWarnings(as.integer(weeks))
            out <- epiweek(year = years, week = weeks)
            return(out)
        }
    }

    out <- as.Date(x, format = format, tryFormats = tryFormats, ...)
    as_epiweek.Date(out)
}

# -------------------------------------------------------------------------
#' @rdname epiweek_class
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
#' @rdname epiweek_class
#' @export
new_epiweek <- function(x = integer()) {
    x <- .make_floored_integer(x)
    .new_epiweek(x)
}

# -------------------------------------------------------------------------
#' @rdname epiweek_class
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
#' @exportS3Method vctrs::vec_ptype_abbr
vec_ptype_abbr.grates_epiweek <- function(x, ...) {"epiwk"}

#' @exportS3Method vctrs::vec_ptype_full
vec_ptype_full.grates_epiweek <- function(x, ...) {"epiweek"}

# -------------------------------------------------------------------------
#' @export
`[.grates_epiweek` <- function(x, ..., drop = FALSE) {
    out <- NextMethod()
    `class<-`(out, class(x))
}

# -------------------------------------------------------------------------
#' @export
`[[.grates_epiweek` <- function(x, ..., drop = TRUE) {
    out <- NextMethod()
    `class<-`(out, class(x))
}

# -------------------------------------------------------------------------
#' @export
`[<-.grates_epiweek` <- function(x, ..., value) {
    if (!inherits(value, "grates_epiweek"))
        stop("Can only assign a <grates_epiweek> object into a <grates_epiweek> object.")
    out <- NextMethod()
    `class<-`(out, class(x))
}

# -------------------------------------------------------------------------
#' @export
`[[<-.grates_epiweek` <- `[<-.grates_epiweek`

# -------------------------------------------------------------------------
#' @export
rep.grates_epiweek <- function(x, ...) {
    out <- NextMethod()
    `class<-`(out, class(x))
}

# -------------------------------------------------------------------------
#' @export
unique.grates_epiweek <- function(x, incomparables = FALSE, ...) {
    out <- NextMethod()
    `class<-`(out, class(x))
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
    .Date(as.double(unclass(x)) * 7 + 3)
}

# -------------------------------------------------------------------------
#' @export
as.POSIXct.grates_epiweek <- function(x, tz = "UTC", ...) {
    if (tz != "UTC")
        stop(
            "<grates_epiweek> objects can only be converted to UTC. ",
            "If other timezones are required, first convert to <Date> and then proceed as desired."
        )
    x <- as.double(unclass(x)) * 7 + 3
    .POSIXct(x * 86400, tz = "UTC")
}

# -------------------------------------------------------------------------
#' @export
as.POSIXlt.grates_epiweek <- function(x, tz = "UTC", ...) {
    if (tz != "UTC")
        stop(
            "<grates_epiweek> objects can only be converted to UTC. ",
            "If other timezones are required, first convert to <Date> and then proceed as desired."
        )
    x <- as.double(unclass(x)) * 7 + 3
    as.POSIXlt(.POSIXct(x * 86400, tz = "UTC"), tz = "UTC")
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
    `class<-`(out, class(x))
}

# -------------------------------------------------------------------------
#' @export
max.grates_epiweek <- function(x, ..., na.rm = FALSE) {
    out <- NextMethod()
    `class<-`(out, class(x))
}

# -------------------------------------------------------------------------
#' @export
range.grates_epiweek <- function(x, ..., na.rm = FALSE) {
    out <- NextMethod()
    `class<-`(out, class(x))
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
        }
        stop("Can only compare <grates_epiweek> objects with <grates_epiweek> objects.")
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
            }
            stop("Can only add integers to <grates_epiweek> objects.")
        },
        "-" = {
            if (missing(e2)) {
                stop("Cannot negate a <grates_epiweek> object.")
            } else if (inherits(e2, "grates_epiweek")) {
                if (inherits(e1, "grates_epiweek")) {
                    weekdiff <- (unclass(e1) - unclass(e2))
                    return(as.difftime(weekdiff, units = "weeks"))
                }
                stop("Can only subtract from a <grates_epiweek> object, not vice-versa.")
            } else if (inherits(e1, "grates_epiweek") && is.integer(e2)) {
                return(.new_epiweek(unclass(e1) - e2))
            } else if (inherits(e1, "grates_epiweek") && .is_whole(e2)) {
                return(.new_epiweek(unclass(e1) - as.integer(e2)))
            }
            stop("Can only subtract whole numbers and other <grates_epiweek> objects from <grates_epiweek> objects.") # nolint: line_length_linter.
        },
        stopf("%s is not compatible with <grates_epiweek> objects.", op)
    )
}

# -------------------------------------------------------------------------
#' @export
is.numeric.grates_epiweek <- function(x) {
    FALSE
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------------------- INTERNALS ------------------------------ #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

.new_epiweek <- function(x) `class<-`(x, "grates_epiweek")

.epiweek <- function(year, week) {
    out <- .yearweek(year = year, week = week, firstday = 7L)
    `class<-`(out, "grates_epiweek")
}
