#' Construct a isoweek object
#'
#' @description
#' `isoweek()` is a constructor for `<grates_isoweek>` objects.
#'
#' @details
#' isoweeks are defined to start on a Monday and `<grates_isoweek>` objects are
#' stored as the number of weeks (starting at 0) from the first Monday prior to
#' the Unix Epoch (1970-01-01). That is, the number of seven day periods from
#' 1969-12-29.
#'
#' Internally they have the same representation as a `<grates_yearweek_monday>`
#' object so are akin to an alias but with a marginally more efficient
#' implementation.
#'
#' @param x `[integer]`
#' Vector representing the number of weeks. `double` vectors will be converted
#' via `as.integer(floor(x))`.
#'
#' @param object
#' An \R object.
#'
#' @return
#' A `<grates_isoweek>` object.
#'
#' @seealso
#' `yearweek()` and `epiweek()`.
#'
#' @examples
#' isoweek(1:10)
#'
#' @export
isoweek <- function(x = integer()) {
    if (!is.integer(x)) {
        if (is.double(x) && is.vector(x)) {
            x <- as.integer(floor(x))
        } else {
            stop("`x` must be integer.")
        }
    }
    .new_isoweek(x)
}

# -------------------------------------------------------------------------
#' @rdname isoweek
#' @export
is_isoweek <- function(object) inherits(object, "grates_isoweek")

# -------------------------------------------------------------------------
#' @export
format.grates_isoweek <- function(x, ...) format.grates_yearweek(x, ...)

# -------------------------------------------------------------------------
#' @export
print.grates_isoweek <- function(x, ...) print.grates_yearweek(x, ...)

# -------------------------------------------------------------------------
vec_ptype_abbr.grates_isoweek <- function(x, ...) "isowk"
vec_ptype_full.grates_isoweek <- function(x, ...) "isoweek"

# -------------------------------------------------------------------------
#' Coerce to a isoweek object
#'
#' @description
#' Generic for conversion to `<grates_isoweek>`
#'
#' @details
#' - Date, POSIXct, and POSIXlt are converted with the timezone respected.
#'
#' @param x
#' An \R object.
#'
#' @param format `[character]`
#' If NULL, then it attempts to pass character values of the form 'YYYY-Www',
#' otherwise passed through to `as.Date()` (default behaviour).
#'
#' @param tryFormats `[character]`
#' Passed through to `as.Date()`
#'
#' @param ...
#' Other values passed to as.Date().
#'
#' @return
#' A `<grates_isoweek>` object.
#'
#' @examples
#' as_isoweek(Sys.Date())
#' as_isoweek(as.POSIXct("2019-03-04 01:01:01", tz = "America/New_York"))
#' as_isoweek("2019-05-03")
#' as_isoweek("2021-W03", format = NULL)
#'
#' @seealso
#' `isoweek()` and `as.Date()`.
#'
#' @export
as_isoweek <- function(x, ...) UseMethod("as_isoweek")

# -------------------------------------------------------------------------
#' @rdname as_isoweek
#' @export
as_isoweek.default <- function(x, ...) {
    stop(
        sprintf(
            "Not implemented for class [%s].",
            paste(class(x), collapse = ", ")
        )
    )
}

# -------------------------------------------------------------------------
#' @rdname as_isoweek
#' @export
as_isoweek.Date <- function(x, ...) {
    firstday <- 1L
    x <- as.integer(floor(unclass(x)))
    x <- (x + 4L - firstday) %/% 7L
    .new_isoweek(x = x)
}

# -------------------------------------------------------------------------
#' @rdname as_isoweek
#' @export
as_isoweek.POSIXt <- function(x, ...) {
    x <- .as_date(x)
    as_isoweek.Date(x)
}

# -------------------------------------------------------------------------
#' @rdname as_isoweek
#' @export
as_isoweek.character <- function(
        x,
        format,
        tryFormats = c("%Y-%m-%d", "%Y/%m/%d"),
        ...
) {
    if (!missing(format) && is.null(format) && ...length() == 0L) {
        yearweek_pattern <- "(^\\d{4}-W([0][1-9]|[1-4][0-9]|5[0-3])$)"
        x <- trimws(x)
        allowed <- grepl(yearweek_pattern, x)
        if (all(allowed)) {
            out <- .parse_isoweek_string(x)
        } else if (any(allowed)) {
            warning("Unable to parse some entries in yearweek format 'YYYY-Www'. Returning these as NA")
            x[!allowed] <- NA_character_
            out <- double(length(x))
            out[!allowed] <- NA_real_
            out[allowed] <- .parse_isoweek_string(x[allowed])
        } else {
            out <- as.Date(x)
        }
    } else {
        out <- as.Date(x, format = format, ...)
    }

    as_isoweek.Date(out)
}

# -------------------------------------------------------------------------
#' @rdname as_isoweek
#' @export
as_isoweek.factor <- function(x, format = NULL, ...) {
    x <- as.character(x)
    as_isoweek.character(x, format = format, ...)
}

# -------------------------------------------------------------------------
#' @rdname as_isoweek
#' @export
as_isoweek.factor <- function(x, format = NULL, ...) {
    x <- as.character(x)
    as_isoweek.character(x, format = format, ...)
}

# -------------------------------------------------------------------------
#' @export
`[.grates_isoweek` <- function(x, ..., drop = FALSE) {
    out <- NextMethod()
    class(out) <- oldClass(x)
    out
}

# -------------------------------------------------------------------------
#' @export
`[[.grates_isoweek` <- function(x, ..., drop = TRUE) {
    out <- NextMethod()
    class(out) <- oldClass(x)
    out
}

# -------------------------------------------------------------------------
#' @export
`[<-.grates_isoweek` <- function(x, ..., value) {
    old_class <- oldClass(x)
    if (!inherits(value, "grates_isoweek"))
        stop("Can only assign <grates_isoweek> objects in to an <grates_isoweek> object.")
    out <- NextMethod()
    class(out) <- oldClass(x)
    out
}

# -------------------------------------------------------------------------
#' @export
`[[<-.grates_isoweek` <- `[<-.grates_isoweek`

# -------------------------------------------------------------------------
#' @export
rep.grates_isoweek <- function(x, ...) {
    out <- NextMethod()
    class(out) <- oldClass(x)
    out
}

# -------------------------------------------------------------------------
#' @export
unique.grates_isoweek <- function(x, incomparables = FALSE, ...) {
    out <- NextMethod()
    class(out) <- oldClass(x)
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
    if (tz != "UTC")
        stop("<grates_isoweek> objects can only be converted to UTC. If other timezones are required, first convert to <Date> and then proceed as desired.")
    x <- as.double(unclass(x)) * 7 - 3L
    .POSIXct(x * 86400, tz = "UTC")
}

# -------------------------------------------------------------------------
#' @export
as.POSIXlt.grates_isoweek <- function(x, tz = "UTC", ...) {
    if (tz != "UTC")
        stop("<grates_isoweek> objects can only be converted to UTC. If other timezones are required, first convert to <Date> and then proceed as desired.")
    x <- as.double(unclass(x)) * 7L -3L
    as.POSIXlt(x * 86400, tz = "UTC", origin = .POSIXct(0, tz = "UTC"))
}

# -------------------------------------------------------------------------
#' @export
as.character.grates_isoweek <- function(x, ...) format.grates_isoweek(x)

# -------------------------------------------------------------------------
#' @export
as.list.grates_isoweek <- function(x, ...) lapply(unclass(x), `class<-`, class(x))

# -------------------------------------------------------------------------
#' @export
as.data.frame.grates_isoweek <- as.data.frame.vector

# -------------------------------------------------------------------------
#' @export
min.grates_isoweek <- function(x, ..., na.rm = FALSE) {
    out <- NextMethod()
    class(out) <- oldClass(x)
    out
}

# -------------------------------------------------------------------------
#' @export
max.grates_isoweek <- function(x, ..., na.rm = FALSE) {
    out <- NextMethod()
    class(out) <- oldClass(x)
    out
}

# -------------------------------------------------------------------------
#' @export
range.grates_isoweek <- function(x, ..., na.rm = FALSE) {
    out <- NextMethod()
    class(out) <- oldClass(x)
    out
}

# -------------------------------------------------------------------------
#' @export
Summary.grates_isoweek <- function(..., na.rm = FALSE) {
    stop(
        sprintf(
            "`%s()` is not supported for <grates_isoweek> objects.",
            .Generic
        )
    )
}

# -------------------------------------------------------------------------
#' @export
Math.grates_isoweek <- function(x, ...) {
    stop(
        sprintf(
            "`%s()` is not supported for <grates_isoweek> objects.",
            .Generic
        )
    )
}

# -------------------------------------------------------------------------
#' @export
quantile.grates_isoweek <- function(x, type = 1, ...) {
    x <- unclass(x)
    x <- as.integer(quantile(x, type = type, ...))
    isoweek(x)
}

# -------------------------------------------------------------------------
#' @export
Ops.grates_isoweek <- function(e1, e2) {
    op <- .Generic

    if (op %in% c("==", "!=", "<", ">", "<=", ">=")) {
        if (inherits(e2, "grates_isoweek")) {
            return(NextMethod())
        } else {
            stop("Can only compare <grates_isoweek> objects with <grates_isoweek> objects.")
        }
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
            } else {
                stop("Can only add integers to <grates_isoweek> objects.")
            }
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
                .new_isoweek(unclass(e1) - e2)
            } else if (inherits(e1, "grates_isoweek") && .is_whole(e2)) {
                .new_isoweek(unclass(e1) - as.integer(e2))
            } else {
                stop("Can only subtract whole numbers and other <grates_isoweek> objects from <grates_isoweek> objects.")
            }
        },
        stop(
            sprintf("%s is not compatible with <grates_isoweek> objects.", op)
        )
    )
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------------------- INTERNALS ------------------------------ #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

.new_isoweek <- function(x) {
    structure(x, class = c("grates_isoweek"))
}

.parse_isoweek_string <- function(x) {

    firstday <- 1L

    # pull out the year and week from string
    year <- as.integer(substr(x, 1, 4))
    week <- as.integer(substr(x, 7, 8))

    # check weeks are valid relative to years
    cond <- (week > .last_week_in_year(year = year, firstday = firstday))
    if (any(cond, na.rm = TRUE)) {
        idx <- which(cond)
        if (length(cond) > 1) {
            stop(
                sprintf(
                    "Some weeks are invalid isoweeks. The first invalid isoweek is %d-W%d (position %d).",
                    year[idx], week[idx], idx
                )
            )
        } else {
            stop(sprintf("%s is not a valid isoweek.", x))
        }

    }

    # convert numeric values to date
    jan4 <- strptime(sprintf("%d-01-04", year), format = "%Y-%m-%d", tz = "UTC")
    wday <- jan4$wday
    out <- jan4 - ((wday + 7L - firstday) %% 7) * 86400
    out <- out + (week - 1) * 7L * 86400
    as.Date(out)
}

