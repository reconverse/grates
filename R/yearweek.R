#' Construct a yearweek object
#'
#' @description
#' `yearweek()` is a constructor for `<grates_yearweek>` objects.
#'
#' @details
#' `<grates_yearweek>` objects are stored as the number of weeks
#' (starting at 0) from the date of the `firstday` nearest the Unix Epoch
#' (1970-01-01). That is, the number of seven day periods from:
#'
#'     - 1969-12-29 for `firstday` equal to 1 (Monday)
#'     - 1969-12-30 for `firstday` equal to 2 (Tuesday)
#'     - 1969-12-31 for `firstday` equal to 3 (Wednesday)
#'     - 1970-01-01 for `firstday` equal to 4 (Thursday)
#'     - 1970-01-02 for `firstday` equal to 5 (Friday)
#'     - 1970-01-03 for `firstday` equal to 6 (Saturday)
#'     - 1970-01-04 for `firstday` equal to 7 (Sunday)
#'
#' @param x `[integer]`
#' Vector representing the number of weeks. `double` vectors will be converted
#' via `as.integer(floor(x))`.
#'
#' @param firstday `[integer]`
#' The day the week starts on from 1 (Monday) to 7 (Sunday).
#'
#' @param object
#' An \R object.
#'
#' @return
#' A `<grates_yearweek>` object with subclass corresponding to the first
#' day of the week they represent (e.g. `<grates_yearweek_monday>`).
#'
#' @examples
#' yearweek(1:10)
#'
#' @seealso
#' `as_yearweek()`, `isoweek()` and `epiweek()`.
#'
#' @export
yearweek <- function(x = integer(), firstday = 1L) {
    if (!is.integer(x)) {
        if (is.double(x) && is.vector(x)) {
            x <- as.integer(floor(x))
        } else {
            stop("`x` must be integer.")
        }
    }

    if (length(firstday) != 1L)
        stop("`firstday` must be an integer of length 1.")

    if (!is.integer(firstday)) {
        if (!.is_whole(firstday))
            stop("`firstday` must be an integer of length 1.")
        firstday <- as.integer(firstday)
    }

    if (firstday < 1L || firstday > 7L || is.na(firstday))
        stop("`firstday` must be an integer between 1 (Monday) and 7 (Sunday).")

    .new_yearweek(x = x, firstday = firstday)
}

# -------------------------------------------------------------------------
#' @rdname yearweek
#' @export
is_yearweek <- function(object) inherits(object, "grates_yearweek")

# -------------------------------------------------------------------------
#' @export
format.grates_yearweek <- function(x, ...) {
    if (length(x) == 0)
        return(character(0))
    week <- get_week.grates_yearweek(x)
    yr <- get_year.grates_yearweek(x)
    out <- sprintf("%04d-W%02d", yr, week)
    out[is.na(x)] <- NA_character_
    setNames(out, names(x))
}

# -------------------------------------------------------------------------
#' @export
print.grates_yearweek <- function(x, ...) {
    # replicate the header similar to vctrs
    n <- length(x)
    cls <- class(x)[[1L]]
    cat("<", cls, "[", n, "]>\n", sep = "")
    if (n)
        print(format.grates_yearweek(x))
    invisible(x)
}

# -------------------------------------------------------------------------
vec_ptype_abbr.grates_yearweek <- function(x, ...) "yrwk"
vec_ptype_full.grates_yearweek <- function(x, ...) "yearweek"
vec_ptype_full.grates_yearweek_monday <- function(x, ...) "yearweek-mon"
vec_ptype_full.grates_yearweek_tuesday <- function(x, ...) "yearweek-tue"
vec_ptype_full.grates_yearweek_wednesday <- function(x, ...) "yearweek-wed"
vec_ptype_full.grates_yearweek_thursday <- function(x, ...) "yearweek-thu"
vec_ptype_full.grates_yearweek_friday <- function(x, ...) "yearweek-fri"
vec_ptype_full.grates_yearweek_saturday <- function(x, ...) "yearweek-sat"
vec_ptype_full.grates_yearweek_sunday <- function(x, ...) "yearweek-sun"

# -------------------------------------------------------------------------
#' Coerce to a yearweek object
#'
#' @description
#' Generic for conversion to <grates_yearweek>
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
#' @inheritParams yearweek
#'
#' @return
#' A `<grates_yearweek>` object.
#'
#' @examples
#' as_yearweek(Sys.Date())
#' as_yearweek(as.POSIXct("2019-03-04 01:01:01", tz = "America/New_York"))
#' as_yearweek("2019-05-03", firstday = 5L)
#' as_yearweek("2021-W03", format = NULL)
#'
#' @seealso
#' `as.Date()` and `yearweek()`.
#'
#' @export
as_yearweek <- function(x, ...) UseMethod("as_yearweek")

# -------------------------------------------------------------------------
#' @rdname as_yearweek
#' @export
as_yearweek.default <- function(x, ...) {
    stop(
        sprintf(
            "Not implemented for class [%s].",
            paste(class(x), collapse = ", ")
        )
    )
}

# -------------------------------------------------------------------------
#' @rdname as_yearweek
#' @export
as_yearweek.Date <- function(x, firstday = 1L, ...) {
    if (length(firstday) != 1L)
        stop("`firstday` must be an integer of length 1.")

    if (!is.integer(firstday)) {
        if (!.is_whole(firstday))
            stop("`firstday` must be an integer of length 1.")
        firstday <- as.integer(firstday)
    }

    if (firstday < 1L || firstday > 7L || is.na(firstday))
        stop("`firstday` must be an integer between 1 (Monday) and 7 (Sunday).")

    x <- as.integer(floor(unclass(x)))
    x <- (x + 4L - firstday) %/% 7L
    .new_yearweek(x = x, firstday = firstday)
}

# -------------------------------------------------------------------------
#' @rdname as_yearweek
#' @export
as_yearweek.POSIXt <- function(x, firstday = 1L, ...) {
    x <- .as_date(x)
    as_yearweek.Date(x, firstday = firstday)
}

# -------------------------------------------------------------------------
#' @rdname as_yearweek
#' @export
as_yearweek.character <- function(
    x,
    firstday = 1L,
    format,
    tryFormats = c("%Y-%m-%d", "%Y/%m/%d"),
    ...
) {
    if (!missing(format) && is.null(format) && ...length() == 0L) {
        yearweek_pattern <- "(^\\d{4}-W([0][1-9]|[1-4][0-9]|5[0-3])$)"
        x <- trimws(x)
        allowed <- grepl(yearweek_pattern, x)
        if (all(allowed)) {
            out <- .parse_yearweek_string(x, firstday)
        } else if (any(allowed)) {
            warning("Unable to parse some entries in yearweek format 'YYYY-Www'. Returning these as NA")
            x[!allowed] <- NA_character_
            out <- double(length(x))
            out[!allowed] <- NA_real_
            out[allowed] <- .parse_yearweek_string(x[allowed], firstday)
        } else {
            out <- as.Date(x)
        }
    } else {
        out <- as.Date(x, format = format, ...)
    }

    as_yearweek.Date(out, firstday = firstday)
}

# -------------------------------------------------------------------------
#' @rdname as_yearweek
#' @export
as_yearweek.factor <- function(x, firstday = 1L, format = NULL, ...) {
    x <- as.character(x)
    as_yearweek.character(x, firstday = firstday, format = format, ...)
}

# -------------------------------------------------------------------------
#' @export
`[.grates_yearweek` <- function(x, ..., drop = FALSE) {
    out <- NextMethod()
    class(out) <- oldClass(x)
    out
}

# -------------------------------------------------------------------------
#' @export
`[[.grates_yearweek` <- function(x, ..., drop = TRUE) {
    out <- NextMethod()
    class(out) <- oldClass(x)
    out
}

# -------------------------------------------------------------------------
#' @export
`[<-.grates_yearweek` <- function(x, ..., value) {
    if (!inherits(value, "grates_yearweek"))
        stop("Can only assign <grates_yearweek> objects in to an <grates_yearweek> object.")
    fdx <- .firstday_from_class(x)
    fdv <- .firstday_from_class(value)
    if (isTRUE(fdx != fdv))
        stop("Incompatible first day of the week.")
    out <- NextMethod()
    class(out) <- oldClass(x)
    out
}

# -------------------------------------------------------------------------
#' @export
`[[<-.grates_yearweek` <- `[<-.grates_yearweek`

# -------------------------------------------------------------------------
#' @export
rep.grates_yearweek <- function(x, ...) {
    out <- NextMethod()
    class(out) <- oldClass(x)
    out
}

# -------------------------------------------------------------------------
#' @export
unique.grates_yearweek <- function(x, incomparables = FALSE, ...) {
    out <- NextMethod()
    class(out) <- oldClass(x)
    out
}

# -------------------------------------------------------------------------
#' @export
c.grates_yearweek <- function(..., recursive = FALSE, use.names = TRUE) {
    dots <- list(...)
    if (!all(vapply(dots, inherits, TRUE, what = "grates_yearweek")))
        stop("Unable to combine <grates_yearweek> objects with other classes.")
    fds <- vapply(dots, .firstday_from_class, 1L)
    if (length(unique(fds)) != 1L)
        stop("Unable to combine <grates_yearweek> objects with different first days of the week.")
    res <- NextMethod()
    .new_yearweek(res, firstday = fds[[1]])
}

# -------------------------------------------------------------------------
#' @export
seq.grates_yearweek <- function(from, to, by = 1L, ...) {

    if (!inherits(to, "grates_yearweek") || length(to) != 1L)
        stop("`to` must be a <grates_yearweek> object of length 1.")

    if (!.is_scalar_whole(by))
        stop("`by` must be an integer of length 1.")

    ffd <- .firstday_from_class(from)
    tfd <- .firstday_from_class(to)
    if (ffd != tfd)
        stop("`to` must have the same first day of the week as `from`")

    from <- as.integer(from)
    to <- as.integer(to)
    out <- seq.int(from = from, to = to, by = by)

    # Ensure integer as we cannot rely on seq.int (may return double)
    out <- as.integer(out)
    .new_yearweek(out, firstday = ffd)
}

# -------------------------------------------------------------------------
#' @export
as.integer.grates_yearweek <- function(x, ...) unclass(x)

# -------------------------------------------------------------------------
#' @export
as.double.grates_yearweek <- function(x, ...) as.double(unclass(x))

# -------------------------------------------------------------------------
#' @export
as.Date.grates_yearweek <- function(x, ...) {
    firstday <- .firstday_from_class(x)
    .Date(as.double(unclass(x)) * 7 + (firstday - 4))
}

# -------------------------------------------------------------------------
#' @export
as.POSIXct.grates_yearweek <- function(x, tz = "UTC", ...) {
    if (tz != "UTC")
        stop("<grates_yearweek> objects can only be converted to UTC. If other timezones are required, first convert to <Date> and then proceed as desired.")
    firstday <- .firstday_from_class(x)
    x <- as.double(unclass(x)) * 7 + (firstday - 4)
    .POSIXct(x * 86400, tz = "UTC")
}

# -------------------------------------------------------------------------
#' @export
as.POSIXlt.grates_yearweek <- function(x, tz = "UTC", ...) {
    if (tz != "UTC")
        stop("<grates_yearweek> objects can only be converted to UTC. If other timezones are required, first convert to <Date> and then proceed as desired.")
    firstday <- .firstday_from_class(x)
    x <- as.double(unclass(x)) * 7 + (firstday - 4)
    as.POSIXlt(x * 86400, tz = "UTC", origin = .POSIXct(0, tz = "UTC"))
}

# -------------------------------------------------------------------------
#' @export
as.character.grates_yearweek <- function(x, ...) format.grates_yearweek(x)

# -------------------------------------------------------------------------
#' @export
as.list.grates_yearweek <- function(x, ...) lapply(unclass(x), `class<-`, class(x))

# -------------------------------------------------------------------------
#' @export
as.data.frame.grates_yearweek <- as.data.frame.vector

# -------------------------------------------------------------------------
#' @export
min.grates_yearweek <- function(x, ..., na.rm = FALSE) {
    out <- NextMethod()
    class(out) <- oldClass(x)
    out
}

# -------------------------------------------------------------------------
#' @export
max.grates_yearweek <- function(x, ..., na.rm = FALSE) {
    out <- NextMethod()
    class(out) <- oldClass(x)
    out
}

# -------------------------------------------------------------------------
#' @export
range.grates_yearweek <- function(x, ..., na.rm = FALSE) {
    out <- NextMethod()
    class(out) <- oldClass(x)
    out
}

# -------------------------------------------------------------------------
#' @export
Summary.grates_yearweek <- function(..., na.rm = FALSE) {
    stop(
        sprintf(
            "`%s()` is not supported for <grates_yearweek> objects.",
            .Generic
        )
    )
}

# -------------------------------------------------------------------------
#' @export
Math.grates_yearweek <- function(x, ...) {
    stop(
        sprintf(
            "`%s()` is not supported for <grates_yearweek> objects.",
            .Generic
        )
    )
}

# -------------------------------------------------------------------------
#' @export
quantile.grates_yearweek <- function(x, type = 1, ...) {
    x <- unclass(x)
    firstday <- .firstday_from_class(x)
    x <- as.integer(quantile(x, type = type, ...))
    yearweek(x, firstday = firstday)
}

# -------------------------------------------------------------------------
#' @export
Ops.grates_yearweek <- function(e1, e2) {
    op <- .Generic
    if (op %in% c("==", "!=", "<", ">", "<=", ">=")) {
        if (inherits(e2, "grates_yearweek")) {
            fd1 <- .firstday_from_class(e1)
            fd2 <- .firstday_from_class(e2)
            if (isTRUE(all.equal(fd1, fd2))) {
                return(NextMethod())
            } else if (op == "==") {
                return(FALSE)
            } else if (op == "!=") {
                return(TRUE)
            } else {
                stop("Can only compare <grates_yearweek> objects with the same first day of the week.")
            }
        } else {
            stop("Can only compare <grates_yearweek> objects with <grates_yearweek> objects.")
        }
    }

    switch(
        op,
        "+" = {
            if (missing(e2)) {
                return(e1)
            } else if (inherits(e1, "grates_yearweek") && inherits(e2, "grates_yearweek")) {
                stop("Cannot add <grates_yearweek> objects to each other.")
            } else if (inherits(e1, "grates_yearweek") && (.is_whole(e2))) {
                fd <- .firstday_from_class(e1)
                .new_yearweek(unclass(e1) + as.integer(e2), firstday = fd)
            } else if (inherits(e2, "grates_yearweek") && (.is_whole(e1))) {
                fd <- .firstday_from_class(e2)
                .new_yearweek(unclass(e2) + as.integer(e1), firstday = fd)
            } else {
                stop("Can only add integers to <grates_yearweek> objects.")
            }
        },
        "-" = {
            if (missing(e2)) {
                stop("Cannot negate a <grates_yearweek> object.")
            } else if (inherits(e2, "grates_yearweek")) {
                if (inherits(e1, "grates_yearweek")) {
                    fd1 <- .firstday_from_class(e1)
                    fd2 <- .firstday_from_class(e2)
                    if (isTRUE(all.equal(fd1, fd2))) {
                        weekdiff <- (unclass(e1) - unclass(e2))
                        as.difftime(weekdiff, units = "weeks")
                    } else {
                        stop("<grates_yearweek> objects must have the same first day of the week to perform subtraction.")
                    }
                } else {
                    stop("Can only subtract from a <grates_yearweek> object, not vice-versa.")
                }
            } else if (inherits(e1, "grates_yearweek") && is.integer(e2)) {
                fd <- .firstday_from_class(e1)
                .new_yearweek(unclass(e1) - e2, firstday = fd)
            } else if (inherits(e1, "grates_yearweek") && .is_whole(e2)) {
                fd <- .firstday_from_class(e1)
                .new_yearweek(unclass(e1) - as.integer(e2), firstday = fd)
            } else {
                stop("Can only subtract whole numbers and other <grates_yearweek> objects from <grates_yearweek> objects.")
            }


        },
        stop(
            sprintf("%s is not compatible with <grates_yearweek> objects.", op)
        )
    )
}

# -------------------------------------------------------------------------
# TODO export this and add checks
yearweek_string_to_date <- function(x, firstday) {

    if (!is.character(x))
        stop("`x` must be a character vector.")

    if (length(firstday) != 1L)
        stop("`firstday` must be an integer of length 1.")

    if (!is.integer(firstday)) {
        if (!.is_whole(firstday))
            stop("`firstday` must be an integer of length 1.")
        firstday <- as.integer(firstday)
    }

    if (firstday < 1L || firstday > 7L || is.na(firstday))
        stop("`firstday` must be an integer between 1 (Monday) and 7 (Sunday).")

    yearweek_pattern <- "(^\\d{4}-W([0][1-9]|[1-4][0-9]|5[0-3])$)"
    allowed <- grepl(yearweek_pattern, trimws(x))
    allowed[is.na(x)] <- TRUE
    dat <- trimws(x)
    out <- rep.int(NA_integer_, length(x))
    out[allowed] <- .parse_yearweek_string(dat[allowed], firstday)
    out <- .Date(out)
    out
}

# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------------------- INTERNALS ------------------------------ #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

.new_yearweek <- function(x = integer(), firstday = 1L) {
    switch(
        firstday,
        .new_yearweek_monday(x),
        .new_yearweek_tuesday(x),
        .new_yearweek_wednesday(x),
        .new_yearweek_thursday(x),
        .new_yearweek_friday(x),
        .new_yearweek_saturday(x),
        .new_yearweek_sunday(x)
    )
}

# -------------------------------------------------------------------------
.new_yearweek_monday <- function(x) {
    structure(x, class = c("grates_yearweek_monday", "grates_yearweek"))
}

# -------------------------------------------------------------------------
.new_yearweek_tuesday <- function(x) {
    structure(x, class = c("grates_yearweek_tuesday", "grates_yearweek"))
}

# -------------------------------------------------------------------------
.new_yearweek_wednesday <- function(x) {
    structure(x, class = c("grates_yearweek_wednesday", "grates_yearweek"))
}

# -------------------------------------------------------------------------
.new_yearweek_thursday <- function(x) {
    structure(x, class = c("grates_yearweek_thursday", "grates_yearweek"))
}

# -------------------------------------------------------------------------
.new_yearweek_friday <- function(x) {
    structure(x, class = c("grates_yearweek_friday", "grates_yearweek"))
}

# -------------------------------------------------------------------------
.new_yearweek_saturday <- function(x) {
    structure(x, class = c("grates_yearweek_saturday", "grates_yearweek"))
}

# -------------------------------------------------------------------------
.new_yearweek_sunday <- function(x) {
    structure(x, class = c("grates_yearweek_sunday", "grates_yearweek"))
}

# -------------------------------------------------------------------------
.firstday_from_class <- function(x) {

    if (inherits(x, "grates_yearweek_monday") || inherits(x, "grates_isoweek"))
        return(1L)
    if (inherits(x, "grates_yearweek_tuesday"))
        return(2L)
    if (inherits(x, "grates_yearweek_wednesday"))
        return(3L)
    if (inherits(x, "grates_yearweek_thursday"))
        return(4L)
    if (inherits(x, "grates_yearweek_friday"))
        return(5L)
    if (inherits(x, "grates_yearweek_saturday"))
        return(6L)
    if (inherits(x, "grates_yearweek_sunday"))
        return(7L)
    stop("Invalid <grates_yearweek> object - class corrupted.")
}

# -------------------------------------------------------------------------
.parse_yearweek_string <- function(x, firstday) {

    # pull out the year and week from string
    year <- as.integer(substr(x, 1, 4))
    week <- as.integer(substr(x, 7, 8))

    # check weeks are valid relative to years
    cond <- (week > .last_week_in_year(year = year, firstday = firstday))
    if (any(cond, na.rm = TRUE)) {
        idx <- which(cond)
        if (length(cond) > 1) {
            stop("Some weeks in are invalid for the given first day. The first invalid year-week combination is %d-W%d (position %d).",
                  year[idx], week[idx], idx)
        } else {
            stop(
                sprintf(
                    "%s is not a valid week for the given first day (%d).",
                    x,
                    firstday
                )
            )
        }

    }

    # convert numeric values to date
    jan4 <- strptime(sprintf("%d-01-04", year), format = "%Y-%m-%d", tz = "UTC")
    wday <- jan4$wday
    out <- jan4 - ((wday + 7L - firstday) %% 7) * 86400
    out <- out + (week - 1) * 7L * 86400
    as.Date(out)
}

# -------------------------------------------------------------------------
.last_week_in_year <- function(year = integer(), firstday = 1L) {
    x <- as.Date(sprintf("%d-12-28", year))
    wday <- strptime(sprintf("%d-12-28", year), format = "%Y-%m-%d", tz = "UTC")$wday
    wday <- 1L + (wday + (7L - firstday)) %% 7L
    midweek <- x + (4L - wday)
    .seven_day_week_in_year(date = midweek)
}

# -------------------------------------------------------------------------
.seven_day_week_in_year <- function(date) {
    x <- .as_utc_posixlt_from_int(date)
    yr <- x$year + 1900L
    jan1 <- sprintf("%d-01-01", yr)
    jan1 <- as.Date(strptime(jan1, format="%Y-%m-%d", tz = "UTC"))
    res <- 1 + (unclass(date) - unclass(jan1)) %/% 7
    attributes(res) <- NULL
    res
}
