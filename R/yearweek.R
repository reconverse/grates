# -------------------------------------------------------------------------
#' Minimal constructor for a yearweek object
#'
# -------------------------------------------------------------------------
#' `new_yearweek()` is a constructor for `<grates_yearweek>` objects aimed at
#' developers.
#'
# -------------------------------------------------------------------------
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
#'
#' Vector representing the number of weeks.
#'
#' `double` vectors will be converted via `as.integer(floor(x))`.
#'
#' @param firstday `[integer]`
#'
#' The day the week starts on from 1 (Monday) to 7 (Sunday).
#'
#' @param xx
#'
#' \R object.
#'
# -------------------------------------------------------------------------
#' @return
#' A `<grates_yearweek>` object with subclass corresponding to the first
#' day of the week they represent (e.g. `<grates_yearweek_monday>`).
#'
# -------------------------------------------------------------------------
#' @examples
#' new_yearweek(1:10)
#'
# -------------------------------------------------------------------------
#' @seealso
#' `as_yearweek()`, `new_isoweek()` and `new_epiweek()`.
#'
# -------------------------------------------------------------------------
#' @export
new_yearweek <- function(x = integer(), firstday = 1L) {
    if (is.vector(x, "double")) {
        x <- as.integer(floor(x))
    } else if (!is.integer(x)) {
        stop("`x` must be integer.")
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
#' Constructor for yearweek objects
#'
# -------------------------------------------------------------------------
#' `yearweek()` is a constructor for `<grates_yearweek>` objects. These are
#' weeks whose first day can be specified by the user.
#'
# -------------------------------------------------------------------------
#' For yearweek objects the first week of a "year" is considered to be the first
#' yearweek containing 4 days of the given calendar year. This means that the
#' calendar year will sometimes be different to that of the associated yearweek
#' object.
#'
# -------------------------------------------------------------------------
#' @note
#' Internally `<grates_yearweek>` objects are stored as the number of weeks
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
#' @inheritParams new_yearweek
#'
# -------------------------------------------------------------------------
#' @return
#' A `<grates_yearweek>` object with subclass corresponding to the first day of
#' the week they represent (e.g. `<grates_yearweek_monday>`).
#'
# -------------------------------------------------------------------------
#' @examples
#' yearweek(year = 2000L, week = 3L)
#'
# -------------------------------------------------------------------------
#' @seealso
#' `as_yearweek()` and `new_yearweek()`.
#'
# -------------------------------------------------------------------------
#' @export
yearweek <- function(year = integer(), week = integer(), firstday = 1L) {

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

    # check firstday
    if (length(firstday) != 1L)
        stop("`firstday` must be an integer of length 1.")

    if (!is.integer(firstday)) {
        if (!.is_whole(firstday))
            stop("`firstday` must be an integer of length 1.")
        firstday <- as.integer(firstday)
    }

    if (firstday < 1L || firstday > 7L || is.na(firstday))
        stop("`firstday` must be an integer between 1 (Monday) and 7 (Sunday).")

    # check compatible lengths
    tmp <- .recycle(year, week)
    year <- tmp[[1L]]
    week <- tmp[[2L]]

    .yearweek(year = year, week = week, firstday = firstday)
}

# -------------------------------------------------------------------------
#' @rdname new_yearweek
#' @export
is_yearweek <- function(xx) {
    inherits(xx, "grates_yearweek")
}

# -------------------------------------------------------------------------
#' @export
format.grates_yearweek <- function(x, ...) {
    if (length(x) == 0L)
        return(character(0L))
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
#' @exportS3Method vctrs::vec_ptype_abbr
vec_ptype_abbr.grates_yearweek <- function(x, ...) {"yrwk"}

#' @exportS3Method vctrs::vec_ptype_full
vec_ptype_full.grates_yearweek <- function(x, ...) {"yearweek"}

#' @exportS3Method vctrs::vec_ptype_full
vec_ptype_full.grates_yearweek_monday <- function(x, ...) {"yearweek-mon"}

#' @exportS3Method vctrs::vec_ptype_full
vec_ptype_full.grates_yearweek_tuesday <- function(x, ...) {"yearweek-tue"}

#' @exportS3Method vctrs::vec_ptype_full
vec_ptype_full.grates_yearweek_wednesday <- function(x, ...) {"yearweek-wed"}

#' @exportS3Method vctrs::vec_ptype_full
vec_ptype_full.grates_yearweek_thursday <- function(x, ...) {"yearweek-thu"}

#' @exportS3Method vctrs::vec_ptype_full
vec_ptype_full.grates_yearweek_friday <- function(x, ...) {"yearweek-fri"}

#' @exportS3Method vctrs::vec_ptype_full
vec_ptype_full.grates_yearweek_saturday <- function(x, ...) {"yearweek-sat"}

#' @exportS3Method vctrs::vec_ptype_full
vec_ptype_full.grates_yearweek_sunday <- function(x, ...) {"yearweek-sun"}

# -------------------------------------------------------------------------
#' Coerce to a yearweek object
#'
#' @description
#' Generic for conversion to <grates_yearweek>.
#'
#' @details
#'
#' - Date, POSIXct, and POSIXlt are converted with the timezone respected.
#' - Character objects are first coerced to date via `as.Date()` unless
#'   `format = "yearweek"` in which case input is assumed to be in the form
#'   "YYYY-Wxx" and parsed accordingly.
#'
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
#' @inheritParams new_yearweek
#'
#' @return
#' A `<grates_yearweek>` object.
#'
#' @examples
#' as_yearweek(Sys.Date())
#' as_yearweek(as.POSIXct("2019-03-04 01:01:01", tz = "America/New_York"))
#' as_yearweek("2019-05-03", firstday = 5L)
#' as_yearweek("2019-W12", format = "yearweek")
#'
#' @seealso
#' `as.Date()` and `new_yearweek()`.
#'
#' @export
as_yearweek <- function(x, ...) {
    UseMethod("as_yearweek")
}

# -------------------------------------------------------------------------
#' @rdname as_yearweek
#' @export
as_yearweek.default <- function(x, ...) {
    stopf("Not implemented for class [%s].", paste(class(x), collapse = ", "))
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
    if (!missing(format)) {
        if (length(format) > 1L)
            stop("If specified, `format` must be of length 1.")

        if (format == "yearweek") {
            years <- sub("^([0-9]{4}).*", "\\1", x, perl = TRUE)
            years <- suppressWarnings(as.integer(years))
            weeks <- sub(".*-[wW]([0-9]{1,2}$)", "\\1", x, perl = TRUE)
            weeks <- suppressWarnings(as.integer(weeks))
            out <- yearweek(year = years, week = weeks, firstday = firstday)
            return(out)
        }
    }
    out <- as.Date(x, format = format, tryFormats = tryFormats, ...)
    as_yearweek.Date(out, firstday = firstday)
}

# -------------------------------------------------------------------------
#' @rdname as_yearweek
#' @export
as_yearweek.factor <- function(
    x,
    firstday = 1L,
    format,
    tryFormats = c("%Y-%m-%d", "%Y/%m/%d"),
    ...
) {
    x <- as.character(x)
    as_yearweek.character(x, firstday = firstday, format = format, ...)
}

# -------------------------------------------------------------------------
#' @export
`[.grates_yearweek` <- function(x, ..., drop = FALSE) {
    out <- NextMethod()
    class(out) <- class(x)
    out
}

# -------------------------------------------------------------------------
#' @export
`[[.grates_yearweek` <- function(x, ..., drop = TRUE) {
    out <- NextMethod()
    class(out) <- class(x)
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
    class(out) <- class(x)
    out
}

# -------------------------------------------------------------------------
#' @export
`[[<-.grates_yearweek` <- `[<-.grates_yearweek`

# -------------------------------------------------------------------------
#' @export
rep.grates_yearweek <- function(x, ...) {
    out <- NextMethod()
    class(out) <- class(x)
    out
}

# -------------------------------------------------------------------------
#' @export
unique.grates_yearweek <- function(x, incomparables = FALSE, ...) {
    out <- NextMethod()
    class(out) <- class(x)
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
    .new_yearweek(res, firstday = fds[[1L]])
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
as.Date.grates_yearweek <- function(x, ...) {
    firstday <- .firstday_from_class(x)
    .Date(as.double(unclass(x)) * 7 + (firstday - 4))
}

# -------------------------------------------------------------------------
#' @export
as.POSIXct.grates_yearweek <- function(x, tz = "UTC", ...) {
    if (tz != "UTC") {
        stop(
            "<grates_yearweek> objects can only be converted to UTC. ",
            "If other timezones are required, first convert to <Date> and then proceed as desired."
        )
    }

    firstday <- .firstday_from_class(x)
    x <- as.double(unclass(x)) * 7 + (firstday - 4)
    .POSIXct(x * 86400, tz = "UTC")
}

# -------------------------------------------------------------------------
#' @export
as.POSIXlt.grates_yearweek <- function(x, tz = "UTC", ...) {
    if (tz != "UTC") {
        stop(
            "<grates_yearweek> objects can only be converted to UTC. ",
            "If other timezones are required, first convert to <Date> and then proceed as desired."
        )
    }
    firstday <- .firstday_from_class(x)
    x <- as.double(unclass(x)) * 7 + (firstday - 4)
    as.POSIXlt(x * 86400, tz = "UTC", origin = .POSIXct(0, tz = "UTC"))
}

# -------------------------------------------------------------------------
#' @export
as.character.grates_yearweek <- function(x, ...) {
    format.grates_yearweek(x)
}

# -------------------------------------------------------------------------
#' @export
as.list.grates_yearweek <- function(x, ...) {
    lapply(unclass(x), `class<-`, class(x))
}

# -------------------------------------------------------------------------
#' @export
as.data.frame.grates_yearweek <- as.data.frame.vector

# -------------------------------------------------------------------------
#' @export
min.grates_yearweek <- function(x, ..., na.rm = FALSE) {
    out <- NextMethod()
    class(out) <- class(x)
    out
}

# -------------------------------------------------------------------------
#' @export
max.grates_yearweek <- function(x, ..., na.rm = FALSE) {
    out <- NextMethod()
    class(out) <- class(x)
    out
}

# -------------------------------------------------------------------------
#' @export
range.grates_yearweek <- function(x, ..., na.rm = FALSE) {
    out <- NextMethod()
    class(out) <- class(x)
    out
}

# -------------------------------------------------------------------------
#' @export
Summary.grates_yearweek <- function(..., na.rm = FALSE) {
    stopf("`%s()` is not supported for <grates_yearweek> objects.", .Generic)
}

# -------------------------------------------------------------------------
#' @export
Math.grates_yearweek <- function(x, ...) {
    stopf("`%s()` is not supported for <grates_yearweek> objects.", .Generic)
}

# -------------------------------------------------------------------------
#' @export
quantile.grates_yearweek <- function(x, type = 1, ...) {
    x <- unclass(x)
    firstday <- .firstday_from_class(x)
    x <- as.integer(quantile(x, type = type, ...))
    new_yearweek(x, firstday = firstday)
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
            }
            stop("Can only compare <grates_yearweek> objects with the same first day of the week.")
        }
        stop("Can only compare <grates_yearweek> objects with <grates_yearweek> objects.")
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
                return(.new_yearweek(unclass(e1) + as.integer(e2), firstday = fd))
            } else if (inherits(e2, "grates_yearweek") && (.is_whole(e1))) {
                fd <- .firstday_from_class(e2)
                return(.new_yearweek(unclass(e2) + as.integer(e1), firstday = fd))
            }
            stop("Can only add integers to <grates_yearweek> objects.")
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
                        return(as.difftime(weekdiff, units = "weeks"))
                    }
                    stop("<grates_yearweek> objects must have the same first day of the week to perform subtraction.")
                }
                stop("Can only subtract from a <grates_yearweek> object, not vice-versa.")
            } else if (inherits(e1, "grates_yearweek") && is.integer(e2)) {
                fd <- .firstday_from_class(e1)
                return(.new_yearweek(unclass(e1) - e2, firstday = fd))
            } else if (inherits(e1, "grates_yearweek") && .is_whole(e2)) {
                fd <- .firstday_from_class(e1)
                return(.new_yearweek(unclass(e1) - as.integer(e2), firstday = fd))
            }
            stop("Can only subtract whole numbers and other <grates_yearweek> objects from <grates_yearweek> objects.")
        },
        stopf("%s is not compatible with <grates_yearweek> objects.", op)
    )
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
.yearweek <- function(year, week, firstday) {
    na_values <- is.na(year) | is.na(week)
    invalid <- !logical(length(na_values))
    if(!all(na_values))
        invalid[!na_values] <- week[!na_values] > .last_week_in_year(year = year[!na_values], firstday = firstday)
    if (any(invalid))
        warning("Some entries invalid for given `year` and `week` values. Returning these as NA.", call. = FALSE)

    out <- rep.int(NA_integer_, length(year))

    if (!all(invalid)) {
        year <- year[!invalid]
        week <- week[!invalid]
        # convert numeric values to date
        jan4 <- strptime(sprintf("%d-01-04", year), format = "%Y-%m-%d", tz = "UTC")
        wday <- jan4$wday
        tmp <- jan4 - ((wday + 7L - firstday) %% 7) * 86400
        tmp <- tmp + (week - 1) * 7L * 86400
        res <- as.Date(tmp)
        res <- as_yearweek.Date(res, firstday = firstday)
        out[!invalid] <- res
    }

    .new_yearweek(out, firstday = firstday)
}

# -------------------------------------------------------------------------
.last_week_in_year <- function(year = integer(), firstday = 1L) {
    #x <- as.Date(sprintf("%d-12-28", year))
    x <- .Date(.days_before_year(year) + .days_before_yearmonth(year, 12L) - 719162L + 27L)
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
    jan1 <- as.Date(strptime(jan1, format = "%Y-%m-%d", tz = "UTC"))
    res <- 1 + (unclass(date) - unclass(jan1)) %/% 7
    attributes(res) <- NULL
    res
}
