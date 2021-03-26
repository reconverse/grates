# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------------------- AS_YRWK -------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' Convert an object to a yrwk
#'
#' @description
#' - Date, POSIXct, and POSIXlt are converted directly.  Any day, hour, minute,
#'   or second components are dropped. POSIXct and POSIXlt are converted to
#'   dates via `as.date()` with the timezone respected.
#'
#' - Character input is assumed to be provided in either ISO 8601 standard
#'   format, i.e. "yyyy-mm-dd", or yrwk format, i.e. "yyyy-Www". On conversion
#'   the day component will be dropped.
#'
#' @param x `An object to coerce to yrwk.
#' @param firstday An integer representing the day the week starts on from 1
#'   (Monday) to 7 (Sunday).
#' @param ... Not used.
#'
#' @note Internally `yrwk` objects are stored as the number of weeks from the
#'   date of the `firstday` nearest the Unix Epoch (1970-01-01).  That is:
#'
#'     - 1969-12-29 for `firstday` as Monday
#'     - 1969-12-30 for `firstday` as Tuesday
#'     - 1969-12-31 for `firstday` as Wednesday
#'     - 1970-01-01 for `firstday` as Thursday
#'     - 1970-01-02 for `firstday` as Friday
#'     - 1970-01-03 for `firstday` as Saturday
#'     - 1970-01-04 for `firstday` as Sunday
#'
#' @return A `yrwk` object.
#'
#' @examples
#' as_yrwk(Sys.Date())
#' as_yrwk(as.POSIXct("2019-03-04 01:01:01", tz = "America/New_York"))
#' as_yrwk("2019-05-03")
#' as_yrwk("2021-W03")
#'
#' @export
as_yrwk <- function(x, firstday = 1L, ...) {
  UseMethod("as_yrwk")
}


#' @rdname as_yrwk
#' @export
as_yrwk.default <- function(x, firstday = 1L, ...) {
  stop(sprintf("Can't convert a <%s> to a <yrwk>" , class(x)[1]), call. = FALSE)
}


#' @rdname as_yrwk
#' @export
as_yrwk.yrwk <- function(x, ...) {
  x
}


#' @rdname as_yrwk
#' @export
as_yrwk.Date <- function(x, firstday = 1L, ...) {

  # Ensure first day can be cast to integer and is in valid range
  firstday <- int_cast(firstday)
  if (firstday > 7L | firstday < 1L) {
    stop(
      "`firstday` must be a whole number between 1 and 7 (inclusive)",
      call. = FALSE
    )
  }

  # Ensure no fractional days
  x <- as.numeric(trunc(x))

  # calculate the week number (relative to firstday nearest unix epoch)
  weeknumber <- (x + 4 - firstday) %/% 7
  attributes(weeknumber) <- NULL

  # create class
  yrwk <- new_yrwk(weeknumber = weeknumber, firstday = firstday)

  # finishing touches
  yrwk[is.na(x)] <- NA_real_
  names(yrwk) <- names(x)
  yrwk
}


#' @rdname as_yrwk
#' @export
as_yrwk.POSIXt <- function(x, firstday = 1L, ...) {

  # Ensure first day can be cast to integer and is in valid range
  firstday <- int_cast(firstday)
  if (firstday > 7L | firstday < 1L) {
    stop(
      "`firstday` must be a whole number between 1 and 7 (inclusive)",
      call. = FALSE
    )
  }

  # Ensure no fractional days
  x <- trunc(x)

  # calculate date value
  out <- as.Date(x, tz = tzone(x))

  # calculate the week number (relative to firstday nearest unix epoch)
  weeknumber <- (as.numeric(out) + 4 - firstday) %/% 7
  attributes(weeknumber) <- NULL

  # create class
  yrwk <- new_yrwk(weeknumber = weeknumber, firstday = firstday)

  # finishing touches
  yrwk[is.na(out)] <- NA_integer_
  names(yrwk) <- names(x)
  yrwk
}


#' @rdname as_yrwk
#' @export
as_yrwk.character <- function(x, firstday = 1L, ...) {

  # Ensure first day can be cast to integer and is in valid range
  firstday <- int_cast(firstday)
  if (firstday > 7L | firstday < 1L) {
    stop(
      "`firstday` must be a whole number between 1 and 7 (inclusive)",
      call. = FALSE
    )
  }

  # ISO 8601 standard (YYYY-MM-DD)
  iso_pattern <- "(^\\d{4}-(0[1-9]|1[012])-(0[1-9]|[12][0-9]|3[0-1])$)"

  # custom format YYYY-Www
  yrwk_pattern <- "(^\\d{4}-W([0][1-9]|[1-4][0-9]|5[0-3])$)"

  # either pattern is allowed, as are NA's
  pattern <- paste(iso_pattern, yrwk_pattern, sep = "|")
  allowed <- grepl(pattern, trimws(x))
  allowed[is.na(x)] <- TRUE
  if (!all(allowed)) {
    stop(
      "Not all dates are in a valid formate:",
      sprintf("The first incorrect date is: %s", x[!allowed][1]),
      call. = FALSE
    )
  }

  # Ensure first day can be cast to integer and is in valid range
  firstday <- int_cast(firstday)
  if (firstday > 7L | firstday < 1L) {
    stop(
      "`firstday` must be a whole number between 1 and 7 (inclusive)",
      call. = FALSE
    )
  }

  # remove extraneous whitespace
  dat <- trimws(x)

  # Note - The following is a little inefficient if all dates are in YYYY-Www
  # format as the conversion takes place twice.  However it allows for the
  # mixing of character strings which may, or may not, be useful!

  # convert to dates
  idx <- grepl(iso_pattern, dat)
  out <- rep(new_date(), length(idx))
  cond <- grepl(iso_pattern, dat)
  out[cond] <- as.Date(dat[cond])
  out[!cond] <- parse_yrwk_string(dat[!cond], firstday)

  # convert to yrwk
  out <- as_yrwk.Date(out, firstday = firstday)
  names(out) <- names(x)
  out
}


#' @rdname as_yrwk
#' @export
as_yrwk.factor <- function(x, firstday = 1L, ...) {
  as_yrwk.character(as.character(x), firstday = firstday)
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------- FORMATING / PRINTING -------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
format.yrwk <- function(x, ...) {
  if (length(x) == 0) return(character(0))
  wk <- yrwk_to_week(x)
  yr <- yrwk_to_year(x)
  out <- sprintf("%04d-W%02d", yr, wk)
  out[is.na(x)] <- NA_character_
  names(out) <- names(x)
  out
}


#' @export
print.yrwk <- function(x, ...) {
  print(format.yrwk(x, ...))
  invisible(x)
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------- METHODS: CONVERSIONS FROM YRWK --------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
as.POSIXct.yrwk <- function(x, tz = "UTC", ...) {
  x <- as.Date.yrwk(x)
  if (tz == "UTC") {
    as_utc_posixct_from_int(x)
  } else {
    as_zoned_posixct_from_int(x, tz = tz)
  }
}


#' @export
as.POSIXlt.yrwk <- function(x, tz = "UTC", ...) {
  x <- as.Date.yrwk(x)
  if (tz == "UTC") {
    as_utc_posixlt_from_int(x)
  } else {
    as_zoned_posixlt_from_int(x, tz = tz)
  }

}


#' @export
as.Date.yrwk <- function(x, ...) {
  firstday <- attr(x, "firstday")
  weeknumber <- as.numeric(x)
  x <- new_date((7 * weeknumber) + (firstday - 4))
  attributes(x) <- NULL
  new_date(x)
}


#' @export
as.character.yrwk <- function(x, ...) format(x, ...)


#' @export
as.list.yrwk <- function(x, ...) {
  fd <- attr(x, "firstday")
  dat <- unclass(x)
  lapply(dat, new_yrwk, firstday = fd)
}


#' @export
as.numeric.yrwk <- function(x, ...) {
  attributes(x) <- NULL
  x
}


# This code is the same as that of the as.data.frame.yearmon code in Zoo by
# Achim Zeileis et al.
#' @export
as.data.frame.yrwk <- function(x, row.names = NULL, optional = FALSE, ...) {
  nrows <- length(x)
  nm <- paste(deparse(substitute(x), width.cutoff = 500), collapse = " ")
  if (is.null(row.names)) {
    if (nrows == 0)
      row.names <- character(0)
    else if(length(row.names <- names(x)) == nrows && !any(duplicated(row.names))) {
    }
    else if(optional) row.names <- character(nrows)
    else row.names <- seq_len(nrows)
  }
  names(x) <- NULL
  value <- list(x)
  if(!optional) names(value) <- nm
  attr(value, "row.names") <- row.names
  class(value) <- "data.frame"
  value
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------ METHODS: MISCELLANEOUS ------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
is.numeric.yrwk <- function(x) FALSE


#' @export
`[.yrwk` <- function (x, ..., drop = TRUE) {
  cl <- oldClass(x)
  fd <- attr(x, "firstday")
  class(x) <- NULL
  val <- NextMethod()
  class(val) <- cl
  attr(val, "firstday") <- fd
  val
}


#' @export
`[[.yrwk` <- function (x, ..., drop = TRUE) {
  cl <- oldClass(x)
  fd <- attr(x, "firstday")
  class(x) <- NULL
  val <- NextMethod()
  class(val) <- cl
  attr(val, "firstday") <- fd
  val
}


#' @export
`[<-.yrwk` <- function(x, i, value) {

  fd <- attr(x, "firstday")
  cl <- oldClass(x)

  if (inherits(value, "yrwk")) {
    if (fd != attr(value, "firstday")) {
      stop("yrwk objects must have the same firstday attribute", call. = FALSE)
    }
  }

  if (!all(inherits(value, "yrwk") | is.na(value))) {
    stop("Can only assign yrwk objects in to a yrwk object", call. = FALSE)
  }

  val <- NextMethod("[<-")
  attr(val, "firstday") <- fd
  class(val) <- cl
  val
}


#' @export
rep.yrwk <- function (x, ..., drop = TRUE) {
  cl <- oldClass(x)
  fd <- attr(x, "firstday")
  class(x) <- NULL
  val <- NextMethod()
  class(val) <- cl
  attr(val, "firstday") <- fd
  val
}

#' @export
unique.yrwk <- function (x, incomparables = FALSE, ...) {
  cl <- oldClass(x)
  fd <- attr(x, "firstday")
  class(x) <- NULL
  val <- NextMethod()
  class(val) <- cl
  attr(val, "firstday") <- fd
  val
}


#' @export
c.yrwk <- function(..., recursive = FALSE, use.names = TRUE) {
  dots <- list(...)
  if (!all(vapply(dots, inherits, logical(1), what = "yrwk") | is.na(dots))) {
    stop(
      "To combine <yrwk> objects with different objects first convert to a common class",
      call. = FALSE
    )
  }
  fd <- attr(dots[[1]], "firstday")
  fds <- lapply(dots, attr, numeric(1), which = "firstday")

  if (!all(vapply(fds, function(x) {is.null(x) || x == fd}, logical(1)))) {
    stop(
      "Unable to combine <yrwk> objects with different `firstday` attributes",
      call. = FALSE
    )
  }
  res <- NextMethod()
  class(res) <- c("yrwk", "grate")
  attr(res, "firstday") <- fd
  res
}

#' @export
seq.yrwk <- function(from, to, by = 1L, ...) {
  by <- int_cast(by)

  if (inherits(to, "yrwk")) {
    if (attr(from, "firstday") != attr(to, "firstday")) {
      stop("`to` must have the same firstday attribute as `from", call. = FALSE)
    }
  } else {
    stop("Can only create a sequence between two `yrwk` objects", call. = FALSE)
  }

  from <- as.numeric(from)
  to = as.numeric(to)
  out <- seq(from = from, to = to, by = by)
  new_yrwk(weeknumber = out, firstday = attr(from, "firstday"))
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# --------------------------------- MATHS --------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
Math.yrwk <- function(x, ...) {
  .fn <- .Generic
  fn <- switch(
    .fn,
    is.nan = is.nan.yrwk(x),
    is.finite = is.finite.yrwk(x),
    is.infinite = is.infinite.yrwk(x),
    stop(sprintf("`%s()` is not supported for <yrwk>", .fn), call. = FALSE)
  )
}

is.nan.yrwk <- function(x, ...) vector("logical", length(x))

is.finite.yrwk <- function(x, ...) !is.na(unclass(x))

is.infinite.yrwk <- function(x, ...) vector("logical", length(x))


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ---------------------------------- OPS ---------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
Ops.yrwk <- function(e1, e2) {
  op <- .Generic
  if (op %in% c("==", "!=", "<", ">", "<=", ">=")) {
    if (inherits(e2, "yrwk")) {
      fd1 <- attr(e1, "firstday")
      fd2 <- attr(e2, "firstday")
      if (isTRUE(all.equal(fd1, fd2))) {
        return(NextMethod())
      } else {
        stop(
          "Can only compare <yrwk> objects with the same `firstday` attribute",
          call. = FALSE
        )
      }
    } else {
      stop("Can only compare <yrwk> objects with <yrwk> objects", call. = FALSE)
    }
  }

  switch(
    op,
    "+" = {
      if (missing(e2)) {
        return(e1)
      } else if (inherits(e1, "yrwk") && inherits(e2, "yrwk")) {
        stop("Cannot add <yrwk> objects to each other", call. = FALSE)
      } else if (inherits(e1, "yrwk") && (all(is.wholenumber(unclass(e2)), na.rm = TRUE))) {
        new_yrwk(unclass(e1) + e2, firstday = attr(e1, "firstday"))
      } else if (inherits(e2, "yrwk") && (all(is.wholenumber(unclass(e1)),  na.rm = TRUE))) {
        new_yrwk(unclass(e2) + e1, firstday = attr(e2, "firstday"))
      } else {
        stop("Can only add whole numbers to <yrwk> objects", call. = FALSE)
      }
    },
    "-" = {
      if (missing(e2)) {
        stop("Cannot negate a <yrwk> object", call. = FALSE)
      } else if (inherits(e2, "yrwk")) {
        if (inherits(e1, "yrwk")) {
          fd1 <- attr(e1, "firstday")
          fd2 <- attr(e2, "firstday")
          if (isTRUE(all.equal(fd1, fd2))) {
            as.integer(e1) - as.integer(e2)
          } else {
            stop("<yrwk> objects must have the same `firstday` attribute to perform subtraction")
          }
        } else if (all(is.wholenumber(unclass(e1)),  na.rm = TRUE)) {
          stop("Can only subtract from a <yrwk> object not vice-versa", call. = FALSE)
        }
      } else if (inherits(e1, "yrwk") && (all(is.wholenumber(unclass(e2)), na.rm = TRUE))) {
        new_yrwk(unclass(e1) - as.numeric(e2), firstday = attr(e1, "firstday"))
      } else {
        stop("Can only subtract whole numbers and other <yrwk> objects from <yrwk> objects", call. = FALSE)
      }
    },
    stop(sprintf("%s is not compatible with <yrwk> objects", op), call. = FALSE)
  )
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------------------- SUMMARY -------------------------------- #
# ------------------------------------------------------------------------- #
# ---- THE FOLLOWING IS BASED ON THE FUNCTION IN ZOO BY ACHIM ZEILEIS ----- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
Summary.yrwk <- function (..., na.rm)
{
  ok <- switch(.Generic, max = TRUE, min = TRUE, range = TRUE, FALSE)
  if (!ok) stop(.Generic, " not defined for yrwk objects")
  fd <- attr(list(...)[[1]], "firstday")
  val <- NextMethod(.Generic)
  class(val) <- oldClass(list(...)[[1]])
  attr(val, "firstday") <- fd
  val
}

# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------- INTERNALS ------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

new_yrwk <- function(weeknumber = numeric(), firstday = integer()) {
  structure(weeknumber, firstday = firstday, class = c("yrwk", "grate"))
}

yrwk_to_week <- function(x) {
  firstday <- attr(x, "firstday")
  weeknumber <- as.numeric(x)
  x <- new_date((7 * weeknumber) + (firstday - 4))
  midweek <- x + 3
  seven_day_week_in_year(date = midweek)
}

yrwk_to_year <- function(x) {
  wk <- yrwk_to_week(x)
  firstday <- attr(x, "firstday")
  weeknumber <- as.numeric(x)
  x <- new_date((7 * weeknumber) + (firstday - 4))
  dat <- as_utc_posixlt_from_int(x)
  december <- dat$mon == 11L
  january <- dat$mon == 0L
  boundary_adjustment <- integer(length(x))
  boundary_adjustment[january  & wk >= 52] <- -1L
  boundary_adjustment[december & wk == 1]  <- 1L
  yr = dat$year + 1900L
  yr + boundary_adjustment
}


seven_day_week_in_year <- function(date) {
  xx <- as_utc_posixlt_from_int(date)
  yr <- xx$year + 1900L
  jan1 <- sprintf("%d-01-01", yr)
  jan1 <- as.Date(strptime(jan1, format = "%Y-%m-%d", tz = "UTC"))
  res <- 1 + (unclass(date) - unclass(jan1)) %/% 7
  attributes(res) <- NULL
  res
}


parse_yrwk_string <- function(x, firstday) {

  # pull out the year and week from string
  year <- as.integer(substr(x, 1, 4))
  week <- as.integer(substr(x, 7, 8))

  # check weeks are valid relative to years
  cond <- (week > last_week_in_year(year = year, firstday = firstday))
  if (any(cond, na.rm = TRUE)) {
    idx <- which(cond)
    stop(
      "Some weeks are invalid for the given week_start\n",
      sprintf("The first invalid year-week combination is %d-%d", year[idx], week[idx]),
      call. = FALSE
    )
  }

  numeric_yrwk_to_date(year = year, week = week, firstday = firstday)
}


last_week_in_year <- function(year = integer(), firstday = 1L) {
  x <- as.Date(sprintf("%d-12-28", year))
  wday <- strptime(sprintf("%d-12-28", year), format="%Y-%m-%d", tz = "UTC")$wday
  wday <- 1L + (wday + (7L - firstday)) %% 7L
  midweek <- x + (4L - wday)
  seven_day_week_in_year(date = midweek)
}


numeric_yrwk_to_date <- function(year = integer(), week = integer(), firstday = integer()) {
  jan4 <- strptime(sprintf("%d-01-04", year), format="%Y-%m-%d", tz = "UTC")
  wday <- jan4$wday
  out <- jan4 - ((wday + 7L - firstday) %% 7) * 86400
  out <- out + (week - 1) * 7L * 86400
  as.Date(out)
}


# week helpers ------------------------------------------------------------

#' Translate user input to the start date of the week
#'
#' @param a Weekday specification: E.g. "ISOweek", "MMWRweek", "EPIweek",
#'   "Mon-week", "Tue-week", etc.
#'
#' @return the corresponding weekday
#'
#' @examples
#' get_week_start("ISOweek")
#' get_week_start("MMWRweek")
#' get_week_start("EPIweek")
#'
#' # weeks that start on saturday
#' get_week_start("Sat-week")
#' get_week_start("week: Saturday")
#' get_week_start("2 weeks: Saturday")
#'
#' @keywords internal
#' @export
get_week_start <- function(weekday, numeric = TRUE) {
  wkdy <- gsub("weeks?", "", tolower(weekday))
  wkdy <- gsub("[[:punct:][:blank:][:digit:]]*", "", wkdy)
  wkdy <- if (wkdy == "") "monday" else wkdy # the input was "weeks"
  res <- switch(
    wkdy,
    "mmwr" = "sunday", # MMWR == CDC epiweek
    "epi"  = "sunday", # CDC epiweek
    "iso"  = "monday", # ISOweek == WHO epiweek
    wkdy # all others
  )
  weekday_from_char(res, numeric)
}

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# -------- The following function is modified from the aweek package --------- #
# -------------------------- copyright Zhian Kamvar -------------------------- #
# ---------------------------------------------------------------------------- #

# Copy of the aweek licence
# MIT License
# Copyright (c) 2019
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
#   The above copyright notice and this permission notice shall be included in
#   all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

#' Helper function to find the weekday from a character string
#'
#' @param x a character string specifying the weekday in the current locale or
#'   English.
#'
#' @return an integer from 1 to 7 indicating the day of the ISO 8601 week.
#' @keywords internal
#' @noRd
#' @examples
#'
#' # Will always work
#' weekday_from_char("Monday")
#' weekday_from_char("Tue")
#' weekday_from_char("W")
#'
#' # Change to a German locale
#' lct <- Sys.getlocale("LC_TIME")
#' Sys.setlocale("LC_TIME", "de_DE.utf8")
#'
#' weekday_from_char("Sonntag")
#'
#' # Reset locale
#' Sys.setlocale("LC_TIME", lct)
weekday_from_char <- function(x, numeric = TRUE) {

  # First try with an English locale
  w <- c("monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday")
  weekday <- grep(x, w, ignore.case = TRUE, value = !numeric)

  if (length(weekday) == 0) {
    # find the definitions of the weekdays in the current locale
    w <- weekdays(as.Date(as_yrwk(as.Date("2020-01-01"), firstday = 1L)) + 0:6)
    weekday <- grep(x, w, ignore.case = TRUE, value = !numeric)
  }

  if (length(weekday) != 1) {
    msg <- paste(
      "The weekday '%s' did not unambiguously match (via grep) any of the",
      "valid weekdays in the current locale ('%s') or an English locale:\n  %s"
    )
    stop(
      sprintf(msg, x, Sys.getlocale('LC_TIME'), paste(w, collapse = ", ")),
      call. = FALSE
    )
  }

  if (numeric) {
    return(as.integer(weekday))
  } else {
    return(weekday)
  }
}



