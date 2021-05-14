# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ----------------------------- AS_YEARWEEK ------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' Convert an object to yearweek
#'
#' @description
#' - Date, POSIXct, and POSIXlt are converted, with the timezone respected,
#'   using [clock::as_date()].
#'
#' - Character input is parsed by two methods. Firstly, if `format = NULL` then
#'   character input is first checked to see if it is in the format of "YYYY-Www"
#'   (e.g. "2021-W03") and parsed accordingly. If it is in a different format,
#'   or `format` is non-NULL then it is parsed  using [clock::date_parse()].
#'
#' @param x An object to coerce to yearweekly.
#' @param firstday An integer representing the day the week starts on from 1
#'   (Monday) to 7 (Sunday).
#' @param ... Not currently used.
#'
#' @note Internally `yearweek` objects are stored as the number of weeks
#'   from the date of the `firstday` nearest the Unix Epoch (1970-01-01).  That
#'   is:
#'
#'     - 1969-12-29 for `firstday` as Monday
#'     - 1969-12-30 for `firstday` as Tuesday
#'     - 1969-12-31 for `firstday` as Wednesday
#'     - 1970-01-01 for `firstday` as Thursday
#'     - 1970-01-02 for `firstday` as Friday
#'     - 1970-01-03 for `firstday` as Saturday
#'     - 1970-01-04 for `firstday` as Sunday
#'
#' @return A `grate_yearweek` object.
#'
#' @examples
#' as_yearweek(Sys.Date())
#' as_yearweek(as.POSIXct("2019-03-04 01:01:01", tz = "America/New_York"))
#' as_yearweek("2019-05-03")
#' as_yearweek("2021-W03")
#'
#' @seealso [clock::date_parse()]
#'
#' @export
as_yearweek <- function(x, ...) {
  UseMethod("as_yearweek")
}


#' @rdname as_yearweek
#' @export
as_yearweek.default <- function(x, ...) {
  stop(
    sprintf("Can't convert a <%s> to a <grate_yearweek>" , class(x)[1]),
    call. = FALSE
  )
}

#' @rdname as_yearweek
#' @export
as_yearweek.grate_yearweek <- function(x, ...) {
  x
}


#' @rdname as_yearweek
#' @export
as_yearweek.Date <- function(x, firstday = 1L, ...) {

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
  yearweek <- new_grate_yearweek(weeknumber = weeknumber, firstday = firstday)

  # finishing touches
  yearweek[is.na(x)] <- NA_real_
  names(yearweek) <- names(x)
  yearweek
}


#' @importFrom clock as_date
#' @rdname as_yearweek
#' @export
as_yearweek.POSIXt <- function(x, firstday = 1L, ...) {

  # Ensure first day can be cast to integer and is in valid range
  firstday <- int_cast(firstday)
  if (firstday > 7L | firstday < 1L) {
    stop(
      "`firstday` must be a whole number between 1 and 7 (inclusive)",
      call. = FALSE
    )
  }

  # calculate date value
  out <- as_date(x)

  # calculate the week number (relative to firstday nearest unix epoch)
  weeknumber <- (as.numeric(out) + 4 - firstday) %/% 7
  attributes(weeknumber) <- NULL

  # create class
  yearweek <- new_grate_yearweek(weeknumber = weeknumber, firstday = firstday)

  # finishing touches
  yearweek[is.na(out)] <- NA_integer_
  names(yearweek) <- names(x)
  yearweek
}

#' @inheritParams clock::date_parse
#' @rdname as_yearweek
#' @export
as_yearweek.character <- function(x, firstday = 1L, format = NULL, locale = clock_locale(), ...) {

  # Ensure first day can be cast to integer and is in valid range
  firstday <- int_cast(firstday)
  if (firstday > 7L | firstday < 1L) {
    stop(
      "`firstday` must be a whole number between 1 and 7 (inclusive)",
      call. = FALSE
    )
  }

  # We add an additional parsing method for a custom YYYY-Www format before
  # using clock to parse if this fails
  if (is.null(format)) {
    yrwk_pattern <- "(^\\d{4}-W([0][1-9]|[1-4][0-9]|5[0-3])$)"
    allowed <- grepl(yrwk_pattern, trimws(x))
    allowed[is.na(x)] <- TRUE
    if (all(allowed)) {
      # remove additional whitespace and parse
      dat <- trimws(x)
      out <- parse_yrwk_string(dat, firstday)
    } else {
      out <- date_parse(x, locale = locale)
    }
  }
   else {
     out <- date_parse(x, format = format, locale = locale)
   }

  if (all(is.na(out))) stop("Unable to parse any entries of x as Dates")

  # convert to yearweek
  out <- as_yearweek.Date(out, firstday = firstday)
  names(out) <- names(x)
  out
}


#' @rdname as_yearweek
#' @export
as_yearweek.factor <- function(x, firstday = 1L, format = NULL, locale = clock_locale(), ...) {
  as_yearweek.character(
    as.character(x),
    firstday = firstday,
    format = format,
    locale = locale
  )
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------- FORMATING / PRINTING -------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
format.grate_yearweek <- function(x, ...) {
  if (length(x) == 0) return(character(0))
  wk <- yearweek_to_week(x)
  yr <- yearweek_to_year(x)
  out <- sprintf("%04d-W%02d", yr, wk)
  out[is.na(x)] <- NA_character_
  names(out) <- names(x)
  out
}


#' @export
print.grate_yearweek <- function(x, ...) {
  print(format.grate_yearweek(x, ...))
  invisible(x)
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------ METHODS: CONVERSIONS FROM yearweek ------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
as.Date.grate_yearweek <- function(x, ...) {
  firstday <- attr(x, "firstday")
  weeknumber <- as.numeric(x)
  x <- new_date((7 * weeknumber) + (firstday - 4))
  attributes(x) <- NULL
  new_date(x)
}

#' @importFrom clock as_zoned_time as_date_time
#' @export
as.POSIXct.grate_yearweek <- function(x, tz = "UTC", ...) {
  x <- as.Date.grate_yearweek(x)
  x <- as_zoned_time(x, zone = tz, nonexistent = "roll-forward", ambiguous = "latest")
  as_date_time(x)
}

#' @importFrom clock as_zoned_time
#' @export
as.POSIXlt.grate_yearweek <- function(x, tz = "UTC", ...) {
  x <- as.Date.grate_yearweek(x)
  x <- as_zoned_time(x, zone = tz, nonexistent = "roll-forward", ambiguous = "latest")
  as.POSIXlt(x)
}

#' @export
as.character.grate_yearweek <- function(x, ...) format(x, ...)


#' @export
as.list.grate_yearweek <- function(x, ...) {
  fd <- attr(x, "firstday")
  lapply(unclass(x), new_grate_yearweek, firstday = fd)
}


#' @export
as.numeric.grate_yearweek <- function(x, ...) {
  attributes(x) <- NULL
  x
}

#' @export
as.data.frame.grate_yearweek <- as.data.frame.vector

# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------ METHODS: MISCELLANEOUS ------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
is.numeric.grate_yearweek <- function(x) FALSE


#' @export
`[.grate_yearweek` <- function (x, ..., drop = TRUE) {
  cl <- oldClass(x)
  fd <- attr(x, "firstday")
  class(x) <- NULL
  val <- NextMethod()
  class(val) <- cl
  attr(val, "firstday") <- fd
  val
}


#' @export
`[[.grate_yearweek` <- function (x, ..., drop = TRUE) {
  cl <- oldClass(x)
  fd <- attr(x, "firstday")
  class(x) <- NULL
  val <- NextMethod()
  class(val) <- cl
  attr(val, "firstday") <- fd
  val
}


#' @export
`[<-.grate_yearweek` <- function(x, i, value) {

  fd <- attr(x, "firstday")
  cl <- oldClass(x)

  if (!all(inherits(value, "grate_yearweek") | is.na(value))) {
    stop(
      "Can only assign <grate_yearweek> objects in to a <grate_yearweek> object",
      call. = FALSE
    )
  }

  if (fd != attr(value, "firstday") && !is.na(value)) {
    stop(
      "<grate_yearweek> objects must have the same `firstday` attribute",
      call. = FALSE
    )
  }

  val <- NextMethod("[<-")
  class(val) <- cl
  attr(val, "firstday") <- fd
  val
}


#' @export
rep.grate_yearweek <- function (x, ..., drop = TRUE) {
  cl <- oldClass(x)
  fd <- attr(x, "firstday")
  class(x) <- NULL
  val <- NextMethod()
  class(val) <- cl
  attr(val, "firstday") <- fd
  val
}

#' @export
unique.grate_yearweek <- function (x, incomparables = FALSE, ...) {
  cl <- oldClass(x)
  fd <- attr(x, "firstday")
  class(x) <- NULL
  val <- NextMethod()
  class(val) <- cl
  attr(val, "firstday") <- fd
  val
}


#' @export
c.grate_yearweek <- function(..., recursive = FALSE, use.names = TRUE) {
  dots <- list(...)
  if (!all(vapply(dots, inherits, logical(1), what = "grate_yearweek") | is.na(dots))) {
    stop(
      "To combine <grate_yearweek> objects with different objects first convert to a common class",
      call. = FALSE
    )
  }
  fd <- attr(dots[[1]], "firstday")
  fds <- lapply(dots, attr, numeric(1), which = "firstday")

  if (!all(vapply(fds, function(x) {is.null(x) || x == fd}, logical(1)))) {
    stop(
      "Unable to combine <grate_yearweek> objects with different `firstday` attributes",
      call. = FALSE
    )
  }
  res <- NextMethod()
  class(res) <- c("grate_yearweek")
  attr(res, "firstday") <- fd
  res
}

#' @export
seq.grate_yearweek <- function(from, to, by = 1L, ...) {
  by <- int_cast(by)

  if (inherits(to, "grate_yearweek")) {
    if (attr(from, "firstday") != attr(to, "firstday")) {
      stop("`to` must have the same firstday attribute as `from", call. = FALSE)
    }
  } else {
    stop("Can only create a sequence between two `yearweek` objects", call. = FALSE)
  }

  from <- as.numeric(from)
  to = as.numeric(to)
  out <- seq(from = from, to = to, by = by)
  new_grate_yearweek(weeknumber = out, firstday = attr(from, "firstday"))
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# --------------------------------- MATHS --------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

is.na.grate_yearweek <- function(x, ...) is.na(unclass(x))

is.nan.grate_yearweek <- function(x, ...) vector("logical", length(x))

is.finite.grate_yearweek <- function(x, ...) !is.na(unclass(x))

is.infinite.grate_yearweek <- function(x, ...) vector("logical", length(x))

#' @export
Math.grate_yearweek <- function(x, ...) {
  .fn <- .Generic
  fn <- switch(
    .fn,
    is.na = is.na.grate_yearweek(x),
    is.nan = is.nan.grate_yearweek(x),
    is.finite = is.finite.grate_yearweek(x),
    is.infinite = is.infinite.grate_yearweek(x),
    stop(sprintf("`%s()` is not supported for <grate_yearweek>", .fn), call. = FALSE)
  )
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ---------------------------------- OPS ---------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
Ops.grate_yearweek <- function(e1, e2) {
  op <- .Generic
  if (op %in% c("==", "!=", "<", ">", "<=", ">=")) {
    if (inherits(e2, "grate_yearweek")) {
      fd1 <- attr(e1, "firstday")
      fd2 <- attr(e2, "firstday")
      if (isTRUE(all.equal(fd1, fd2))) {
        return(NextMethod())
      } else {
        stop(
          "Can only compare <grate_yearweek> objects with the same `firstday` attribute",
          call. = FALSE
        )
      }
    } else {
      stop("Can only compare <grate_yearweek> objects with <grate_yearweek> objects", call. = FALSE)
    }
  }

  switch(
    op,
    "+" = {
      if (missing(e2)) {
        return(e1)
      } else if (inherits(e1, "grate_yearweek") && inherits(e2, "grate_yearweek")) {
        stop("Cannot add <grate_yearweek> objects to each other", call. = FALSE)
      } else if (inherits(e1, "grate_yearweek") && (all(is.wholenumber(unclass(e2)), na.rm = TRUE))) {
        new_grate_yearweek(unclass(e1) + e2, firstday = attr(e1, "firstday"))
      } else if (inherits(e2, "grate_yearweek") && (all(is.wholenumber(unclass(e1)),  na.rm = TRUE))) {
        new_grate_yearweek(unclass(e2) + e1, firstday = attr(e2, "firstday"))
      } else {
        stop("Can only add whole numbers to <grate_yearweek> objects", call. = FALSE)
      }
    },
    "-" = {
      if (missing(e2)) {
        stop("Cannot negate a <grate_yearweek> object", call. = FALSE)
      } else if (inherits(e2, "grate_yearweek")) {
        if (inherits(e1, "grate_yearweek")) {
          fd1 <- attr(e1, "firstday")
          fd2 <- attr(e2, "firstday")
          if (isTRUE(all.equal(fd1, fd2))) {
            as.integer(e1) - as.integer(e2)
          } else {
            stop("<grate_yearweek> objects must have the same `firstday` attribute to perform subtraction")
          }
        } else if (all(is.wholenumber(unclass(e1)),  na.rm = TRUE)) {
          stop("Can only subtract from a <grate_yearweek> object not vice-versa", call. = FALSE)
        }
      } else if (inherits(e1, "grate_yearweek") && (all(is.wholenumber(unclass(e2)), na.rm = TRUE))) {
        new_grate_yearweek(unclass(e1) - as.numeric(e2), firstday = attr(e1, "firstday"))
      } else {
        stop("Can only subtract whole numbers and other <grate_yearweek> objects from <grate_yearweek> objects", call. = FALSE)
      }
    },
    stop(sprintf("%s is not compatible with <grate_yearweek> objects", op), call. = FALSE)
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
Summary.grate_yearweek <- function (..., na.rm)
{
  ok <- switch(.Generic, max = TRUE, min = TRUE, range = TRUE, FALSE)
  if (!ok) stop(.Generic, " not defined for yearweek objects")
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

new_grate_yearweek <- function(weeknumber = numeric(), firstday = integer()) {
  structure(weeknumber, firstday = firstday, class = "grate_yearweek")
}

yearweek_to_week <- function(x) {
  firstday <- attr(x, "firstday")
  weeknumber <- as.numeric(x)
  x <- new_date((7 * weeknumber) + (firstday - 4))
  midweek <- x + 3
  seven_day_week_in_year(date = midweek)
}

yearweek_to_year <- function(x) {
  wk <- yearweek_to_week(x)
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
