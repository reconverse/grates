# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------------------- AS_PERIOD ------------------------------ #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' Convert an object to a period
#'
#' @param x An object to coerce to a period.
#' @param firstdate The date to anchor the intervals from.  If NULL (default)
#'   the earliest date in the vector will be used.
#' @param interval An integer or character indicating the (fixed) size of the
#'   time interval used for computing the incidence; defaults to 1 day. This can
#'   also be a text string that corresponds to a valid date interval, e.g.
#'     * (x) day(s)
#'     * (x) weeks(s)
#'     * (x) epiweeks(s)
#'     * (x) isoweeks(s)
#'     * (x) months(s)
#'     * (x) quarter(s)
#'     * (x) years(s)
#'   More details can be found in the "Interval specification" below.
#' @param ... Not currently used.
#'
#' @return A `period` object except when `interval = 1` or `interval = 1` in
#'   which case the a `Date` object is returned.
#'
#' @note Internally `period` objects are represented by the date at the beginning
#'   of the period and then stored as the number of days (starting at 0) since
#'   the Unix Epoch (1970-01-01).
#'
#' @examples
#' as_period(Sys.Date(), interval = "28 days")
#' as_period(as.POSIXct("2019-03-04 01:01:01", tz = "America/New_York"), interval = 10)
#' as_period("2019-05-03", interval = "2 weeks")
#'
#' @export
as_period <- function(x, interval, firstdate, ...) {
  UseMethod("as_period")
}


#' @rdname as_period
#' @export
as_period.default <- function(x, interval, firstdate, ...) {
  stop(sprintf("Can't convert a <%s> to a <period>" , class(x)[1]), call. = FALSE)
}


#' @rdname as_period
#' @export
as_period.period <- function(x, interval = 1L, firstdate = NULL, ...) {
  x
}


#' @rdname as_period
#' @export
as_period.Date <- function(x, interval = 1L, firstdate = NULL, ...) {

  # check interval
  stopifnot("Interval is not valid" = valid_interval(interval))

  # truncate days
  x <- trunc(x)

  # ensure we have a firstdate value
  if (is.null(firstdate)) {
    firstdate <- min(x, na.rm = TRUE)
  } else {
    if (!inherits(firstdate, "Date")) {
      stop("`firstdate` should have the same class (Date) as `x`", call. = FALSE)
    }
    if (firstdate > min(x, na.rm = TRUE)) {
      stop("`firstdate` should be at or before the minimum date in `x`", call. = FALSE)
    }
    if (length(firstdate) != 1L) {
      stop(sprintf(
        "Exactly one value should be provided for `interval` (%d provided)",
        length(interval),
        call. = FALSE
      ))
    }
  }

  # Ensure numeric intervals are whole numbers
  if (is.numeric(interval)) {
    interval <- int_cast(interval)
    if (interval < 1L) stop("interval must be positive (>= 1)", call. = FALSE)
  }

  # No need to change anything if the interval is 1
  if (interval == 1L || (get_interval_type(interval) == "day" && get_interval_number(interval) == 1L)) {
    x <- x[x >= firstdate]
    return(x)
  }

  if (is.numeric(interval)) {
    period <- break_dates(x, interval, firstdate)
  } else if (is.character(interval)){

    # First deal with numeric character intervals
    if (!valid_date_period_character(interval)) {
      suppressWarnings({
        interval <- as.numeric(interval)
      })

      if (is.na(interval)) {
        stop(
          'The interval must be a whole number or one of the following:\n',
          '     "(x) day(s)"\n',
          '     "(x) weeks(s)"\n',
          '     "(x) months(s)"\n',
          '     "(x) quarter(s)"\n',
          '     "(x) years(s)"\n',
          call. = FALSE
        )
      } else {
        period <- break_dates(x, interval, firstdate)
      }
    } else {
      type <- get_interval_type(interval)
      if (type == "week") {
        fd <- get_week_start(interval)
        n <- get_interval_number(interval)
        tmp_interval <- paste(n, "weeks")
        tmp <- as_utc_posixlt_from_int(as.Date(firstdate))$wday
        tmp <- 1L + (tmp - 1) %% 7L
        firstdate <- firstdate - (tmp - fd) %% 7
        period <- break_dates(x, tmp_interval, as.Date(firstdate))
        period <- as.Date(as_yrwk(period, firstday = fd))
      } else if (type == "month") {
        period <- break_dates(x, interval, as.Date(as_yrmon(firstdate)))
        period <- as.Date(as_yrmon(period))
      } else if (type == "quarter") {
        period <- break_dates(x, interval, as.Date(as_yrqtr(firstdate)))
        period <- as.Date(as_yrqtr(period))
      } else if (type == "year") {
        period <- break_dates(x, interval, as.Date(as_yr(firstdate)))
        period <- as.Date(as_yr(period))
      } else {
        period <- break_dates(x, interval, firstdate)
      }
    }
  } else {
    stop(
      "`interval` not valid.  See `?as_period` for valid intervals",
      call. = FALSE
    )
  }

  # create class
  period <- new_period(
    unclass(period),
    interval = interval
  )

  # finishing touches
  period[is.na(x)] <- NA_real_
  names(period) <- names(x)
  period
}


#' @rdname as_period
#' @export
as_period.POSIXt <- function(x, interval = 1L, firstdate = NULL, ...) {

  # convert to date
  if (is.null(firstdate)) {
    firstdate <- as.POSIXlt(min(x, na.rm = TRUE))
  } else {
    if(!inherits(firstdate, "POSIXt")) {
      stop("`firstdate` should have the same class (POSIXt) as `x`", call. = FALSE)
    }
    if (firstdate > min(x, na.rm = TRUE)) {
      stop("`firstdate` should be at or before the minimum date in `x`", call. = FALSE)
    }
  }
  firstdate <- as.Date(firstdate, tz = tzone(firstdate))

  x <- as.POSIXlt(x)
  out <- as.Date(x, tz = tzone(x))
  out <- as_period.Date(out, firstdate = firstdate, interval = interval)

  # finishing touches
  out[is.na(x)] <- NA_real_
  names(out) <- names(x)
  out
}


#' @rdname as_period
#' @export
as_period.character <- function(x, interval = 1L, firstdate = NULL, ...) {

  # ISO 8601 standard (YYYY-MM-DD)
  iso_pattern <- "(^\\d{4}-(0[1-9]|1[012])-(0[1-9]|[12][0-9]|3[0-1])$)"


  # iso_pattern is allowed, as are NA's
  allowed <- grepl(iso_pattern, trimws(x))
  allowed[is.na(x)] <- TRUE
  if (!all(allowed)) {
    stop(
      "Not all dates are in a valid formate:",
      sprintf("The first incorrect date is: %s", x[!allowed][1]),
      call. = FALSE
    )
  }

  # remove extraneous whitespace
  dat <- trimws(x)

  if (!is.null(firstdate)) {
    if (!(grepl(iso_pattern, firstdate) || is.Date(firstdate) || is.null(firstdate))) {
      stop(
        "`firstdate` must a character vector convertible to Date or a Date object",
        call. = FALSE
      )
    }

    if (grepl(iso_pattern, firstdate)) {
      firstdate <- as.Date(firstdate)
    }
  }

  # convert to dates
  dat <- as.Date(dat)

  # convert to period
  dat <- as_period.Date(dat, interval = interval, firstdate = firstdate)
  names(dat) <- names(x)
  dat
}


#' @rdname as_period
#' @export
as_period.factor <- function(x, interval = 1L, firstdate = NULL, ...) {
  as_period.character(as.character(x), firstdate = NULL, interval = 1L, ...)
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------- FORMATING / PRINTING -------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
format.period <- function(x, ...) {
  if (length(x) == 0) return(character(0))
  sprintf("%s to %s", format.Date(new_date(x)), format.Date(new_date(x + 1) - 1))
}

#' @export
print.period <- function(x, ...) {
  interval <- attr(x, "interval")
  if (is.integer(interval)) {
    interval <- sprintf("%d days", interval)
  }
  cat(sprintf("<period> interval = %s\n", interval))
  print(format.period(x, ...))
  invisible(x)
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------- METHODS: CONVERSIONS FROM period -------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
as.POSIXct.period <- function(x, tz = "UTC", ...) {
  if (tz == "UTC") {
    as_utc_posixct_from_int(x)
  } else {
    as_zoned_posixct_from_int(x, tz = tz)
  }
}


#' @export
as.POSIXlt.period <- function(x, tz = "UTC", ...) {
  if (tz == "UTC") {
    as_utc_posixlt_from_int(x)
  } else {
    as_zoned_posixlt_from_int(x, tz = tz)
  }
}


#' @export
as.Date.period <- function(x, ...) {
  attributes(x) <- NULL
  new_date(x)
}


#' @export
as.character.period <- function(x, ...) format(x, ...)


#' @export
as.list.period <- function(x, ...) {
  dat <- unclass(x)
  dur <- attr(x, "interval")
  lapply(dat, new_period, interval = dur)
}

#' @export
as.numeric.period <- function(x, ...) {
  attributes(x) <- NULL
  x
}


# This code is the same as that of the as.data.frame.yearmon code in Zoo by
# Achim Zeileis et al.
#' @export
as.data.frame.period <- function(x, row.names = NULL, optional = FALSE, ...) {
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
is.numeric.period <- function(x) FALSE


#' @export
`[.period` <- function (x, ..., drop = TRUE) {
  cl <- oldClass(x)
  interval <- attr(x, "interval")
  class(x) <- NULL
  val <- NextMethod()
  class(val) <- cl
  attr(val, "interval") <- interval
  val
}


#' @export
`[[.period` <- function (x, ..., drop = TRUE) {
  cl <- oldClass(x)
  interval <- attr(x, "interval")
  class(x) <- NULL
  val <- NextMethod()
  class(val) <- cl
  attr(val, "interval") <- interval
  val
}


#' @export
`[<-.period` <- function(x, i, value) {
  cl <- oldClass(x)
  interval <- attr(x, "interval")
  if (!all(inherits(value, "period") | is.na(value))) {
    stop("Can only assign period objects in to a period object", call. = FALSE)
  }
  val <- NextMethod("[<-")
  class(val) <- cl
  attr(val, "interval") <- interval
  val
}


#' @export
rep.period <- function (x, ..., drop = TRUE) {
  cl <- oldClass(x)
  interval <- attr(x, "interval")
  class(x) <- NULL
  val <- NextMethod()
  class(val) <- cl
  attr(val, "interval") <- interval
  val
}


#' @export
unique.period <- function (x, incomparables = FALSE, ...) {
  cl <- oldClass(x)
  interval <- attr(x, "interval")
  class(x) <- NULL
  val <- NextMethod()
  class(val) <- cl
  attr(val, "interval") <- interval
  val
}


#' @export
c.period <- function(..., recursive = FALSE, use.names = TRUE) {
  dots <- list(...)

  if (!all(vapply(dots, inherits, logical(1), what = "period") | is.na(dots))) {
    stop(
      "To combine <period> objects with different objects first convert to a common class",
      call. = FALSE
    )
  }

  interval <- attr(dots[[1]], "interval")
  intervals <- lapply(dots, attr, numeric(1), which = "interval")
  if (!all(vapply(intervals, function(x) {is.null(x) || x == interval}, logical(1)))) {
    stop(
      "Unable to combine <period> objects with different `interval` attributes",
      call. = FALSE
    )
  }

  dots_dates <- unlist(dots)
  dots_min <- new_date(min(dots_dates))
  dots_max <- new_date(max(dots_dates))
  range <- seq.Date(from = dots_min, to = dots_max, by = 1)
  range <- as.Date(as_period(range, interval = interval))
  if (!all(vapply(dots, function(x) all(x %in% range), logical(1)))){
    stop("Incompatible <period> objects.", call. = FALSE)
  }

  res <- NextMethod()
  class(res) <- c("period", "grate")
  attr(res, "interval") <- interval
  res
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# --------------------------------- MATHS --------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
Math.period <- function(x, ...) {
  .fn <- .Generic
  fn <- switch(
    .fn,
    is.nan = is.nan.period(x),
    is.finite = is.finite.period(x),
    is.infinite = is.infinite.period(x),
    stop(sprintf("`%s()` is not supported for <period>", .fn), call. = FALSE)
  )
}

is.nan.period <- function(x, ...) vector("logical", length(x))

is.finite.period <- function(x, ...) !is.na(unclass(x))

is.infinite.period <- function(x, ...) vector("logical", length(x))


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ---------------------------------- OPS ---------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
Ops.period <- function(e1, e2) {
  op <- .Generic
  if (op %in% c("==", "!=", "<", ">", "<=", ">=")) {
    if (inherits(e2, "period")) {
      return(NextMethod())
    } else {
      stop("Can only compare <period> objects with <period> objects", call. = FALSE)
    }
  }

  switch(
    op,
    "+" = {
      if (missing(e2)) {
        return(e1)
      } else if (inherits(e1, "period") && inherits(e2, "period")) {
        stop("Cannot add <period> objects to each other", call. = FALSE)
      } else if (inherits(e1, "period") && (all(is.wholenumber(unclass(e2)), na.rm = TRUE))) {
        add_periods(e1, unclass(e2))
      } else if (inherits(e2, "period") && (all(is.wholenumber(unclass(e1)),  na.rm = TRUE))) {
        add_periods(e2, unclass(e1))
      } else {
        stop("Can only add whole numbers to <period> objects", call. = FALSE)
      }
    },
    "-" = {
      if (missing(e2)) {
        stop("Cannot negate a <period> object", call. = FALSE)
      } else if (inherits(e1, "period") && !(inherits(e2, "period")) && (all(is.wholenumber(unclass(e2)), na.rm = TRUE))) {
        add_periods(e1, -unclass(e2))
      } else {
        stop("Can only subtract whole numbers from <period> objects", call. = FALSE)
      }
    },
    stop(sprintf("%s is not compatible with <period> objects", op), call. = FALSE)
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
Summary.period <- function (..., na.rm)
{
  ok <- switch(.Generic, max = TRUE, min = TRUE, range = TRUE, FALSE)
  if (!ok) stop(.Generic, " not defined for period objects")
  interval <- attr(list(...)[[1]], "interval")
  val <- NextMethod(.Generic)
  class(val) <- oldClass(list(...)[[1]])
  attr(val, "interval") <- interval
  val
}

# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------- INTERNALS ------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

new_period <- function(x = numeric(), interval) {
  structure(x, interval = interval, class = c("period", "grate"))
}


get_interval_number <- function(x) {
  if (!grepl("^\\d", x)) return(1L)
  as.integer(gsub("^(\\d*).+$", "\\1", x))
}


get_interval_type <- function(x) {

  if (!is.character(x)) {
    return(typeof(x))
  }

  day <- "^\\s*days?\\s*$|\\sdays?\\s+|\\sdays?\\s*$"
  if (grepl(day, x, ignore.case = TRUE)) {
    return("day")
  } else if (grepl("week", x, ignore.case = TRUE)) {
    return("week")
  }  else if (grepl("month", x, ignore.case = TRUE)) {
    return("month")
  } else if (grepl("quarter", x, ignore.case = TRUE)) {
    return("quarter")
  } else if (grepl("year", x, ignore.case = TRUE)) {
    return("year")
  }  else {
    return("day")
  }
}


break_dates <- function(x, interval, firstdate) {
  breaks <- seq(from = firstdate, to = max(x, na.rm = TRUE), by = interval)
  period <- cut(x, breaks = c(breaks, Inf), labels = FALSE, right = FALSE)
  breaks[period]
}


valid_interval <- function(interval) {

  # integer intervals are fine
  if (is.integer(interval)) {
    return(TRUE)
  }

  # numeric intervals are ok if they are effectively integers
  if (is.numeric(interval)) {
    if (all(is.wholenumber(interval))) {
      return(TRUE)
    }
  }

  # Ensure interval is of length one
  if (length(interval) != 1L) {
    stop(sprintf(
      "Exactly one value should be provided for `interval` (%d provided)",
      length(interval),
      call. = FALSE
    ))
  }

  # character intervals are more tricky
  if (is.character(interval)) {
    if (!valid_date_period_character(interval)) {
      suppressWarnings(interval <- as.numeric(interval))
      if (is.na(interval)) {
        stop(
          'The interval must be a whole number or one of the following:\n',
          '     "(x) day(s)"\n',
          '     "(x) weeks(s)"\n',
          '     "(x) epiweeks(s)"\n',
          '     "(x) isoweeks(s)"\n',
          '     "(x) months(s)"\n',
          '     "(x) quarter(s)"\n',
          '     "(x) years(s)"\n',
          call. = FALSE
        )
      } else {
        return(TRUE)
      }
    }
  }

  return(TRUE)
}

valid_date_period_character <- function(x) {
  # have to ensure saturday does not cause issues
  day <- paste0(
    "^\\s*days?\\s*$|",
    "\\sdays?\\s+|",
    "\\sdays?\\s*$|"
  )
  pattern <- paste0(day,"week|epiweek|isoweek|month|quarter|year")
  grepl(pattern, x, ignore.case = TRUE)
}

add_periods <- function(x, n) {
  out <- unclass(x)
  d <- attr(x, "interval")
  if (is.integer(d)) {
    out <- out + (n * d)
  } else {
    dn <- get_interval_number(d) * n
    dt <- get_interval_type(d)
    by = paste(dn, dt)
    out <- vapply(
      new_date(out),
      function(x) seq.Date(x, by = by, length.out = 2)[2],
      double(1)
    )
  }

  start <- min(attr(x, "firstdate"), min(out))
  new_period(out, interval = d)
}





