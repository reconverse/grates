# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------------------- AS_YRMON ------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' Convert an object to a yrmon
#'
#' @description
#' - Date, POSIXct, and POSIXlt are converted directly.  Any day, hour, minute,
#'   or second components are dropped. POSIXct and POSIXlt are converted to
#'   dates via `as.date()` with the timezone respected.
#'
#' - Character input is assumed to be provided in either ISO 8601 standard
#'   format, i.e. "yyyy-mm-dd".
#'
#' @param x `An object to coerce to yrmon.
#' @param ... Not used.
#'
#' @return A `yrmon` object.
#'
#' @examples
#' as_yrmon(Sys.Date())
#' as_yrmon(as.POSIXct("2019-03-04 01:01:01", tz = "America/New_York"))
#' as_yrmon("2019-05-03")
#'
#' @note Internally `yrmon` objects are stored as the number of months
#'   (starting at 0) since the Unix Epoch (1970-01-01).
#'
#' @references The algorithm to convert between dates and yrmon, comes from the
#'   work of Davis Vaughan in the unreleased
#'   [datea](https://github.com/DavisVaughan/datea/) package.
#'
#' @export
as_yrmon <- function(x, ...) {
  UseMethod("as_yrmon")
}


#' @rdname as_yrmon
#' @export
as_yrmon.default <- function(x, ...) {
  stop(sprintf("Can't convert a <%s> to a <yrmon>" , class(x)[1]), call. = FALSE)
}


#' @rdname as_yrmon
#' @export
as_yrmon.yrmon <- function(x, ...) {
  x
}


#' @rdname as_yrmon
#' @export
as_yrmon.Date <- function(x, ...) {

  # convert to posixlt
  x <- as_utc_posixlt_from_int(x)

  # Now calculate the month
  yr <- x$year + 1900L
  mon <- x$mon
  mon <- (yr - 1970L) * 12L + mon

  # create class
  yrmon <- new_yrmon(mon)

  # finishing touches
  yrmon[is.na(mon)] <- NA_real_
  names(yrmon) <- names(x)
  yrmon
}


#' @rdname as_yrmon
#' @export
as_yrmon.POSIXlt <- function(x, ...) {

  # Now calculate the month
  mon <- (x$year - 70L) * 12L + x$mon

  # create class
  yrmon <- new_yrmon(mon)

  # finishing touches
  yrmon[is.na(mon)] <- NA_real_
  names(yrmon) <- names(x)
  yrmon

}


#' @rdname as_yrmon
#' @export
as_yrmon.POSIXct <- function(x, ...) {

  out <- unclass(x) / 86400

  # First convert to posixlt to deal with timezones and then date
  out <- as_zoned_posixlt_from_int(out, tz = tzone(x))

  # Now calculate the month
  mon <- (out$year - 70L) * 12L + out$mon

  # create class
  yrmon <- new_yrmon(mon)

  # finishing touches
  yrmon[is.na(mon)] <- NA_real_
  names(yrmon) <- names(x)
  yrmon

}



#' @rdname as_yrmon
#' @export
as_yrmon.character <- function(x, ...) {

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

  # convert to dates
  dat <- as.Date(dat)

  # convert to yrmon
  dat <- as_yrmon.Date(dat)
  names(dat) <- names(x)
  dat
}


#' @rdname as_yrmon
#' @export
as_yrmon.factor <- function(x, ...) {
  as_yrmon.character(as.character(x))
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------- FORMATING / PRINTING -------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
format.yrmon <- function(x, format = "%Y-%b", ...) {
  if (length(x) == 0) return(character(0))
  format.Date(as.Date(x), format = format)
}

#' @export
print.yrmon <- function(x, format = "%Y-%b", ...) {
  print(format.yrmon(x, format = format, ...))
  invisible(x)
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------- METHODS: CONVERSIONS FROM YRMON -------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
as.POSIXct.yrmon <- function(x, tz = "UTC", ...) {
  attributes(x) <- NULL
  days <- month_to_days(x)
  if (tz == "UTC") {
    as_utc_posixct_from_int(days)
  } else {
    as_zoned_posixct_from_int(days, tz = tz)
  }
}


#' @export
as.POSIXlt.yrmon <- function(x, tz = "UTC", ...) {
  attributes(x) <- NULL
  days <- month_to_days(x)
  if (tz == "UTC") {
    as_utc_posixlt_from_int(days)
  } else {
    as_zoned_posixlt_from_int(days, tz = tz)
  }

}


#' @export
as.Date.yrmon <- function(x, ...) {
  attributes(x) <- NULL
  days <- month_to_days(x)
  new_date(days)
}


#' @export
as.character.yrmon <- function(x, ...) format(x, ...)


#' @export
as.list.yrmon <- function(x, ...) lapply(unclass(x), new_yrmon)

#' @export
as.numeric.yrmon <- function(x, ...) {
  attributes(x) <- NULL
  x
}



# This code is the same as that of the as.data.frame.yearmon code in Zoo by
# Achim Zeileis et al.
#' @export
as.data.frame.yrmon <- function(x, row.names = NULL, optional = FALSE, ...) {
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
is.numeric.yrmon <- function(x) FALSE


#' @export
`[.yrmon` <- function (x, ..., drop = TRUE) {
  cl <- oldClass(x)
  class(x) <- NULL
  val <- NextMethod()
  class(val) <- cl
  val
}


#' @export
`[[.yrmon` <- function (x, ..., drop = TRUE) {
  cl <- oldClass(x)
  class(x) <- NULL
  val <- NextMethod()
  class(val) <- cl
  val
}


#' @export
`[<-.yrmon` <- function(x, i, value) {
  cl <- oldClass(x)
  if (!all(inherits(value, "yrmon") | is.na(value))) {
    stop("Can only assign yrmon objects in to a yrmon object", call. = FALSE)
  }
  val <- NextMethod("[<-")
  class(val) <- cl
  val
}


#' @export
rep.yrmon <- function (x, ..., drop = TRUE) {
  cl <- oldClass(x)
  class(x) <- NULL
  val <- NextMethod()
  class(val) <- cl
  val
}


#' @export
unique.yrmon <- function (x, incomparables = FALSE, ...) {
  cl <- oldClass(x)
  class(x) <- NULL
  val <- NextMethod()
  class(val) <- cl
  val
}


#' @export
c.yrmon <- function(..., recursive = FALSE, use.names = TRUE) {
  dots <- list(...)
  is_mon <- vapply(dots, inherits, logical(1), what = "yrmon")
  is_qtr <- vapply(dots, inherits, logical(1), what = "yrqtr")
  is_na <- is.na(dots)

  if (!all(is_mon | is_qtr | is_na)) {
    stop(
      "To combine <yrmon> objects with different objects first convert to a common class",
      call. = FALSE
    )
  }
  if (any(is_qtr)) {
    dots[is_mon] <- lapply(dots[is_mon], as_yrqtr)
    res <- unlist(dots)
    class(res) <- c("yrqtr", "grate")
  } else {
    res <- unlist(dots)
    class(res) <- c("yrmon", "grate")
  }
  res
}


#' @export
seq.yrmon <- function(from, to, by = 1L, ...) {
  by <- int_cast(by)

  if (!inherits(to, "yrmon")) {
    stop("Can only create a sequence between two `yrmon` objects", call. = FALSE)
  }

  from <- as.numeric(from)
  to = as.numeric(to)
  out <- seq(from = from, to = to, by = by)
  new_yrmon(out)
}



# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# --------------------------------- MATHS --------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
Math.yrmon <- function(x, ...) {
  .fn <- .Generic
  fn <- switch(
    .fn,
    is.nan = is.nan.yrmon(x),
    is.finite = is.finite.yrmon(x),
    is.infinite = is.infinite.yrmon(x),
    stop(sprintf("`%s()` is not supported for <yrmon>", .fn), call. = FALSE)
  )
}

is.nan.yrmon <- function(x, ...) vector("logical", length(x))

is.finite.yrmon <- function(x, ...) !is.na(unclass(x))

is.infinite.yrmon <- function(x, ...) vector("logical", length(x))


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ---------------------------------- OPS ---------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
Ops.yrmon <- function(e1, e2) {
  op <- .Generic
  if (op %in% c("==", "!=", "<", ">", "<=", ">=")) {
    if (inherits(e2, "yrmon")) {
      return(NextMethod())
    } else {
      stop("Can only compare <yrmon> objects with <yrmon> objects", call. = FALSE)
    }
  }

  switch(
    op,
    "+" = {
      if (missing(e2)) {
        return(e1)
      } else if (inherits(e1, "yrmon") && inherits(e2, "yrmon")) {
        stop("Cannot add <yrmon> objects to each other", call. = FALSE)
      } else if (inherits(e1, "yrmon") && (all(is.wholenumber(unclass(e2)), na.rm = TRUE))) {
        new_yrmon(unclass(e1) + e2)
      } else if (inherits(e2, "yrmon") && (all(is.wholenumber(unclass(e1)),  na.rm = TRUE))) {
        new_yrmon(unclass(e2) + e1)
      } else {
        stop("Can only add whole numbers to <yrmon> objects", call. = FALSE)
      }
    },
    "-" = {
      if (missing(e2)) {
        stop("Cannot negate a <yrmon> object", call. = FALSE)
      } else if (inherits(e2, "yrmon")) {
        if (inherits(e1, "yrmon")) {
          as.integer(e1) - as.integer(e2)
        } else if (all(is.wholenumber(unclass(e1)),  na.rm = TRUE)) {
          stop("Can only subtract from a <yrmon> object not vice-versa", call. = FALSE)
        }
      } else if (inherits(e1, "yrmon") && (all(is.wholenumber(unclass(e2)), na.rm = TRUE))) {
        new_yrmon(unclass(e1) - as.numeric(e2))
      } else {
        stop("Can only subtract whole numbers and other <yrmon> objects from <yrmon> objects", call. = FALSE)
      }
    },
    stop(sprintf("%s is not compatible with <yrmon> objects", op), call. = FALSE)
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
Summary.yrmon <- function (..., na.rm)
{
  ok <- switch(.Generic, max = TRUE, min = TRUE, range = TRUE, FALSE)
  if (!ok) stop(.Generic, " not defined for yrmon objects")
  val <- NextMethod(.Generic)
  class(val) <- oldClass(list(...)[[1]])
  val
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------- INTERNALS ------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

new_yrmon <- function(x = numeric()) {
  structure(x, class = c("yrmon", "grate"))
}

delayedAssign(
  "DAYS_IN_MONTH",
  c(31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)
)

delayedAssign(
  "DAYS_BEFORE_MONTH",
  c(0L, 31L, 59L, 90L, 120L, 151L, 181L, 212L, 243L, 273L, 304L, 334L)
)

month_to_days <- function(months) {
  year <- months %/% 12L + 1970L
  month <- months %% 12L + 1L
  days_before_year(year) + days_before_month(year, month) - 719162L
}

days_before_year <- function(year = integer()) {
  year <- year - 1L
  (year * 365) + (year %/% 4) - (year %/% 100) + (year %/% 400)
}

days_before_month <- function(year, month) {
  DAYS_BEFORE_MONTH[month] + ((month > 2) & is_leap_year(year))
}

days_in_month <- function(year, month) {
  DAYS_IN_MONTH[month] + ((month == 2) & is_leap_year(year))
}

add_months <- function(x, n) {
  x <- as_utc_posixlt_from_int(x)
  x$mon <- x$mon + n
  x <- as.Date(x)
  new_yrmon(unclass(x))
}
