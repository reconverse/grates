# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------------------- AS_MONTH ------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' Convert an object to a monthly grates object
#'
#' @description
#' - Date, POSIXct, and POSIXlt are converted, with the timezone respected,
#'   using [clock::as_date()].
#'
#' - Character input is parsed using [clock::date_parse()].
#'
#' @param x An object to coerce to monthly.
#' @param interval An integer indicating the (fixed) number of months used for
#'   grouping; defaults to 1 month.
#' @param origin The date to anchor the intervals from.  If NULL (default)
#'   the earliest date in the vector will be used. If specified, `origin` must
#'   have the same class as x.
#' @param ... Not currently used.
#'
#' @return A `grate_month` object.
#'
#' @examples
#' as_month(Sys.Date())
#' as_month(as.POSIXct("2019-03-04 01:01:01", tz = "America/New_York"), n = 2)
#' as_month("2019-05-03")
#'
#' @note Internally `grate_month` objects are stored as the number of months
#'   (starting at 0) since the Unix Epoch (1970-01-01).
#'
#' @references The algorithm to convert between dates and months relative to the
#'   UNIX Epoch comes from the work of Davis Vaughan in the unreleased
#'   [datea](https://github.com/DavisVaughan/datea/) package.
#'
#' @importFrom clock date_parse date_group as_date
#' @export
as_month <- function(x, interval = 1, origin = NULL, ...) {
  UseMethod("as_month")
}


#' @rdname as_month
#' @export
as_month.default <- function(x, interval = 1, origin = NULL, ...) {
  stop(
    sprintf("Can't convert a <%s> to a <grate_month>" , class(x)[1]),
    call. = FALSE
  )
}

#' @rdname as_month
#' @export
as_month.Date <- function(x, interval = 1, origin = NULL, ...) {

  if (!is.null(origin)) {
    if(!inherits(origin, "Date")) {
      stop("`x` and `origin` must have matching (Date) classes", call. = FALSE)
    }
    if (origin > min(x, na.rm = TRUE)) {
      stop("`origin` should be at or before the minimum date in `x`", call. = FALSE)
    }
    if (length(origin) != 1L) {
      stop(
        sprintf("Exactly one value should be provided for `origin` (%d provided)", length(origin)),
        call. = FALSE
      )
    }
  }

  # Ensure numeric n is a whole number
  if (is.numeric(interval)) {
    interval <- int_cast(interval)
    if (interval < 1L) stop("interval must be positive (>= 1)", call. = FALSE)
  }

  nms <- names(x)
  if (is.null(origin)) origin <- min(x, na.rm = TRUE)

  # remove values before origin
  x <- x[x >= origin]

  origin <- date_group(origin, precision = "month", n = 1)
  by <- sprintf("%d months", interval)
  months <- seq(from = origin, to = max(x, na.rm = TRUE), by = by)
  idx <- cut(x, breaks = c(months, Inf), labels = FALSE, right = FALSE)
  x <- months[idx]

  # convert to posixlt
  x <- as_utc_posixlt_from_int(x)

  # Now calculate the month
  yr <- x$year + 1900L
  mon <- x$mon
  mon <- (yr - 1970L) * 12L + mon

  # create class
  grate_month <- new_grate_month(mon, interval = interval)

  # finishing touches
  grate_month[is.na(mon)] <- NA_real_
  names(grate_month) <- nms
  grate_month
}

#' @importFrom clock as_date
#' @rdname as_month
#' @export
as_month.POSIXt <- function(x, interval = 1, origin = NULL, ...) {
  x <- as_date(x)
  if (is.null(origin)) {
    origin <- min(x, na.rm = TRUE)
  } else {
    origin <- as_date(origin)
  }
  as_month.Date(x = x, interval = interval, origin = origin, ...)
}

#' @inheritParams clock::date_parse
#' @rdname as_month
#' @export
as_month.character <- function(x, interval = 1, origin = NULL, format = NULL, locale = clock_locale(), ...) {
  x <- date_parse(x, format = format, locale = locale)
  if (all(is.na(x))) stop("Unable to parse any entries of x as Dates")
  if (is.null(origin)) {
    origin <- min(x, na.rm = TRUE)
  } else {
    origin <- date_parse(origin, format = format, locale = locale)
  }
  as_month.Date(x, interval, origin)
}


#' @inheritParams clock::date_parse
#' @rdname as_month
#' @export
as_month.factor <- function(x, interval = 1, origin = NULL, format = NULL, locale = clock_locale(), ...) {
  if (!is.null(origin)) origin <- as.character(origin)

  as_month.character(
    as.character(x),
    interval = interval,
    origin = origin,
    format = format,
    locale = locale
  )
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------- FORMATING / PRINTING -------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' Print a grate_month object
#'
#' @param x A grate_month object.
#' @param format The format to use for the bounds of each grate_month entry.
#' @param sep Where more than one month is grouped with others, `sep` is placed
#'   between the upper and lower bounds when printing.
#' @param ... Not currently used.
#'
#' @export
print.grate_month <- function(x, format = "%Y-%b", sep = "to", ...) {
  interval <- attr(x, "interval")
  cat(sprintf("<grate_month: interval = %d>\n", interval))
  print(format.grate_month(x, format = format, sep = sep))
  invisible(x)
}


#' @rdname print.grate_month
#' @export
format.grate_month <- function(x, format = "%Y-%b", sep = "to", ...) {
  if (length(x) == 0) return(character(0))
  interval <- attr(x, "interval")
  if (interval > 1) {
    out <- sprintf(
      "%s %s %s",
      format.Date(as.Date(x), format = format),
      sep,
      format.Date(as.Date(x + 1) - 1, format = format)
    )
  } else {
    out <- format.Date(as.Date(x), format = format)
  }
  out[is.na(x)] <- NA_character_
  out
}



# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ----------------- METHODS: CONVERSIONS FROM grate_month ----------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
as.Date.grate_month <- function(x, ...) {
  attributes(x) <- NULL
  days <- month_to_days(x)
  new_date(days)
}

#' @export
as.POSIXct.grate_month <- function(x, tz = "UTC", ...) {
  attributes(x) <- NULL
  days <- month_to_days(x)
  out <- new_date(days)
  out <- as_zoned_time(out, zone = tz, nonexistent = "roll-forward", ambiguous = "latest")
  as_date_time(out)
}

#' @export
as.POSIXlt.grate_month <- function(x, tz = "UTC", ...) {
  attributes(x) <- NULL
  days <- month_to_days(x)
  out <- new_date(days)
  out <- as_zoned_time(out, zone = tz, nonexistent = "roll-forward", ambiguous = "latest")
  as.POSIXlt(out)
}

#' @export
as.character.grate_month <- function(x, ...) format(x, ...)

#' @export
as.list.grate_month <- function(x, ...) {
  interval <- attr(x, "interval")
  lapply(unclass(x), new_grate_month, interval = interval)
}

#' @export
as.numeric.grate_month <- function(x, ...) {
  attributes(x) <- NULL
  x
}

#' @export
as.data.frame.grate_month <- as.data.frame.vector


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------ METHODS: MISCELLANEOUS ------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
is.numeric.grate_month <- function(x) FALSE


#' @export
`[.grate_month` <- function (x, ..., drop = TRUE) {
  cl <- oldClass(x)
  interval <- attr(x, "interval")
  class(x) <- class(x)[-1]
  val <- NextMethod()
  class(val) <- cl
  attr(val, "interval") <- interval
  val
}


#' @export
`[[.grate_month` <- function (x, ..., drop = TRUE) {
  cl <- oldClass(x)
  interval <- attr(x, "interval")
  class(x) <- NULL
  val <- NextMethod()
  class(val) <- cl
  attr(val, "interval") <- interval
  val
}


#' @export
`[<-.grate_month` <- function(x, i, value) {
  cl <- oldClass(x)
  interval <- attr(x, "interval")
  if (!all(inherits(value, "grate_month") | is.na(value))) {
    stop("Can only assign grate_month objects in to a grate_month object", call. = FALSE)
  }
  if (!identical(interval, attr(value, "interval")) && !is.na(value)) {
    stop("Can only combine <grate_month> objects with identical values of `interval`")
  }
  val <- NextMethod("[<-")
  class(val) <- cl
  attr(val, "interval") <- interval

  if (!is.na(value)) {
    dat <- diff(as.numeric(val))
    dat <- dat[!is.na(dat)]
    if (length(unique(dat)) > 1L) {
      stop("Incompatible <grate_month> objects. Are they anchored to different origins?", call. = FALSE)
    }
  }

  val
}


#' @export
rep.grate_month <- function (x, ..., drop = TRUE) {
  cl <- oldClass(x)
  interval <- attr(x, "interval")
  class(x) <- NULL
  val <- NextMethod()
  class(val) <- cl
  attr(val, "interval") <- interval
  val
}


#' @export
unique.grate_month <- function (x, incomparables = FALSE, ...) {
  cl <- oldClass(x)
  interval <- attr(x, "interval")
  class(x) <- NULL
  val <- NextMethod()
  class(val) <- cl
  attr(val, "interval") <- interval
  val
}


#' @export
c.grate_month <- function(...) {

  dots <- list(...)
  interval <- attr(dots[[1]], "interval")

  is_mon <- vapply(dots, inherits, logical(1), what = "grate_month")
  is_na <- is.na(dots)
  if (!all(is_mon | is_na)) {
    stop(
      "To combine <grate_month> objects with other objects ensure they are a common class",
      call. = FALSE
    )
  }

  valid <- vapply(
    dots,
    function(x) {
      if (all(is.na(x))) {
        TRUE
      } else {
        identical(interval, attr(x, "interval"))
      }
    },
    logical(1)
  )
  if(!all(valid)) {
    stop("Can only combine <grate_month> objects with identical values of `interval`")
  }

  res <- new_grate_month(
    unlist(dots, recursive = FALSE, use.names = TRUE),
    interval = interval
  )


  tmp <- res[!is.na(res)]
  if (length(tmp)) {
    tmp <- diff(as.numeric(tmp))
    tmp <- tmp[!is.na(tmp)]
    if (length(unique(tmp)) > 1L) {
      stop("Incompatible <grate_month> objects. Are they anchored to different origins?", call. = FALSE)
    }
  }

  res
}


#' @export
seq.grate_month <- function(from, to, by = 1L, ...) {
  by <- int_cast(by)

  if (!inherits(to, "grate_month")) {
    stop("Can only create a sequence between two `grate_month` objects", call. = FALSE)
  }

  c(from, to) # this will trigger warning if incompatible

  interval <- attr(from, "interval")
  from <- as.numeric(from)
  to = as.numeric(to)
  out <- seq(from = from, to = to, by = interval*by)
  new_grate_month(out, interval = interval)
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# --------------------------------- MATHS --------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

is.na.grate_month <- function(x, ...) is.na(unclass(x))

is.nan.grate_month <- function(x, ...) vector("logical", length(x))

is.finite.grate_month <- function(x, ...) !is.na(unclass(x))

is.infinite.grate_month <- function(x, ...) vector("logical", length(x))

#' @export
Math.grate_month <- function(x, ...) {
  .fn <- .Generic
  fn <- switch(
    .fn,
    is.na = is.na.grate_month(x),
    is.nan = is.nan.grate_month(x),
    is.finite = is.finite.grate_month(x),
    is.infinite = is.infinite.grate_month(x),
    stop(sprintf("`%s()` is not supported for <grate_month>", .fn), call. = FALSE)
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
Summary.grate_month <- function (..., na.rm)
{
  ok <- switch(.Generic, max = TRUE, min = TRUE, range = TRUE, FALSE)
  if (!ok) stop(.Generic, " not defined for <grate_month> objects")
  val <- NextMethod(.Generic)
  class(val) <- oldClass(list(...)[[1]])
  attr(val, "interval") <- attr(list(...)[[1]], "interval")
  val
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ---------------------------------- OPS ---------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
Ops.grate_month <- function(e1, e2) {
  op <- .Generic
  if (op %in% c("==", "!=", "<", ">", "<=", ">=")) {
    if (inherits(e2, "grate_month")) {
      return(NextMethod())
    } else {
      stop("Can only compare <grate_month> objects with <grate_month> objects", call. = FALSE)
    }
  }

  switch(
    op,
    "+" = {
      if (missing(e2)) {
        return(e1)
      } else if (inherits(e1, "grate_month") && inherits(e2, "grate_month")) {
        stop("Cannot add <grate_month> objects to each other", call. = FALSE)
      } else if (inherits(e1, "grate_month") && (all(is.wholenumber(unclass(e2)), na.rm = TRUE))) {
        interval <- attr(e1, "interval")
        new_grate_month(unclass(e1) + (interval * e2), interval = interval)
      } else if (inherits(e2, "grate_month") && (all(is.wholenumber(unclass(e1)),  na.rm = TRUE))) {
        interval <- attr(e2, "interval")
        new_grate_month(unclass(e2) + (interval * e1), interval = interval)
      } else {
        stop("Can only add whole numbers to <grate_month> objects", call. = FALSE)
      }
    },
    "-" = {
      if (missing(e2)) {
        stop("Cannot negate a <grate_month> object", call. = FALSE)
      } else if (inherits(e2, "grate_month")) {
        if (inherits(e1, "grate_month")) {
          stop("Can only subtract wholenumbers from <grate_month> objects", call. = FALSE)
        } else if (all(is.wholenumber(unclass(e1)),  na.rm = TRUE)) {
          stop("Can only subtract from a <grate_month> object not vice-versa", call. = FALSE)
        }
      } else if (inherits(e1, "grate_month") && (all(is.wholenumber(unclass(e2)), na.rm = TRUE))) {
        interval <- attr(e1, "interval")
        new_grate_month(unclass(e1) - (interval * as.numeric(e2)), interval = interval)
      } else {
        stop("Can only subtract whole numbers from <grate_month> objects", call. = FALSE)
      }
    },
    stop(sprintf("%s is not compatible with <grate_month> objects", op), call. = FALSE)
  )
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------- INTERNALS ------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

new_grate_month <- function(x = numeric(), interval = integer()) {
  structure(x, class = "grate_month", interval = interval)
}


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
