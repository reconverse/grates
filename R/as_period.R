# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------- AS_PERIOD ------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' Convert an object to a period based grates object
#'
#' @description
#' - Date, POSIXct, and POSIXlt are converted, with the timezone respected,
#'   using [clock::as_date()].
#' - Character input is parsed using [clock::date_parse()].
#' - Integer inputs are converted as is. Numeric inputs are floored to integer
#'   before conversion.
#'
#' @param x An object to coerce to a period.
#' @param interval An integer indicating the (fixed) number of days used for
#'   grouping; defaults to 1.
#' @param origin The date to anchor the intervals from. All values prior to the
#'   given `origin` will be discarded. If NULL (default) the earliest value in
#'   the vector will be used. If specified, `origin` must have the same class
#'   as x.
#' @inheritParams clock::date_parse
#' @param ... Not currently used.
#'
#' @return
#'  - If `x` is numeric, a `<grate_int_period>` object except when interval
#'    equals `1`.  In this situation the returned object is an "integer" vector.
#'  - For other valid inputs, a `<grate_period>` object except, again, when
#'    interval equals 1, "day" or "days" (both without prefix).  In this
#'    situation the returned object is a "Date" vector.
#'
#' @examples
#' as_period(Sys.Date(), interval = 2)
#' as_period(as.POSIXct("2019-03-04 01:01:01", tz = "America/New_York"), interval = 3)
#' as_period("2019-05-03")
#'
#' @note Internally `period` objects are represented by the date at the
#'   beginning of the period and then stored as the number of days (starting at
#'   0) since the Unix Epoch (1970-01-01).
#'
#' @export
as_period <- function(x, ...) {
  UseMethod("as_period")
}


#' @rdname as_period
#' @export
as_period.default <- function(x, ...) {
  abort(sprintf("Can't convert a <%s> to a <grate_period>" , class(x)[1]))
}


#' @rdname as_period
#' @export
as_period.integer <- function(x, interval = 1, origin = NULL, ...) {

  check_dots_empty()

  if (!is.null(origin)) {
    origin <- vec_cast(origin, integer(), x_arg = "origin")
    vec_assert(origin, size = 1, arg = "origin")
    stopifnot("`origin` should be at or before the maximum date in `x`" = origin <= max(x, na.rm = TRUE))
  } else {
    origin <- min(x, na.rm = TRUE)
  }

  # remove values before origin
  x <- x[x >= origin]

  # Ensure interval is numeric and a whole number
  interval <- vec_cast(interval, integer(), x_arg = "interval")
  stopifnot("interval must be positive (>= 1)" = interval >= 1L)

  # return early if interval is one
  if (interval == 1L) return(x)

  # generate groupings
  period_starts <- seq.int(from = origin, to = max(x, na.rm = TRUE), by = interval)
  idx <- cut(x, breaks = c(period_starts, Inf), labels = FALSE, right = FALSE)
  out <- period_starts[idx]

  # create class
  grate_period <- new_grate_int_period(out, interval)

  # finishing touches
  grate_period[is.na(x)] <- NA_real_
  setNames(grate_period, names(x))
}


#' @rdname as_period
#' @export
as_period.numeric <- function(x, interval = 1, origin = NULL, ...) {
  x <- floor(x)
  as_period.integer(x, interval = interval, origin = origin)
}


#' @rdname as_period
#' @export
as_period.Date <- function(x, interval = 1, origin = NULL, ...) {

  check_dots_empty()

  # set origin to minimum value if none specified
  if (!is.null(origin)) {
    vec_assert(origin, ptype = new_date(), size = 1, arg = "origin")
    stopifnot("`origin` should be at or before the maximum value in `x`" = origin <= max(x, na.rm = TRUE))
    x <- x[x >= origin]
  } else {
    origin <- min(x, na.rm = TRUE)
  }

  # Ensure interval is numeric and a whole number
  interval <- vec_cast(interval, integer(), x_arg = "interval")
  stopifnot("interval must be positive (>= 1)" = interval >= 1L)

  # return early if interval is one
  if (interval == 1L) return(x)

  period_starts <- seq(from = origin, to = max(x, na.rm = TRUE), by = interval)
  idx <- cut(x, breaks = c(period_starts, Inf), labels = FALSE, right = FALSE)
  out <- period_starts[idx]

  # create class
  grate_period <- new_grate_period(out, interval)

  # finishing touches
  grate_period[is.na(x)] <- NA_real_
  setNames(grate_period, names(x))
}


#' @rdname as_period
#' @export
as_period.POSIXt <- function(x, interval = 1, origin = NULL, ...) {
  check_dots_empty()
  x = as_date(x)
  if (is.null(origin)) origin <- min(x, na.rm = TRUE)
  origin <- as_date(origin)
  as_period.Date(x = x, interval = interval, origin = origin, ...)
}


#' @rdname as_period
#' @export
as_period.character <- function(x, interval = 1, origin = NULL, format = NULL, locale = clock_locale(), ...) {
  check_dots_empty()
  x <- date_parse(x, format = format, locale = locale)
  if (all(is.na(x))) abort("Unable to parse any entries of x as Dates")
  if (is.null(origin)) {
    origin <- min(x, na.rm = TRUE)
  } else {
    origin <- date_parse(origin, format = format, locale = locale)
  }
  as_period.Date(x, interval = interval, origin = origin)
}


#' @inheritParams clock::date_parse
#' @rdname as_period
#' @export
as_period.factor <- function(x, interval = 1, origin = NULL, format = NULL, locale = clock_locale(), ...) {

  check_dots_empty()
  if (!is.null(origin)) origin <- as.character(origin)

  as_period.character(
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

#' Print a grate_period object
#'
#' @param x A grate_period object.
#' @param format The format to use for the bounds of each grate_period entry.
#' @param sep Where more than one month is grouped with others, `sep` is placed
#'   between the upper and lower bounds when printing.
#' @param ... Not currently used.
#'
#' @export
print.grate_period <- function(x, format = "%Y-%m-%d", sep = "to", ...) {
  interval <- attr(x, "interval")
  cat(sprintf("<grate_period: interval = %d days>\n", interval))
  print(format.grate_period(x, format = format, sep = sep))
  invisible(x)
}


#' @rdname print.grate_period
#' @export
format.grate_period <- function(x, format = "%Y-%m-%d", sep = "to", ...) {
  if (length(x) == 0) return(character(0))
  out <- sprintf(
    "%s %s %s",
    format.Date(as.Date(x), format = format),
    sep,
    format.Date(as.Date(x + 1) - 1, format = format)
  )
  out[is.na(x)] <- NA_character_
  out
}


#' Print a grate_int_period object
#'
#' @param x A grate_int_period object.
#' @param ... Not currently used.
#'
#' @export
print.grate_int_period <- function(x, ...) {
  interval <- attr(x, "interval")
  cat(sprintf("<grate_int_period: interval = %d>\n", interval))
  print(format.grate_int_period(x))
  invisible(x)
}


#' @rdname print.grate_int_period
#' @export
format.grate_int_period <- function(x, ...) {
  if (length(x) == 0) return(character(0))
  out <- sprintf("%d - %d", as.integer(x), as.integer(x + 1) - 1)
  out[is.na(x)] <- NA_character_
  out
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ----------------- METHODS: CONVERSIONS FROM grate_period----------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
as.Date.grate_int_period <- function(x, ...) {
  check_dots_empty()
  abort("Cannot convert object of class <grate_int_period> to Date")
}


#' @export
as.Date.grate_period <- function(x, ...) {
  check_dots_empty()
  attributes(x) <- NULL
  new_date(x)
}


#' @export
as.POSIXct.grate_int_period <- function(x, ...) {
  check_dots_empty()
  abort("Cannot convert object of class <grate_int_period> to POSIXct")
}


#' @export
as.POSIXct.grate_period <- function(x, tz = "UTC", ...) {
  check_dots_empty()
  out <- as.Date(x)
  out <- as_zoned_time(out, zone = tz, nonexistent = "roll-forward", ambiguous = "latest")
  as_date_time(out)
}


as.POSIXlt.grate_int_period <- function(x, ...) {
  check_dots_empty()
  abort("Cannot convert object of class <grate_int_period> to POSIXlt")
}


#' @export
as.POSIXlt.grate_period <- function(x, tz = "UTC", ...) {
  check_dots_empty()
  out <- as.Date(x)
  out <- as_zoned_time(out, zone = tz, nonexistent = "roll-forward", ambiguous = "latest")
  as.POSIXlt(out)
}


#' @export
as.character.grate_period <- function(x, ...) format(x, ...)


#' @export
as.list.grate_int_period <- function(x, ...) {
  check_dots_empty()
  interval <- attr(x, "interval")
  out <- lapply(unclass(x), new_grate_int_period, interval = interval)
}


#' @export
as.list.grate_period <- function(x, ...) {
  interval <- attr(x, "interval")
  out <- lapply(unclass(x), new_grate_period, interval = interval)
}

#' @export
as.numeric.grate_period <- function(x, ...) {
  check_dots_empty()
  attributes(x) <- NULL
  x
}

#' @export
as.data.frame.grate_period <- as.data.frame.vector


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------ METHODS: MISCELLANEOUS ------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
is.numeric.grate_period <- function(x) FALSE


#' @export
`[.grate_period` <- function (x, ..., drop = TRUE) {
  cl <- oldClass(x)
  interval <- attr(x, "interval")
  val <- NextMethod()
  class(val) <- cl
  attr(val, "interval") <- interval
  val
}


#' @export
`[[.grate_period` <- function (x, ..., drop = TRUE) {
  cl <- oldClass(x)
  interval <- attr(x, "interval")
  val <- NextMethod()
  class(val) <- cl
  attr(val, "interval") <- interval
  val
}


#' @export
`[<-.grate_int_period` <- function(x, i, value) {
  cl <- oldClass(x)
  interval <- attr(x, "interval")
  if (!all(inherits(value, "grate_int_period") | is.na(value))) {
    abort("Can only assign grate_int_period objects in to a grate_int_period object")
  }
  if (!identical(interval, attr(value, "interval")) && !is.na(value)) {
    abort("Can only combine <grate_int_period> objects with identical values of `interval`")
  }
  val <- NextMethod("[<-")
  class(val) <- cl
  attr(val, "interval") <- interval

  # TODO - make a more general validation function
  if (!is.na(value)) {
    dat <- diff(as.numeric(val))
    dat <- dat[!is.na(dat)]
    if (length(unique(dat)) > 1L) {
      abort(c(
        "Incompatible <grate_int_period> objects.",
        i = "Are they anchored to incompatible origins?"
      ))
    }
  }

  val
}



#' @export
`[<-.grate_period` <- function(x, i, value) {
  cl <- oldClass(x)
  interval <- attr(x, "interval")
  if (!all(inherits(value, "grate_period") | is.na(value))) {
    abort("Can only assign grate_period objects in to a grate_period object")
  }
  if (!identical(interval, attr(value, "interval")) && !is.na(value)) {
    abort("Can only combine <grate_period> objects with identical values of `interval`")
  }
  val <- NextMethod("[<-")
  class(val) <- cl
  attr(val, "interval") <- interval

  # TODO - make a more general validation function
  if (!is.na(value)) {
    dat <- diff(as.numeric(val))
    dat <- dat[!is.na(dat)]
    if (length(unique(dat)) > 1L) {
      abort(c(
        "Incompatible <grate_period> objects.",
        i = "Are they anchored to incompatible origins?"
      ))
    }
  }

  val
}


#' @export
rep.grate_period <- function (x, ..., drop = TRUE) {
  cl <- oldClass(x)
  interval <- attr(x, "interval")
  val <- NextMethod()
  class(val) <- cl
  attr(val, "interval") <- interval
  val
}


#' @export
unique.grate_period <- function (x, incomparables = FALSE, ...) {
  cl <- oldClass(x)
  interval <- attr(x, "interval")
  class(x) <- NULL
  val <- NextMethod()
  class(val) <- cl
  attr(val, "interval") <- interval
  val
}


#' @export
c.grate_int_period <- function(..., recursive = FALSE, use.names = TRUE) {

  dots <- list(...)
  interval <- attr(dots[[1]], "interval")

  is_mon <- vapply(dots, inherits, logical(1), what = "grate_int_period")
  is_na <- is.na(dots)
  if (!all(is_mon | is_na)) {
    abort(c(
      "Unable to combine with <grate_int_period> object with other classes.",
      i = "Covert to a common class prior to combining"
    ))
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
    abort("Can only combine <grate_int_period> objects with identical values of `interval`")
  }

  res <- new_grate_int_period(
    unlist(dots, recursive = recursive, use.names = use.names),
    interval = interval
  )

  tmp <- res[!is.na(res)]
  if (length(tmp)) {
    tmp <- diff(as.numeric(tmp))
    tmp <- tmp[!is.na(tmp)]
    if (length(unique(tmp)) > 1L) {
      abort(c(
        "Incompatible <grate_int_period> objects.",
        i = "Are they anchored to incompatible origins?"
      ))
    }
  }

  res
}


#' @export
c.grate_period <- function(..., recursive = FALSE, use.names = TRUE) {

  dots <- list(...)
  interval <- attr(dots[[1]], "interval")

  is_mon <- vapply(dots, inherits, logical(1), what = "grate_period")
  is_na <- is.na(dots)
  if (!all(is_mon | is_na)) {
    abort(c(
      "Unable to combine with <grate_period> object with other classes.",
      i = "Covert to a common class prior to combining"
    ))
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
    abort("Can only combine <grate_period> objects with identical values of `interval`")
  }

  res <- new_grate_period(
    unlist(dots, recursive = recursive, use.names = use.names),
    interval = interval
  )

  tmp <- res[!is.na(res)]
  if (length(tmp)) {
    tmp <- diff(as.numeric(tmp))
    tmp <- tmp[!is.na(tmp)]
    if (length(unique(tmp)) > 1L) {
      abort(c(
        "Incompatible <grate_period> objects.",
        i = "Are they anchored to incompatible origins?"
      ))
    }
  }

  res
}


#' @export
seq.grate_int_period <- function(from, to, by = 1L, ...) {
  by <- vec_cast(by, integer(), x_arg = "by")

  if (!inherits(to, "grate_int_period")) {
    abort("Can only create a sequence between two `grate_int_period` objects")
  }

  c(from, to) # this will trigger warning if incompatible

  interval <- attr(from, "interval")
  from <- as.numeric(from)
  to = as.numeric(to)
  out <- seq(from = from, to = to, by = (interval * by))
  new_grate_int_period(out, interval = interval)
}


#' @export
seq.grate_period <- function(from, to, by = 1L, ...) {
  by <- vec_cast(by, integer(), x_arg = "by")

  if (!inherits(to, "grate_period")) {
    abort("Can only create a sequence between two `grate_period` objects")
  }

  # TODO - add validation check
  c(from, to) # this will trigger warning if incompatible

  interval <- attr(from, "interval")
  from <- as.numeric(from)
  to = as.numeric(to)
  out <- seq(from = from, to = to, by = (interval * by))
  new_grate_period(out, interval = interval)
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# --------------------------------- MATHS --------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

is.na.grate_period <- function(x, ...) is.na(unclass(x))

is.nan.grate_period <- function(x, ...) vector("logical", length(x))

is.finite.grate_period <- function(x, ...) !is.na(unclass(x))

is.infinite.grate_period <- function(x, ...) vector("logical", length(x))

#' @export
Math.grate_period <- function(x, ...) {
  .fn <- .Generic
  fn <- switch(
    .fn,
    is.na = is.na.grate_period(x),
    is.nan = is.nan.grate_period(x),
    is.finite = is.finite.grate_period(x),
    is.infinite = is.infinite.grate_period(x),
    abort(sprintf("`%s()` is not supported for <grate_period> objects", .fn))
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
Summary.grate_period <- function (..., na.rm)
{
  ok <- switch(.Generic, max = TRUE, min = TRUE, range = TRUE, FALSE)
  if (!ok) abort(.Generic, " not defined for <grate_period> objects")
  dots <- list(...)
  if (.Generic == "range") {
    val <- c(do.call(min, dots), do.call(max, dots))
  } else {
    val <- NextMethod(.Generic)
    class(val) <- oldClass(dots[[1]])
    attr(val, "interval") <- attr(dots[[1]], "interval")
  }
  val
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ---------------------------------- OPS ---------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
Ops.grate_int_period <- function(e1, e2) {
  op <- .Generic
  if (op %in% c("==", "!=", "<", ">", "<=", ">=")) {
    if (inherits(e2, "grate_int_period")) {
      return(NextMethod())
    } else {
      abort("Can only compare <grate_int_period> objects with <grate_int_period> objects")
    }
  }

  switch(
    op,
    "+" = {
      if (missing(e2)) {
        return(e1)
      } else if (inherits(e1, "grate_int_period") && inherits(e2, "grate_int_period")) {
        abort("Cannot add <grate_month> objects to each other")
      } else if (inherits(e1, "grate_int_period") && (all(is.wholenumber(unclass(e2)), na.rm = TRUE))) {
        interval <- attr(e1, "interval")
        new_grate_int_period(unclass(e1) + interval*e2, interval = interval)
      } else if (inherits(e2, "grate_int_period") && (all(is.wholenumber(unclass(e1)),  na.rm = TRUE))) {
        interval <- attr(e2, "interval")
        new_grate_int_period(unclass(e2) + interval*e1, interval = interval)
      } else {
        abort("Can only add whole numbers to <grate_int_period> objects")      }
    },
    "-" = {
      if (missing(e2)) {
        abort("Cannot negate a <grate_int_period> object")
      } else if (inherits(e2, "grate_int_period")) {
        if (inherits(e1, "grate_int_period")) {
          abort("Can only subtract whole numbers from <grate_int_period> objects")
        } else if (all(is.wholenumber(unclass(e1)),  na.rm = TRUE)) {
          abort("Can only subtract from a <grate_int_period> object not vice-versa")
        }
      } else if (inherits(e1, "grate_int_period") && (all(is.wholenumber(unclass(e2)), na.rm = TRUE))) {
        interval <- attr(e1, "interval")
        new_grate_int_period(unclass(e1) - interval*as.numeric(e2), interval = interval)
      } else {
        abort("Can only subtract whole numbers from <grate_int_period> objects")
      }
    },
    abort(sprintf("%s is not compatible with <grate_int_period> objects", op))
  )
}


#' @export
Ops.grate_period <- function(e1, e2) {
  op <- .Generic
  if (op %in% c("==", "!=", "<", ">", "<=", ">=")) {
    if (inherits(e2, "grate_period")) {
      return(NextMethod())
    } else {
      abort("Can only compare <grate_period> objects with <grate_period> objects")
    }
  }

  switch(
    op,
    "+" = {
      if (missing(e2)) {
        return(e1)
      } else if (inherits(e1, "grate_period") && inherits(e2, "grate_period")) {
        abort("Cannot add <grate_period> objects to each other")
      } else if (inherits(e1, "grate_period") && (all(is.wholenumber(unclass(e2)), na.rm = TRUE))) {
        interval <- attr(e1, "interval")
        new_grate_period(unclass(e1) + interval*e2, interval = interval)
      } else if (inherits(e2, "grate_period") && (all(is.wholenumber(unclass(e1)),  na.rm = TRUE))) {
        interval <- attr(e2, "interval")
        new_grate_period(unclass(e2) + interval*e1, interval = interval)
      } else {
        abort("Can only add whole numbers to <grate_period> objects")
      }
    },
    "-" = {
      if (missing(e2)) {
        abort("Cannot negate a <grate_period> object")
      } else if (inherits(e2, "grate_period")) {
        if (inherits(e1, "grate_period")) {
          abort("Can only subtract whole numbers from <grate_period> objects")
        } else if (all(is.wholenumber(unclass(e1)),  na.rm = TRUE)) {
          abort("Can only subtract from a <grate_period> object not vice-versa")
        }
      } else if (inherits(e1, "grate_period") && (all(is.wholenumber(unclass(e2)), na.rm = TRUE))) {
        interval <- attr(e1, "interval")
        new_grate_period(unclass(e1) - interval*as.numeric(e2), interval = interval)
      } else {
        abort("Can only subtract whole numbers from <grate_period> objects")
      }
    },
    abort(sprintf("%s is not compatible with <grate_period> objects", op))
  )
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------- INTERNALS ------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
new_grate_period <- function(x, interval = integer()) {
  structure(x, interval = interval, class = "grate_period")
}


new_grate_int_period <- function(x, interval = integer()) {
  out <- new_grate_period(x, interval)
  class(out) <- c("grate_int_period", class(out))
  out
}
