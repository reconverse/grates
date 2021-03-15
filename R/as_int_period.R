# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ----------------------------- AS_INT_PERIOD ----------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' Convert an object to an integer period
#'
#' @param x An object to coerce to int_period.
#' @param firstdate The date to start the intervals from.  If NULL (default) the
#'   earliest date in the vector will be used.
#' @param interval How many days to include in each period.
#' @param ... Not used.
#'
#' @return An `int_period` object.
#'
#' @export
as_int_period <- function(x, interval, firstdate, ...) {
  UseMethod("as_int_period")
}


#' @rdname as_int_period
#' @export
as_int_period.default <- function(x, interval, firstdate, ...) {
  stop(
    sprintf("Can't convert a <%s> to an <int_period>", class(x)[1]),
    call. = FALSE
  )
}


#' @rdname as_int_period
#' @export
as_int_period.int_period <- function(x, interval = 1L, firstdate = NULL, ...) {
  x
}


#' @rdname as_int_period
#' @export
as_int_period.integer <- function(x, interval = 1L, firstdate = NULL, ...) {

  # ensure we have a firstdate value
  if (is.null(firstdate)) {
    firstdate <- min(x, na.rm = TRUE)
  } else {
    firstdate <- int_cast(firstdate)
  }

  # Ensure interval is of length one
  if (length(interval) != 1L) {
    stop(sprintf(
      "Exactly one value should be provided for `interval` (%d provided)",
      length(interval),
      call. = FALSE
    ))
  }

  # Ensure numeric intervals are whole numbers
  if (is.numeric(interval)) {
    interval <- int_cast(interval)
    if (interval < 1L) stop("interval must be positive (>= 1)", call. = FALSE)
  } else if (!is.integer(interval)) {
    stop("For integer dates the interval must also be integer.", call. = FALSE)
  }

  # No need to change anything if the interval is 1
  if (interval == 1L || interval == 1) {
    x <- x[x >= firstdate]
    return(x)
  }

  period <- break_ints(x, interval, firstdate)


  # create class
  period <- new_int_period(
    period,
    interval = interval
  )

  # finishing touches
  period[is.na(x)] <- NA_integer_
  names(period) <- names(x)
  period
}


#' @rdname as_int_period
#' @export
as_int_period.numeric <- function(x, interval = 1L, firstdate = NULL, ...) {
  x <- int_cast(x)
  as_int_period.integer(x, interval = interval, firstdate = firstdate, ...)
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------- FORMATING / PRINTING -------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
format.int_period <- function(x, ...) {
  if (length(x) == 0) return(character(0))
  sprintf("%d - %d", as.integer(x), as.integer(x + 1) - 1)
}

#' @export
print.int_period <- function(x, ...) {
  interval <- attr(x, "interval")
  cat(sprintf("<int_period> interval = %d\n", interval))
  print(format.int_period(x, ...))
  invisible(x)
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------- METHODS: CONVERSIONS FROM period -------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
as.character.int_period <- function(x, ...) format(x, ...)


#' @export
as.list.int_period <- function(x, ...) {
  dat <- unclass(x)
  dur <- attr(x, "interval")
  lapply(dat, new_int_period, interval = dur)
}

#' @export
as.numeric.int_period <- function(x, ...) {
  attributes(x) <- NULL
  as.numeric(x)
}

#' @export
as.integer.int_period <- function(x, ...) {
  attributes(x) <- NULL
  as.integer(x)
}


# This code is the same as that of the as.data.frame.yearmon code in Zoo by
# Achim Zeileis et al.
#' @export
as.data.frame.int_period <- function(x, row.names = NULL, optional = FALSE, ...) {
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
is.numeric.int_period <- function(x) TRUE

#' @export
`[.int_period` <- function (x, ..., drop = TRUE) {
  cl <- oldClass(x)
  interval <- attr(x, "interval")
  class(x) <- NULL
  val <- NextMethod()
  class(val) <- cl
  attr(val, "interval") <- interval
  val
}


#' @export
`[[.int_period` <- function (x, ..., drop = TRUE) {
  cl <- oldClass(x)
  interval <- attr(x, "interval")
  class(x) <- NULL
  val <- NextMethod()
  class(val) <- cl
  attr(val, "interval") <- interval
  val
}


#' @export
`[<-.int_period` <- function(x, i, value) {
  cl <- oldClass(x)
  interval <- attr(x, "interval")
  if (!all(inherits(value, "int_period") | is.na(value))) {
    stop("Can only assign int_period objects in to an int_period object", call. = FALSE)
  }
  val <- NextMethod("[<-")
  class(val) <- cl
  attr(val, "interval") <- interval
  val
}


#' @export
rep.int_period <- function (x, ..., drop = TRUE) {
  cl <- oldClass(x)
  interval <- attr(x, "interval")
  class(x) <- NULL
  val <- NextMethod()
  class(val) <- cl
  attr(val, "interval") <- interval
  val
}


#' @export
unique.int_period <- function (x, incomparables = FALSE, ...) {
  cl <- oldClass(x)
  interval <- attr(x, "interval")
  class(x) <- NULL
  val <- NextMethod()
  class(val) <- cl
  attr(val, "interval") <- interval
  val
}


#' @export
c.int_period <- function(..., recursive = FALSE, use.names = TRUE) {
  dots <- list(...)

  if (!all(vapply(dots, inherits, logical(1), what = "int_period") | is.na(dots))) {
    stop(
      "To combine <int_period> objects with different objects first convert to a common class",
      call. = FALSE
    )
  }

  interval <- attr(dots[[1]], "interval")
  intervals <- lapply(dots, attr, numeric(1), which = "interval")
  if (!all(vapply(intervals, function(x) {is.null(x) || x == interval}, logical(1)))) {
    stop(
      "Unable to combine <int_period> objects with different `interval` attributes",
      call. = FALSE
    )
  }

  dots_dates <- unlist(dots)
  dots_min <- min(dots_dates)
  dots_max <- max(dots_dates)
  range <- seq(from = dots_min, to = dots_max, by = 1)
  range <- as_int_period(range, interval = interval)

  if (!all(vapply(dots, function(x) all(x %in% range), logical(1)))){
    stop("Incompatible <int_period> objects.", call. = FALSE)
  }


  res <- NextMethod()
  class(res) <- c("int_period", "grate")
  attr(res, "interval") <- interval
  res
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# --------------------------------- MATHS --------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
Math.int_period <- function(x, ...) {
  .fn <- .Generic
  fn <- switch(
    .fn,
    is.nan = is.nan.int_period(x),
    is.finite = is.finite.int_period(x),
    is.infinite = is.infinite.int_period(x),
    stop(sprintf("`%s()` is not supported for <int_period>", .fn), call. = FALSE)
  )
}

is.nan.int_period <- function(x, ...) vector("logical", length(x))

is.finite.int_period <- function(x, ...) !is.na(unclass(x))

is.infinite.int_period <- function(x, ...) vector("logical", length(x))


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ---------------------------------- OPS ---------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
Ops.int_period <- function(e1, e2) {
  op <- .Generic
  if (op %in% c("==", "!=", "<", ">", "<=", ">=")) {
    if (inherits(e2, "int_period")) {
      return(NextMethod())
    } else {
      stop("Can only compare <int_period> objects with <int_period> objects", call. = FALSE)
    }
  }

  switch(
    op,
    "+" = {
      if (missing(e2)) {
        return(e1)
      } else if (inherits(e1, "int_period") && inherits(e2, "int_period")) {
        stop("Cannot add <int_period> objects to each other", call. = FALSE)
      } else if (inherits(e1, "int_period") && (all(is.wholenumber(unclass(e2)), na.rm = TRUE))) {
        add_int_periods(e1, unclass(e2))
      } else if (inherits(e2, "int_period") && (all(is.wholenumber(unclass(e1)),  na.rm = TRUE))) {
        add_int_periods(e2, unclass(e1))
      } else {
        stop("Can only add whole numbers to <int_period> objects", call. = FALSE)
      }
    },
    "-" = {
      if (missing(e2)) {
        stop("Cannot negate a <int_period> object", call. = FALSE)
      } else if (inherits(e1, "int_period") && !(inherits(e2, "int_period")) && (all(is.wholenumber(unclass(e2)), na.rm = TRUE))) {
        add_int_periods(e1, -unclass(e2))
      } else {
        stop("Can only subtract whole numbers from <int_period> objects", call. = FALSE)
      }
    },
    stop(sprintf("%s is not compatible with <int_period> objects", op), call. = FALSE)
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
Summary.int_period <- function (..., na.rm)
{
  ok <- switch(.Generic, max = TRUE, min = TRUE, range = TRUE, FALSE)
  if (!ok) stop(.Generic, " not defined for <int_period> objects")
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

new_int_period <- function(x = integer(), interval = integer()) {
  structure(
    x,
    interval = interval,
    class = c("int_period", "grate")
  )
}

break_ints <- function(x, interval, firstdate) {
  breaks <- seq(from = firstdate, to = max(x, na.rm = TRUE), by = interval)
  period <- cut(x, breaks = c(breaks, Inf), labels = FALSE, right = FALSE)
  period <- breaks[period]
}


add_int_periods <- function(x, n) {
  d <- attr(x, "interval")
  out <- as.integer(x) + (n * d)
  new_int_period(out, interval = d)
}
