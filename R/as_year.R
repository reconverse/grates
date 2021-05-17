# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# --------------------------------- AS_YEAR --------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' Convert an object to a year
#'
#' @description
#' - Date, POSIXct, and POSIXlt are converted directly.  Any day, hour, minute,
#'   or second components are dropped. POSIXct and POSIXlt are converted to
#'   dates via `as.date()` with the timezone respected.
#'
#' - Character input is assumed to be provided in either ISO 8601 standard
#'   format, i.e. "yyyy-mm-dd".
#'
#' @param x `An object to coerce to year.
#' @inheritParams clock::date_parse
#' @param ... Not currently used.
#'
#' @return A `year` object.
#'
#' @examples
#' as_year(Sys.Date())
#' as_year(as.POSIXct("2019-03-04 01:01:01", tz = "America/New_York"))
#' as_year("2019-05-03")
#'
#' @export
as_year <- function(x, ...) {
  UseMethod("as_year")
}


#' @rdname as_year
#' @export
as_year.default <- function(x, ...) {
  abort(sprintf("Can't convert a <%s> to a <grate_year>" , class(x)[1]))
}


#' @rdname as_year
#' @export
as_year.grate_year <- function(x, ...) {
  check_dots_empty()
  x
}

#' @rdname as_year
#' @export
as_year.grate_month <- function(x, ...) {
  check_dots_empty()
  interval <- attr(x, "interval")
  if (interval > 1) {
    abort("Can only convert <grate_month> objects to <year> if interval = 1")
  }
  as_year(as.Date(x))
}

#' @rdname as_year
#' @export
as_year.grate_quarter <- function(x, ...) {#
  check_dots_empty()
  as_year(as.Date(x))
}


#' @rdname as_year
#' @export
as_year.Date <- function(x, ...) {

  check_dots_empty()

  # convert to posixlt and floor date to start of quarter
  year <- as_utc_posixlt_from_int(x)$year + 1900L
  year <- new_grate_year(as.numeric(year))

  # finishing touches
  year[is.na(x)] <- NA_real_
  setNames(year, names(x))
}


#' @rdname as_year
#' @export
as_year.POSIXt <- function(x, ...) {

  check_dots_empty()

  x <- as.POSIXlt(x)
  year <- x$year + 1900L
  year <- new_grate_year(as.numeric(year))

  # finishing touches
  year[is.na(x)] <- NA_real_
  names(year) <- names(x)
  year
}



#' @rdname as_year
#' @export
as_year.character <- function(x, format = NULL, locale = clock_locale(),...) {
  check_dots_empty()
  x <- date_parse(x, format = format, locale = locale)
  if (all(is.na(x))) abort("Unable to parse any entries of x as Dates")
  as_year.Date(x)
}

#' @inheritParams clock::date_parse
#' @rdname as_year
#' @export
as_year.factor <- function(x, format = NULL, locale = clock_locale(),...) {
  check_dots_empty()
  as_year.character(as.character(x), format = NULL, locale = clock_locale())
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------- FORMATING / PRINTING -------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
format.grate_year <- function(x, ...) {
  if (length(x) == 0) return(character(0))
  format(unclass(x))
}

#' @export
print.grate_year <- function(x, ...) {
  print(format.grate_year(x))
  invisible(x)
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------- METHODS: CONVERSIONS FROM year -------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
as.POSIXct.grate_year <- function(x, tz = "UTC", ...) {
  date_time_build(
    year = unclass(x),
    zone = tz,
    invalid = "error",
    nonexistent = "error",
    ambiguous = "error"
  )
}


#' @export
as.POSIXlt.grate_year <- function(x, tz = "UTC", ...) {
  tmp <- date_time_build(
    year = unclass(x),
    zone = tz,
    invalid = "error",
    nonexistent = "error",
    ambiguous = "error"
  )
  as.POSIXlt(tmp)
}


#' @export
as.Date.grate_year <- function(x, ...) {
  date_build(year = unclass(x), invalid = "error" )
}

#' @export
as.character.grate_year <- function(x, ...) format(x, ...)

#' @export
as.list.grate_year <- function(x, ...) lapply(unclass(x), new_grate_year)

#' @export
as.numeric.grate_year <- function(x, ...) unclass(x)

#' @export
as.data.frame.grate_year <- as.data.frame.vector


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------ METHODS: MISCELLANEOUS ------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
is.numeric.grate_year <- function(x) TRUE


#' @export
`[.grate_year` <- function (x, ..., drop = TRUE) {
  cl <- oldClass(x)
  class(x) <- NULL
  val <- NextMethod()
  class(val) <- cl
  val
}


#' @export
`[[.grate_year` <- function (x, ..., drop = TRUE) {
  cl <- oldClass(x)
  class(x) <- NULL
  val <- NextMethod()
  class(val) <- cl
  val
}


#' @export
`[<-.grate_year` <- function(x, i, value) {
  cl <- oldClass(x)
  if (!all(inherits(value, "grate_year") | is.na(value))) {
    abort("Can only assign year objects in to a year object")
  }
  val <- NextMethod("[<-")
  class(val) <- cl
  val
}


#' @export
rep.grate_year <- function (x, ..., drop = TRUE) {
  cl <- oldClass(x)
  class(x) <- NULL
  val <- NextMethod()
  class(val) <- cl
  val
}


#' @export
unique.grate_year <- function (x, incomparables = FALSE, ...) {
  cl <- oldClass(x)
  class(x) <- NULL
  val <- NextMethod()
  class(val) <- cl
  val
}

#' @export
c.grate_year <- function(..., recursive = FALSE, use.names = TRUE) {
  dots <- list(...)
  is_mon <- vapply(
    dots,
    function(x) {
      inherits(x, "grate_month") && (attr(x, "interval") == 1)
    },
    logical(1)
  )
  is_year <- vapply(dots, inherits, logical(1), what = "grate_year")
  is_qtr <- vapply(dots, inherits, logical(1), what = "grate_quarter")
  is_na <- is.na(dots)

  if (!all(is_mon | is_na | is_year | is_qtr)) {
    abort(c(
      "Unable to combine with <grate_year> object with other classes.",
      i = "Covert to a common class prior to combining"
    ))
  }
  dots[is_mon] <- lapply(dots[is_mon], as_year)
  dots[is_qtr] <- lapply(dots[is_qtr], as_year)
  res <- unlist(dots)
  class(res) <- "grate_year"
  res
}


#' @export
seq.grate_year <- function(from, to, by = 1L, ...) {
  by <- vec_cast(by, integer(), x_arg = "by")

  if (!(inherits(to, "grate_year") || inherits(to, "integer"))) {
    abort("Can only create a sequence between two `grate_year` objects or an integer")
  }

  end <- to - from
  idx <- seq.int(from = 0, to = end, by = by)
  from + idx
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# --------------------------------- MATHS --------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
Math.grate_year <- function(x, ...) {
  .fn <- .Generic
  fn <- switch(
    .fn,
    is.na = is.na.grate_year(x),
    is.nan = is.nan.grate_year(x),
    is.finite = is.finite.grate_year(x),
    is.infinite = is.infinite.grate_year(x),
    abort(sprintf("`%s()` is not supported for <grate_year>", .fn))
  )
}

is.na.grate_year <- function(x, ...) is.na(unclass(x))

is.nan.grate_year <- function(x, ...) vector("logical", length(x))

is.finite.grate_year <- function(x, ...) !is.na(unclass(x))

is.infinite.grate_year <- function(x, ...) vector("logical", length(x))


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ---------------------------------- OPS ---------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
Ops.grate_year <- function(e1, e2) {
  op <- .Generic
  if (op %in% c("==", "!=", "<", ">", "<=", ">=")) {
    if (inherits(e2, "grate_year") || inherits(e2, "integer")) {
      return(NextMethod())
    } else {
      abort("Can only compare <grate_year> objects with <grate_year> objects or integers")
    }
  }

  switch(
    op,
    "+" = {
      if (missing(e2)) {
        return(e1)
      } else if (inherits(e1, "grate_year") && inherits(e2, "grate_year")) {
        abort("Cannot add <grate_year> objects to each other")
      } else if (inherits(e1, "grate_year") && (all(is.wholenumber(unclass(e2)), na.rm = TRUE))) {
        new_grate_year(unclass(e1) + e2)
      } else if (inherits(e2, "grate_year") && (all(is.wholenumber(unclass(e1)),  na.rm = TRUE))) {
        new_grate_year(e1 + unclass(e2))
      } else {
        abort("Can only add whole numbers to <grate_year> objects")
      }
    },
    "-" = {
      if (missing(e2)) {
        abort("Cannot negate a <grate_year> object")
      } else if (inherits(e2, "grate_year")) {
        if (inherits(e1, "grate_year")) {
          unclass(e1) - unclass(e2)
        } else if (all(is.wholenumber(unclass(e1)),  na.rm = TRUE)) {
          abort("Can only subtract from a <grate_year> object not vice-versa")
        }
      } else if (inherits(e1, "grate_year") && (all(is.wholenumber(unclass(e2)), na.rm = TRUE))) {
        new_grate_year(unclass(e1) - e2)
      } else {
        abort("Can only subtract whole numbers and other <grate_year> objects from <grate_year> objects")
      }
    },
    abort(sprintf("%s is not compatible with <grate_year> objects", op))
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
Summary.grate_year <- function (..., na.rm)
{
  ok <- switch(.Generic, max = TRUE, min = TRUE, range = TRUE, FALSE)
  if (!ok) abort(.Generic, " not defined for <grate_year> objects")
  val <- NextMethod(.Generic)
  class(val) <- oldClass(list(...)[[1]])
  val
}

# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------- INTERNALS ------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
new_grate_year <- function(x = numeric()) {
  structure(x, class = c("grate_year"))
}
