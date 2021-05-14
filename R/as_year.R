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
#' @param ... Not used.
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
  stop(sprintf("Can't convert a <%s> to a <grate_year>" , class(x)[1]), call. = FALSE)
}


#' @rdname as_year
#' @export
as_year.grate_year <- function(x, ...) {
  x
}

#' @rdname as_year
#' @export
as_year.grate_yearmon <- function(x, ...) {
  as_year(as.Date(x))
}

#' @rdname as_year
#' @export
as_year.grate_yearquarter <- function(x, ...) {
  as_year(as.Date(x))
}


#' @rdname as_year
#' @export
as_year.Date <- function(x, ...) {

  # convert to posixlt and floor date to start of quarter
  year <- as_utc_posixlt_from_int(x)$year + 1900L
  year <- new_grate_year(as.numeric(year))

  # finishing touches
  year[is.na(x)] <- NA_real_
  names(year) <- names(x)
  year
}


#' @rdname as_year
#' @export
as_year.POSIXt <- function(x, ...) {

  x <- as.POSIXlt(x)
  year <- x$year + 1900L
  year <- new_grate_year(as.numeric(year))

  # finishing touches
  year[is.na(x)] <- NA_real_
  names(year) <- names(x)
  year
}


#' @inheritParams clock::date_parse
#' @rdname as_year
#' @export
as_year.character <- function(x, format = NULL, locale = clock_locale(),...) {
  x <- date_parse(x, format = format, locale = locale)
  if (all(is.na(x))) stop("Unable to parse any entries of x as Dates")
  as_year.Date(x)
}

#' @inheritParams clock::date_parse
#' @rdname as_year
#' @export
as_year.factor <- function(x, format = NULL, locale = clock_locale(),...) {
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

#' @importFrom clock date_time_build
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

#' @importFrom clock date_time_build
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

#' @importFrom clock date_build
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
    stop("Can only assign year objects in to a year object", call. = FALSE)
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
  is_mon <- vapply(dots, inherits, logical(1), what = "grate_month")
  is_year <- vapply(dots, inherits, logical(1), what = "grate_year")
  is_na <- is.na(dots)

  if (!all(is_mon | is_na | is_year)) {
    stop(
      "To combine <grate_year> objects with different objects first convert to a common class",
      call. = FALSE
    )
  }
  dots[is_mon] <- lapply(dots[is_mon], as_year)
  res <- unlist(dots)
  class(res) <- "grate_year"
  res
}


#' @export
seq.grate_year <- function(from, to, by = 1L, ...) {
  by <- int_cast(by)

  if (!(inherits(to, "grate_year") || inherits(to, "integer"))) {
    stop(
      "Can only create a sequence between two `grate_year` objects or an integer",
      call. = FALSE
    )
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
    stop(sprintf("`%s()` is not supported for <grate_year>", .fn), call. = FALSE)
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
      stop(
        "Can only compare <grate_year> objects with <grate_year> objects or integers",
        call. = FALSE
      )
    }
  }

  switch(
    op,
    "+" = {
      if (missing(e2)) {
        return(e1)
      } else if (inherits(e1, "grate_year") && inherits(e2, "grate_year")) {
        stop("Cannot add <grate_year> objects to each other", call. = FALSE)
      } else if (inherits(e1, "grate_year") && (all(is.wholenumber(unclass(e2)), na.rm = TRUE))) {
        new_grate_year(unclass(e1) + e2)
      } else if (inherits(e2, "grate_year") && (all(is.wholenumber(unclass(e1)),  na.rm = TRUE))) {
        new_grate_year(e1 + unclass(e2))
      } else {
        stop("Can only add whole numbers to <grate_year> objects", call. = FALSE)
      }
    },
    "-" = {
      if (missing(e2)) {
        stop("Cannot negate a <grate_year> object", call. = FALSE)
      } else if (inherits(e2, "grate_year")) {
        if (inherits(e1, "grate_year")) {
          unclass(e1) - unclass(e2)
        } else if (all(is.wholenumber(unclass(e1)),  na.rm = TRUE)) {
          stop("Can only subtract from a <grate_year> object not vice-versa", call. = FALSE)
        }
      } else if (inherits(e1, "grate_year") && (all(is.wholenumber(unclass(e2)), na.rm = TRUE))) {
        new_grate_year(unclass(e1) - e2)
      } else {
        stop("Can only subtract whole numbers and other <grate_year> objects from <grate_year> objects", call. = FALSE)
      }
    },
    stop(sprintf("%s is not compatible with <grate_year> objects", op), call. = FALSE)
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
  if (!ok) stop(.Generic, " not defined for <grate_year> objects")
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
