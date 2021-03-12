# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# --------------------------------- AS_YR --------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' Convert an object to a yr
#'
#' @description
#' - Date, POSIXct, and POSIXlt are converted directly.  Any day, hour, minute,
#'   or second components are dropped. POSIXct and POSIXlt are converted to
#'   dates via `as.date()` with the timezone respected.
#'
#' - Character input is assumed to be provided in either ISO 8601 standard
#'   format, i.e. "yyyy-mm-dd".
#'
#' @param x `An object to coerce to yr.
#' @param ... Not used.
#'
#' @examples
#' as_yr(Sys.Date())
#' as_yr(as.POSIXct("2019-03-04 01:01:01", tz = "America/New_York"))
#' as_yr("2019-05-03")
#'
#' @export
as_yr <- function(x, ...) {
  UseMethod("as_yr")
}


#' @rdname as_yr
#' @export
as_yr.default <- function(x, ...) {
  stop(sprintf("Can't convert a <%s> to a <yr>" , class(x)[1]), call. = FALSE)
}


#' @rdname as_yr
#' @export
as_yr.yr <- function(x, ...) {
  x
}

#' @rdname as_yr
#' @export
as_yr.yrmon <- function(x, ...) {
  as_yr(as.Date(x))
}

#' @rdname as_yr
#' @export
as_yr.yrqtr <- function(x, ...) {
  as_yr(as.Date(x))
}


#' @rdname as_yr
#' @export
as_yr.Date <- function(x, ...) {

  # convert to posixlt and floor date to start of quarter
  yr <- as_utc_posixlt_from_int(x)$year + 1900L
  yr <- new_yr(as.numeric(yr))

  # finishing touches
  yr[is.na(x)] <- NA_real_
  names(yr) <- names(x)
  yr
}


#' @rdname as_yr
#' @export
as_yr.POSIXt <- function(x, ...) {

  x <- as.POSIXlt(x)
  yr <- x$year + 1900L
  yr <- new_yr(as.numeric(yr))

  # finishing touches
  yr[is.na(x)] <- NA_real_
  names(yr) <- names(x)
  yr
}



#' @rdname as_yr
#' @export
as_yr.character <- function(x, ...) {

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

  # convert to yr
  dat <- as_yr.Date(dat)
  names(dat) <- names(x)
  dat
}


#' @rdname as_yr
#' @export
as_yr.factor <- function(x, ...) {
  as_yr.character(as.character(x))
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------- FORMATING / PRINTING -------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
format.yr <- function(x, ...) {
  if (length(x) == 0) return(character(0))
  format(unclass(x))
}

#' @export
print.yr <- function(x, ...) {
  print(format.yr(x, ...))
  invisible(x)
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------- METHODS: CONVERSIONS FROM yr -------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
as.POSIXct.yr <- function(x, tz = "UTC", ...) {
  x <-  days_before_year(unclass(x)) - 719162L
  if (tz == "UTC") {
    as_utc_posixct_from_int(x)
  } else {
    as_zoned_posixct_from_int(x, tz = tz)
  }
}


#' @export
as.POSIXlt.yr <- function(x, tz = "UTC", ...) {
  x <-  days_before_year(unclass(x)) - 719162L
  if (tz == "UTC") {
    as_utc_posixlt_from_int(x)
  } else {
    as_zoned_posixlt_from_int(x, tz = tz)
  }

}


#' @export
as.Date.yr <- function(x, ...) {
  x <-  days_before_year(unclass(x)) - 719162L
  new_date(x)
}


#' @export
as.character.yr <- function(x, ...) format(x, ...)


#' @export
as.list.yr <- function(x, ...) lapply(unclass(x), new_yr)


#' @export
as.numeric.yr <- function(x, ...) {
  unclass(x)
}


# This code is the same as that of the as.data.frame.yearmon code in Zoo by
# Achim Zeileis et al.
#' @export
as.data.frame.yr <- function(x, row.names = NULL, optional = FALSE, ...) {
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
is.numeric.yr <- function(x) TRUE


#' @export
`[.yr` <- function (x, ..., drop = TRUE) {
  cl <- oldClass(x)
  class(x) <- NULL
  val <- NextMethod()
  class(val) <- cl
  val
}


#' @export
`[[.yr` <- function (x, ..., drop = TRUE) {
  cl <- oldClass(x)
  class(x) <- NULL
  val <- NextMethod()
  class(val) <- cl
  val
}


#' @export
`[<-.yr` <- function(x, i, value) {
  cl <- oldClass(x)
  if (!all(inherits(value, "yr") | is.na(value))) {
    stop("Can only assign yr objects in to a yr object", call. = FALSE)
  }
  val <- NextMethod("[<-")
  class(val) <- cl
  val
}


#' @export
rep.yr <- function (x, ..., drop = TRUE) {
  cl <- oldClass(x)
  class(x) <- NULL
  val <- NextMethod()
  class(val) <- cl
  val
}


#' @export
unique.yr <- function (x, incomparables = FALSE, ...) {
  cl <- oldClass(x)
  class(x) <- NULL
  val <- NextMethod()
  class(val) <- cl
  val
}

#' @export
c.yr <- function(..., recursive = FALSE, use.names = TRUE) {
  dots <- list(...)
  is_mon <- vapply(dots, inherits, logical(1), what = "yrmon")
  is_qtr <- vapply(dots, inherits, logical(1), what = "yrqtr")
  is_yr <- vapply(dots, inherits, logical(1), what = "yr")
  is_na <- is.na(dots)

  if (!all(is_mon | is_qtr | is_na | is_yr)) {
    stop(
      "To combine <yr> objects with different objects first convert to a common class",
      call. = FALSE
    )
  }
  dots[is_mon] <- lapply(dots[is_mon], as_yr)
  dots[is_qtr] <- lapply(dots[is_qtr], as_yr)
  res <- unlist(dots)
  class(res) <- c("yr", "grate")
  res
}


#' @export
seq.yr <- function(from, to, by = 1L, ...) {
  by <- int_cast(by)

  if (!(inherits(to, "yr") || inherits(to, "integer"))) {
    stop(
      "Can only create a sequence between two `yr` objects or an integer",
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
Math.yr <- function(x, ...) {
  .fn <- .Generic
  fn <- switch(
    .fn,
    is.nan = is.nan.yr(x),
    is.finite = is.finite.yr(x),
    is.infinite = is.infinite.yr(x),
    stop(sprintf("`%s()` is not supported for <yr>", .fn), call. = FALSE)
  )
}

is.nan.yr <- function(x, ...) vector("logical", length(x))

is.finite.yr <- function(x, ...) !is.na(unclass(x))

is.infinite.yr <- function(x, ...) vector("logical", length(x))


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ---------------------------------- OPS ---------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
Ops.yr <- function(e1, e2) {
  op <- .Generic
  if (op %in% c("==", "!=", "<", ">", "<=", ">=")) {
    if (inherits(e2, "yr") || inherits(e2, "integer")) {
      return(NextMethod())
    } else {
      stop(
        "Can only compare <yr> objects with <yr> objects or integers",
        call. = FALSE
      )
    }
  }

  switch(
    op,
    "+" = {
      if (missing(e2)) {
        return(e1)
      } else if (inherits(e1, "yr") && inherits(e2, "yr")) {
        stop("Cannot add <yr> objects to each other", call. = FALSE)
      } else if (inherits(e1, "yr") && (all(is.wholenumber(unclass(e2)), na.rm = TRUE))) {
        new_yr(unclass(e1) + e2)
      } else if (inherits(e2, "yr") && (all(is.wholenumber(unclass(e1)),  na.rm = TRUE))) {
        new_yr(e1 + unclass(e2))
      } else {
        stop("Can only add whole numbers to <yr> objects", call. = FALSE)
      }
    },
    "-" = {
      if (missing(e2)) {
        stop("Cannot negate a <yr> object", call. = FALSE)
      } else if (inherits(e2, "yr")) {
        if (inherits(e1, "yr")) {
          unclass(e1) - unclass(e2)
        } else if (all(is.wholenumber(unclass(e1)),  na.rm = TRUE)) {
          stop("Can only subtract from a <yr> object not vice-versa", call. = FALSE)
        }
      } else if (inherits(e1, "yr") && (all(is.wholenumber(unclass(e2)), na.rm = TRUE))) {
        new_yr(unclass(e1) - e2)
      } else {
        stop("Can only subtract whole numbers and other <yr> objects from <yr> objects", call. = FALSE)
      }
    },
    stop(sprintf("%s is not compatible with <yr> objects", op), call. = FALSE)
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
Summary.yr <- function (..., na.rm)
{
  ok <- switch(.Generic, max = TRUE, min = TRUE, range = TRUE, FALSE)
  if (!ok) stop(.Generic, " not defined for yr objects")
  val <- NextMethod(.Generic)
  class(val) <- oldClass(list(...)[[1]])
  val
}

# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------- INTERNALS ------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

new_yr <- function(x = numeric()) {
  structure(x, class = c("yr", "grate"))
}
