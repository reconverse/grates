# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------------------- AS_YRQTR ------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' Convert an object to a yrqtr
#'
#' @description
#' - Date, POSIXct, and POSIXlt are converted directly.  Any day, hour, minute,
#'   or second components are dropped. POSIXct and POSIXlt are converted to
#'   dates via `as.date()` with the timezone respected.
#'
#' - Character input is assumed to be provided in either ISO 8601 standard
#'   format, i.e. "yyyy-mm-dd".
#'
#' @param x `An object to coerce to yrqtr.
#' @param ... Not used.
#'
#' @examples
#' as_yrqtr(Sys.Date())
#' as_yrqtr(as.POSIXct("2019-03-04 01:01:01", tz = "America/New_York"))
#' as_yrqtr("2019-05-03")
#'
#' @export
as_yrqtr <- function(x, ...) {
  UseMethod("as_yrqtr")
}


#' @rdname as_yrqtr
#' @export
as_yrqtr.default <- function(x, ...) {
  stop(sprintf("Can't convert a <%s> to a <yrqtr>" , class(x)[1]), call. = FALSE)
}


#' @rdname as_yrqtr
#' @export
as_yrqtr.yrqtr <- function(x, ...) {
  x
}

#' @rdname as_yrqtr
#' @export
as_yrqtr.yrmon <- function(x, ...) {
  out <- as.numeric(x) %/% 3
  out <- new_yrqtr(out)
  names(out) <- names(x)
  out
}


#' @rdname as_yrqtr
#' @export
as_yrqtr.Date <- function(x, ...) {
  out <- as_yrmon(x)
  out <- as.numeric(out) %/% 3
  out <- new_yrqtr(out)
  names(out) <- names(x)
  out
}


#' @rdname as_yrqtr
#' @export
as_yrqtr.POSIXt <- as_yrqtr.Date


#' @rdname as_yrqtr
#' @export
as_yrqtr.character <- as_yrqtr.Date


#' @rdname as_yrqtr
#' @export
as_yrqtr.factor <- as_yrqtr.Date


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------- FORMATING / PRINTING -------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
format.yrqtr <- function(x, ...) {
  if (length(x) == 0) return(character(0))
  days <- month_to_days(as.numeric(x) * 3)
  x <- as_utc_posixlt_from_int(days)
  out <- sprintf("%04d-Q%d", x$year + 1900L, x$mon %/% 3L +1)
  out[is.na(x)] <- NA_character_
  names(out) <- names(x)
  out
}

#' @export
print.yrqtr <- function(x, ...) {
  print(format.yrqtr(x, ...))
  invisible(x)
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------- METHODS: CONVERSIONS FROM YRQTR -------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
as.POSIXct.yrqtr <- function(x, tz = "UTC", ...) {
  attributes(x) <- NULL
  days <- month_to_days(as.numeric(x) * 3)
  if (tz == "UTC") {
    as_utc_posixct_from_int(days)
  } else {
    as_zoned_posixct_from_int(days, tz = tz)
  }
}


#' @export
as.POSIXlt.yrqtr <- function(x, tz = "UTC", ...) {
  attributes(x) <- NULL
  days <- month_to_days(as.numeric(x) * 3)
  if (tz == "UTC") {
    as_utc_posixlt_from_int(days)
  } else {
    as_zoned_posixlt_from_int(days, tz = tz)
  }

}


#' @export
as.Date.yrqtr <- function(x, ...) {
  attributes(x) <- NULL
  days <- month_to_days(as.numeric(x) * 3)
  new_date(days)
}


#' @export
as.character.yrqtr <- function(x, ...) format(x, ...)


#' @export
as.list.yrqtr <- function(x, ...) lapply(unclass(x), new_yrqtr)


#' @export
as.numeric.yrqtr <- function(x, ...) {
  attributes(x) <- NULL
  x
}


# This code is the same as that of the as.data.frame.yearmon code in Zoo by
# Achim Zeileis et al.
#' @export
as.data.frame.yrqtr <- function(x, row.names = NULL, optional = FALSE, ...) {
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
is.numeric.yrqtr <- function(x) FALSE


#' @export
`[.yrqtr` <- function (x, ..., drop = TRUE) {
  cl <- oldClass(x)
  class(x) <- NULL
  val <- NextMethod()
  class(val) <- cl
  val
}


#' @export
`[[.yrqtr` <- function (x, ..., drop = TRUE) {
  cl <- oldClass(x)
  class(x) <- NULL
  val <- NextMethod()
  class(val) <- cl
  val
}


#' @export
`[<-.yrqtr` <- function(x, i, value) {
  cl <- oldClass(x)
  if (!all(inherits(value, "yrqtr") | is.na(value))) {
    stop("Can only assign yrqtr objects in to a yrqtr object", call. = FALSE)
  }
  val <- NextMethod("[<-")
  class(val) <- cl
  val
}


#' @export
rep.yrqtr <- function (x, ..., drop = TRUE) {
  cl <- oldClass(x)
  class(x) <- NULL
  val <- NextMethod()
  class(val) <- cl
  val
}


#' @export
unique.yrqtr <- function (x, incomparables = FALSE, ...) {
  cl <- oldClass(x)
  class(x) <- NULL
  val <- NextMethod()
  class(val) <- cl
  val
}

#' @export
c.yrqtr <- function(..., recursive = FALSE, use.names = TRUE) {
  dots <- list(...)
  is_mon <- vapply(dots, inherits, logical(1), what = "yrmon")
  is_qtr <- vapply(dots, inherits, logical(1), what = "yrqtr")
  is_na <- is.na(dots)

  if (!all(is_mon | is_qtr | is_na)) {
    stop(
      "To combine <yrqtr> objects with different objects first convert to a common class",
      call. = FALSE
    )
  }
  dots[is_mon] <- lapply(dots[is_mon], as_yrqtr)
  res <- unlist(dots)
  class(res) <- c("yrqtr", "grate")
  res
}


#' @export
seq.yrqtr <- function(from, to, by = 1L, ...) {
  by <- int_cast(by)

  if (!inherits(to, "yrqtr")) {
    stop("Can only create a sequence between two `yrqtr` objects", call. = FALSE)
  }

  from <- as.numeric(from)
  to = as.numeric(to)
  out <- seq(from = from, to = to, by = by)
  new_yrqtr(out)
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# --------------------------------- MATHS --------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
Math.yrqtr <- function(x, ...) {
  .fn <- .Generic
  fn <- switch(
    .fn,
    is.nan = is.nan.yrqtr(x),
    is.finite = is.finite.yrqtr(x),
    is.infinite = is.infinite.yrqtr(x),
    stop(sprintf("`%s()` is not supported for <yrqtr>", .fn), call. = FALSE)
  )
}

is.nan.yrqtr <- function(x, ...) vector("logical", length(x))

is.finite.yrqtr <- function(x, ...) !is.na(unclass(x))

is.infinite.yrqtr <- function(x, ...) vector("logical", length(x))


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ---------------------------------- OPS ---------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
Ops.yrqtr <- function(e1, e2) {
  op <- .Generic
  if (op %in% c("==", "!=", "<", ">", "<=", ">=")) {
    if (inherits(e2, "yrqtr")) {
      return(NextMethod())
    } else {
      stop("Can only compare <yrqtr> objects with <yrqtr> objects", call. = FALSE)
    }
  }

  switch(
    op,
    "+" = {
      if (missing(e2)) {
        return(e1)
      } else if (inherits(e1, "yrqtr") && inherits(e2, "yrqtr")) {
        stop("Cannot add <yrqtr> objects to each other", call. = FALSE)
      } else if (inherits(e1, "yrqtr") && (all(is.wholenumber(unclass(e2)), na.rm = TRUE))) {
        new_yrqtr(unclass(e1) + e2)
      } else if (inherits(e2, "yrqtr") && (all(is.wholenumber(unclass(e1)),  na.rm = TRUE))) {
        new_yrqtr(unclass(e2) + e1)
      } else {
        stop("Can only add whole numbers to <yrqtr> objects", call. = FALSE)
      }
    },
    "-" = {
      if (missing(e2)) {
        stop("Cannot negate a <yrqtr> object", call. = FALSE)
      } else if (inherits(e2, "yrqtr")) {
        if (inherits(e1, "yrqtr")) {
          as.integer(e1) - as.integer(e2)
        } else if (all(is.wholenumber(unclass(e1)),  na.rm = TRUE)) {
          stop("Can only subtract from a <yrqtr> object not vice-versa", call. = FALSE)
        }
      } else if (inherits(e1, "yrqtr") && (all(is.wholenumber(unclass(e2)), na.rm = TRUE))) {
        new_yrqtr(unclass(e1) - as.numeric(e2))
      } else {
        stop("Can only subtract whole numbers and other <yrqtr> objects from <yrqtr> objects", call. = FALSE)
      }
    },
    stop(sprintf("%s is not compatible with <yrqtr> objects", op), call. = FALSE)
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
Summary.yrqtr <- function (..., na.rm)
{
  ok <- switch(.Generic, max = TRUE, min = TRUE, range = TRUE, FALSE)
  if (!ok) stop(.Generic, " not defined for yrqtr objects")
  val <- NextMethod(.Generic)
  class(val) <- oldClass(list(...)[[1]])
  val
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------- INTERNALS ------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

new_yrqtr <- function(x = numeric()) {
  structure(x, class = c("yrqtr", "grate"))
}

delayedAssign(
  "DAYS_IN_QUARTER",
  c(90L, 91L, 92L, 92L)
)

days_in_quarter <- function(year, quarter) {
  DAYS_IN_QUARTER[quarter] + ((quarter == 1) & is_leap_year(year))
}

