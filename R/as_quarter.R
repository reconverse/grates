# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------ AS_QUARTER ------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' Convert an object to a quarter
#'
#' @description
#' - Date, POSIXct, and POSIXlt are converted, with the timezone respected,
#'   using [clock::as_date()].
#'
#' - Character input is parsed using [clock::date_parse()].
#'
#' @param x An object to coerce to quarterly.
#' @inheritParams as_month
#' @inheritParams clock::date_parse
#' @param ... Not currently used.
#'
#' @return A `grate_quarter` object.
#'
#' @examples
#' as_quarter(Sys.Date())
#' as_quarter(as.POSIXct("2019-03-04 01:01:01", tz = "America/New_York"))
#' as_quarter("2019-05-03")
#'
#' @note Internally `grate_quarter` objects are stored as the number of
#'   months to the lower bound of the quarter (starting at 0) since the Unix
#'   Epoch (1970-01-01).
#'
#' @export
as_quarter <- function(x, ...) {
  UseMethod("as_quarter")
}


#' @rdname as_quarter
#' @export
as_quarter.default <- function(x, ...) {
  abort(sprintf("Can't convert a <%s> to a <grate_quarter>" , class(x)[1]))
}


#' @rdname as_quarter
#' @export
as_quarter.Date <- function(x, ...) {
  check_dots_empty()
  out <- as_month.Date(x, interval = 3, origin = ORIGIN)
  class(out) <- c("grate_quarter", class(out))
  out
}


#' @rdname as_quarter
#' @export
as_quarter.POSIXt <- function(x, ...) {
  check_dots_empty()
  x <- as_date(x)
  out <- as_month.Date(x = x, interval = 3, origin = ORIGIN)
  class(out) <- c("grate_quarter", class(out))
  out
}


#' @rdname as_quarter
#' @export
as_quarter.character <- function(x, format = NULL, locale = clock_locale(), ...) {
  check_dots_empty()
  x <- date_parse(x, format = format, locale = locale)
  if (all(is.na(x))) abort("Unable to parse any entries of x as Dates")
  out <- as_month.Date(x = x, interval = 3, origin = ORIGIN)
  class(out) <- c("grate_quarter", class(out))
  out
}


#' @rdname as_quarter
#' @export
as_quarter.factor <- function(x, format = NULL, locale = clock_locale(), ...) {
  check_dots_empty()
  as_quarter.character(
    as.character(x),
    format = format,
    locale = locale
  )
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------- FORMATING / PRINTING -------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' Print a grate_quarter object
#'
#' @param x A grate_quarter object.
#' @param ... Not currently used.
#'
#' @export
format.grate_quarter <- function(x, ...) {
  if (length(x) == 0) return(character(0))
  days <- month_to_days(as.numeric(x))
  x <- as_utc_posixlt_from_int(days)
  out <- sprintf("%04d-Q%d", x$year + 1900L, x$mon %/% 3L +1)
  out[is.na(x)] <- NA_character_
  names(out) <- names(x)
  out
}

#' @export
print.grate_quarter <- function(x, ...) {
  cat("<grate_quarter>\n")
  print(format.grate_quarter(x))
  invisible(x)
}

# make nice headings for tibble columns
#' @export
vec_ptype_abbr.grate_quarter <- function(x) "qtr"

