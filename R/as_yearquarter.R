# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ---------------------------- AS_YEARQUARTER ----------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' Convert an object to yearquarter
#'
#' @description
#' - Date, POSIXct, and POSIXlt are converted, with the timezone respected,
#'   using [clock::as_date()].
#'
#' - Character input is parsed using [clock::date_parse()].
#'
#' @param x An object to coerce to yearquarterly.
#' @param ... Not currently used.
#'
#' @return A `grate_yearquarter` object.
#'
#' @examples
#' as_yearquarter(Sys.Date())
#' as_yearquarter(as.POSIXct("2019-03-04 01:01:01", tz = "America/New_York"))
#' as_yearquarter("2019-05-03")
#'
#' @note Internally `grate_yearquarter` objects are stored as the number of
#'   months to the lower bound of the quarter (starting at 0) since the Unix
#'   Epoch (1970-01-01).
#'
#' @export
as_yearquarter <- function(x, ...) {
  UseMethod("as_yearquarter")
}


#' @rdname as_yearquarter
#' @export
as_yearquarter.default <- function(x, ...) {
  stop(
    sprintf("Can't convert a <%s> to a <grate_yearquarter>" , class(x)[1]),
    call. = FALSE
  )
}


#' @rdname as_yearquarter
#' @export
as_yearquarter.Date <- function(x,  ...) {
  out <- as_month.Date(x, n = 3, origin = as.Date("1970-01-01"))
  class(out) <- c("grate_yearquarter", class(out))
  out
}


#' @rdname as_yearquarter
#' @importFrom clock as_date
#' @export
as_yearquarter.POSIXt <- function(x,  ...) {
  x <- as_date(x)
  out <- as_month.Date(x = x, n = 3, origin = as.Date("1970-01-01"))
  class(out) <- c("grate_yearquarter", class(out))
  out
}


#' @inheritParams clock::date_parse
#' @rdname as_yearquarter
#' @export
as_yearquarter.character <- function(x, format = NULL, locale = clock_locale(), ...) {
  x <- date_parse(x, format = format, locale = locale)
  if (all(is.na(x))) stop("Unable to parse any entries of x as Dates")
  out <- as_month.Date(x = x, n = 3, origin = as.Date("1970-01-01"))
  class(out) <- c("grate_yearquarter", class(out))
  out
}


#' @inheritParams clock::date_parse
#' @rdname as_yearquarter
#' @export
as_yearquarter.factor <- function(x, format = NULL, locale = clock_locale(), ...) {
  as_yearquarter.character(as.character(x), format = format, locale = locale)
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------- FORMATING / PRINTING -------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' Print a grate_yearquarter object
#'
#' @param x A grate_yearquarter object.
#' @param ... Not currently used.
#'
#' @export
format.grate_yearquarter <- function(x, ...) {
  if (length(x) == 0) return(character(0))
  days <- month_to_days(as.numeric(x))
  x <- as_utc_posixlt_from_int(days)
  out <- sprintf("%04d-Q%d", x$year + 1900L, x$mon %/% 3L +1)
  out[is.na(x)] <- NA_character_
  names(out) <- names(x)
  out
}

#' @export
print.grate_yearquarter <- function(x, ...) {
  cat("<grate_yearquarter>\n")
  print(format.grate_month(x))
  invisible(x)
}
