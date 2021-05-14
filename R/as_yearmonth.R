# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------ AS_YEARMONTH ----------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' Convert an object to yearmonth
#'
#' @description
#' - Date, POSIXct, and POSIXlt are converted, with the timezone respected,
#'   using [clock::as_date()].
#'
#' - Character input is parsed using [clock::date_parse()].
#'
#' @param x An object to coerce to yearmonthly.
#' @param ... Not currently used.
#'
#' @return A `grate_yearmonth` object.
#'
#' @examples
#' as_yearmonth(Sys.Date())
#' as_yearmonth(as.POSIXct("2019-03-04 01:01:01", tz = "America/New_York"))
#' as_yearmonth("2019-05-03")
#'
#' @note Internally `grate_yearmonth` objects are stored as the number of months
#'   (starting at 0) since the Unix Epoch (1970-01-01).
#'
#' @references The algorithm to convert between dates and months relative to the
#'   UNIX Epoch comes from the work of Davis Vaughan in the unreleased
#'   [datea](https://github.com/DavisVaughan/datea/) package.
#'
#' @export
as_yearmonth <- function(x, ...) {
  UseMethod("as_yearmonth")
}


#' @rdname as_yearmonth
#' @export
as_yearmonth.default <- function(x, ...) {
  stop(
    sprintf("Can't convert a <%s> to a <grate_yearmonth>" , class(x)[1]),
    call. = FALSE
  )
}


#' @rdname as_yearmonth
#' @export
as_yearmonth.Date <- function(x, ...) as_yrmon(x)


#' @rdname as_yearmonth
#' @export
as_yearmonth.POSIXt <- function(x, ...) as_yrmon(x)


#' @inheritParams clock::date_parse
#' @rdname as_yearmonth
#' @export
as_yearmonth.character <- function(x, format = NULL, locale = clock_locale(), ...) {
  as_yrmon(x, format = format, locale = locale)
}


#' @rdname as_yearmonth
#' @export
as_yearmonth.factor <- function(x, format = NULL, locale = clock_locale(), ...) {
  as_yrmon(x, format = format, locale = locale)
}


#' Print a grate_yearmonth object
#'
#' @param x A grate_yearmonth object.
#' @param format The format to use for the yearmonth value.
#' @param ... Not currently used.
#'
#' @export
print.grate_yearmonth <- function(x, format = "%Y-%b", ...) {
  interval <- attr(x, "interval")
  cat("<grate_yearmonth>")
  print(format.grate_month(x, format = format))
  invisible(x)
}


#' @rdname print.grate_yearmonth
#' @export
format.grate_yearmonth <- function(x, format = "%Y-%b", ...) {
  if (length(x) == 0) return(character(0))
  out <- format.Date(as.Date(x), format = format)
  out[is.na(x)] <- NA_character_
  out
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------- INTERNALS ------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
as_yrmon <- function(x, ...) {
  out <- as_month(x, ...)
  class(out) <- c("grate_yearmonth", class(out))
  out
}
