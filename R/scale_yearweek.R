#' <grate_yearweek> scale
#'
#' ggplot2 scale for <grate_yearweek> vector.
#'
#' @param n.breaks Approximate number of breaks calculated using
#'   `scales::breaks_pretty` (default 6).
#' @param format Format to use if "Date" scales are required. If NULL
#'   (default) then labels are centralised and of the form "lower category bound
#'   to upper category bound". If not NULL then the value is used by
#'   `format.Date()` and can be any input acceptable by that function.
#' @param firstday Integer value of the first weekday: 1 (Monday) to 7 (Sunday).
#' @param ... Not currently used.
#'
#' @return A scale for use with ggplot2.


#' @export
scale_x_grate_yearweek <- function(..., n.breaks = 6, firstday, format = NULL) {

  # check ggplot2 is installed (this also ensures scales presence)
  check_suggests("ggplot2")

  if (missing(firstday)) {
    abort("Please provide a value of `firstday` corresponding to the given data")
  }

  if (!is.wholenumber(firstday) || firstday < 1 || firstday > 7) {
    abort("`x` must be a whole number between 1 and 7 (inclusive)")
  }

  ggplot2::scale_x_continuous(
    trans = grate_yearweek_trans(
      n.breaks = n.breaks,
      firstday = firstday,
      format = format
    )
  )
}


# When adding a geom, ggplot2 will look for a relevant method to the generic
# scale_type.  It will then dispatch based on the output of this function to
# one of scale_x_yearweek_fd_xxx defined below.
# I think this is a cleaner approach then the one we are forced to employ with
# the `period` and month classes.

scale_type.grate_yearweek <- function(x) {
  fd <- attr(x, "firstday")
  sprintf("yearweek_%d", fd)
}

grate_yearweek_trans <- function(n.breaks, firstday, format) {

  if (is.null(format)) {
    shift <- 0
  } else {
    shift <- 0.5
  }

  # breaks function
  brks <- function(x) {
    dat <- scales::breaks_pretty(n.breaks)(as.numeric(x))
    dat <- as.Date(new_grate_yearweek(dat, firstday = firstday))
    as.numeric(as_yearweek(dat, firstday = firstday)) - shift
  }

  # format function
  fmt <- function(x) {
    x <- x + shift
    attr(x, "firstday") <- firstday
    class(x) <- "grate_yearweek"
    if (is.null(format)) {
      format.grate_yearweek(x)
    } else {
      format.Date(as.Date(x), format)
    }
  }

  scales::trans_new(
    "grate_yearweek",
    transform = as.numeric,
    inverse = as.numeric,
    breaks = brks,
    format = fmt
  )
}


#' Hidden scales
#'
#' Wrappers around [scale_x_grate_yearweek()] with pre-specified firstday argument.
#'
#' @inheritParams scale_x_grate_yearweek
#'
#' @return A scale for use with ggplot2.
#'
#' @name hidden-scales
NULL

#' @export
#' @keywords internal
#' @rdname hidden-scales
scale_x_yearweek_1 <- function(..., n.breaks = 6) {
  scale_x_grate_yearweek(..., n.breaks = n.breaks, firstday = 1)
}

#' @export
#' @keywords internal
#' @rdname hidden-scales
scale_x_yearweek_2 <- function(..., n.breaks = 6) {
  scale_x_grate_yearweek(..., n.breaks = n.breaks, firstday = 2)
}

#' @export
#' @keywords internal
#' @rdname hidden-scales
scale_x_yearweek_3 <- function(..., n.breaks = 6) {
  scale_x_grate_yearweek(..., n.breaks = n.breaks, firstday = 3)
}

#' @export
#' @keywords internal
#' @rdname hidden-scales
scale_x_yearweek_4 <- function(..., n.breaks = 6) {
  scale_x_grate_yearweek(..., n.breaks = n.breaks, firstday = 4)
}

#' @export
#' @keywords internal
#' @rdname hidden-scales
scale_x_yearweek_5 <- function(..., n.breaks = 6) {
  scale_x_grate_yearweek(..., n.breaks = n.breaks, firstday = 5)
}

#' @export
#' @keywords internal
#' @rdname hidden-scales
scale_x_yearweek_6 <- function(..., n.breaks = 6) {
  scale_x_grate_yearweek(..., n.breaks = n.breaks, firstday = 6)
}

#' @export
#' @keywords internal
#' @rdname hidden-scales
scale_x_yearweek_7 <- function(..., n.breaks = 6) {
  scale_x_grate_yearweek(..., n.breaks = n.breaks, firstday = 7)
}
