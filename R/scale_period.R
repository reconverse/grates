grate_period_env <-  new.env(parent = emptyenv())

#' <grate_period> scale
#'
#' ggplot2 scale for <grate_period> vector.
#'
#' @param n.breaks Approximate number of breaks calculated using
#'   `scales::breaks_pretty` (default 6).
#' @param format Format to use for x scale. Passed to [`format.Date()`].
#' @inheritParams as_period
#'
#' @return A scale for use with ggplot2.
#' @export
scale_x_grate_period <- function(n.breaks = 6, format = "%Y-%m-%d", interval, origin) {

  # check ggplot2 is installed (this also ensures scales presence)
  check_suggests("ggplot2")

  if (missing(interval)) {
    interval <- grate_period_env$interval
    if (is.null(interval)) {
      abort("Please specify the `interval` of the grate_period input")
    }
  }

  if (missing(origin)) {
    origin <- grate_period_env$origin
    if (is.null(origin)) {
      abort("Please specify the `origin` of the grate_period input")
    }
  }

  ggplot2::scale_x_continuous(
    trans = grate_period_trans(
      n.breaks = n.breaks,
      format = format,
      interval = interval,
      origin = origin
    )
  )
}


scale_type.grate_period <- function(x) {
  grate_period_env$interval <- attr(x, "interval")
  grate_period_env$origin <- min(x, na.rm = TRUE)
  "grate_period"
}


grate_period_trans <- function(n.breaks, format, interval, origin) {

  # breaks function
  brks <- function(x) {
    dat <- trunc(scales::pretty_breaks(n.breaks)(new_date(x)))
    m <- as.numeric(min(dat, na.rm = TRUE))
    if (m < origin) {
      origin <- m - (interval - ((as.numeric(origin) - m) %% interval))
    }
    tmp <- as_period(dat, interval = interval, origin = new_date(origin))
    as.Date(tmp) - interval / 2
  }

  # format function
  fmt <- function(x) {
    format(new_date(round(x + interval/2)), format)
  }

  # set environment variables to NULL so they don't mess other plots up
  grate_period_env$interval <- NULL
  grate_period_env$origin <- NULL

  scales::trans_new(
    "grate_period",
    transform = new_date,
    inverse = as.numeric,
    breaks = brks,
    format = fmt
  )
}
