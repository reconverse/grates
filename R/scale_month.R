grate_month_env <-  new.env(parent = emptyenv())

#' <grate_month> scale
#'
#' ggplot2 scale for <grate_month> vector.
#'
#' @param n.breaks Approximate number of breaks calculated using
#'   `scales::breaks_pretty` (default 6).
#' @param date_format Format to use if "Date" scales are required. If NULL then
#'   labels are centralised and of the form "lower category bound to upper
#'   category bound". If not NULL then the value is used by `format.Date()` and
#'   can be any input acceptable by that function (defaults to "%Y-%m-%d).
#' @param bounds_format Format to use for grouped date labels. Only used if
#'   `date_format` is NULL.
#' @param sep Separator to use for grouped date labels.
#' @param interval An integer indicating the (fixed) number of months used for
#'   the original grouping.
#' @param origin The date used to anchor the grouping.
#'
#' @return A scale for use with ggplot2.
#' @export
scale_x_grate_month <- function(n.breaks = 6, date_format = "%Y-%m-%d",
                                bounds_format = "%Y-%b", sep = "to",
                                interval, origin) {

  # check ggplot2 is installed (this also ensures scales presence)
  check_suggests("ggplot2")

  if (missing(interval)) {
    interval <- grate_month_env$interval
    if (is.null(interval)) {
      abort("Please specify the `interval` of the grate_month input")
    }
  }

  if (missing(origin)) {
    origin <- grate_month_env$origin
    if (is.null(origin)) {
      abort("Please specify the `origin` of the grate_month input")
    }
  }

  ggplot2::scale_x_continuous(
    trans = grate_month_trans(
      n.breaks = n.breaks,
      date_format = date_format,
      bounds_format = bounds_format,
      sep = sep,
      interval = interval,
      origin = origin
    )
  )
}


scale_type.grate_month <- function(x) {
  grate_month_env$interval <- attr(x, "interval")
  grate_month_env$origin <- min(x, na.rm = TRUE)
  "grate_month"
}


grate_month_trans <- function(n.breaks, date_format, bounds_format, sep,
                              interval, origin) {

  if (is.null(date_format)) {
    shift <- 0
  } else {
    shift <- interval / 2
  }

  # breaks function
  brks <- function(x) {
    dat <- scales::breaks_pretty(n.breaks)(as.numeric(x))
    m <- min(dat, na.rm = TRUE)
    while (m < origin) {
      origin <- origin - interval
    }
    origin <- as.Date(new_grate_month(origin, interval = interval))
    dat <- as.Date(new_grate_month(dat, interval = interval))
    as.numeric(as_month(dat, interval = interval, origin = origin)) - shift
  }

  # format function
  fmt <- function(x) {
    x <- x + shift
    class(x) <- "grate_month"
    if (is.null(date_format)) {
      attr(x, "interval") <- interval
      format.grate_month(x, format = bounds_format, sep = sep)
    } else {
      attr(x, "interval") <- 1
      format.grate_month(x, format = date_format)
    }
  }

  # set environment variables to NULL so they don't mess other plots up
  grate_month_env$interval <- NULL
  grate_month_env$origin <- NULL

  scales::trans_new(
    "grate_month",
    transform = as.numeric,
    inverse = as.numeric,
    breaks = brks,
    format = fmt
  )
}
