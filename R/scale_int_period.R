grate_int_period_env <-  new.env(parent = emptyenv())

#' <grate_period> scale
#'
#' ggplot2 scale for <grate_period> vector.
#'
#' @inheritParams scale_x_grate_period
#'
#' @return A scale for use with ggplot2.
#' @export
scale_x_grate_int_period <- function(n.breaks = 6, interval, origin) {

  # check ggplot2 is installed (this also ensures scales presence)
  check_suggests("ggplot2")

  if (missing(interval)) {
    interval <- grate_int_period_env$interval
    if (is.null(interval)) {
      stop("Please specify the `interval` of the grate_int_period input", call. = FALSE)
    }
  }

  if (missing(origin)) {
    origin <- grate_int_period_env$origin
    if (is.null(origin)) {
      stop("Please specify the `origin` of the grate_int_period input", call. = FALSE)
    }
  }

  ggplot2::scale_x_continuous(
    trans = grate_int_period_trans(
      n.breaks = n.breaks,
      interval = interval,
      origin = origin
    )
  )
}


scale_type.grate_int_period <- function(x) {
  grate_int_period_env$interval <- attr(x, "interval")
  grate_int_period_env$origin <- min(x, na.rm = TRUE)
  c("grate_int_period")
}



grate_int_period_trans <- function(n.breaks, interval, origin) {

  brks <- function(x) {
    dat <- scales::pretty_breaks(n.breaks)(x)
    m <- floor(min(dat, na.rm = TRUE))
    origin <- as.integer(origin)
    if (m < origin) {
      origin <- m - (interval - ((origin - as.numeric(m)) %% interval))
    }
    as.integer(as_period(dat, interval = interval, origin = origin)) - interval/2
  }

  # format function
  fmt <- function(x) {
    as.numeric(x) + interval/2
  }

  # set environment variables to NULL so they don't mess other plots up
  grate_int_period_env$interval <- NULL
  grate_int_period_env$origin <- NULL

  scales::trans_new(
    "grate_int_period",
    transform = as.numeric,
    inverse = as.numeric,
    breaks = brks,
    format = fmt
  )
}


