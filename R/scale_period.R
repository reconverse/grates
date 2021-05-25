grates_period_env <-  new.env(parent = emptyenv())

scale_type.grates_period <- function(x) {
  grates_period_env$n <- attr(x, "n")
  grates_period_env$origin <- attr(x, "origin")
  "grates_period"
}

#' <grates_period> scale
#'
#' ggplot2 scale for <grates_period> vector.
#'
#' @param n.breaks Approximate number of breaks calculated using
#'   `scales::breaks_pretty` (default 6).
#' @param format Format to use for x scale. Passed to [`format.Date()`].
#' @param n Number of months used for the original grouping.
#' @param origin Month since the Unix epoch used in the original grouping.
#'
#' @return A scale for use with ggplot2.
#' @export
scale_x_grates_period <- function(n.breaks = 6, format = "%Y-%m-%d", n, origin) {

  # check ggplot2 is installed (this also ensures scales presence)
  check_suggests("ggplot2")

  if (missing(n)) {
    n <- grates_period_env$n
    if (is.null(n)) {
      abort("Please specify the `n` of the grate_period input")
    }
  }

  if (missing(origin)) {
    origin <- grates_period_env$origin
    if (is.null(origin)) {
      abort("Please specify the `origin` of the grate_period input")
    }
  }

  ggplot2::scale_x_continuous(
    trans = grates_period_trans(
      n.breaks = n.breaks,
      format = format,
      n = n,
      origin = origin
    )
  )
}


grates_period_trans <- function(n.breaks, format, n, origin) {
  shift <- n / 2

  # breaks function
  brks <- function(x) {
    dat <- trunc(scales::breaks_pretty(n.breaks)(new_date(x)))
    as.numeric(as_period(dat, n = n, origin = origin)) - shift
  }

  # format function
  fmt <- function(x) {
    x <- x + shift
    class(x) <- c("grates_period", "vctrs_vctr")
    attr(x, "n") <- 1L
    attr(x, "origin") <- as.integer(origin)
    format.grates_period(x, format = format)
  }

  # set environment variables to NULL so they don't mess other plots up
  grates_period_env$n <- NULL
  grates_period_env$origin <- NULL

  scales::trans_new(
    "grates_period",
    transform = as.numeric,
    inverse = as.numeric,
    breaks = brks,
    format = fmt
  )
}
