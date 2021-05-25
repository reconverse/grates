grates_int_period_env <-  new.env(parent = emptyenv())

scale_type.grates_int_period <- function(x) {
  grates_int_period_env$n <- attr(x, "n")
  grates_int_period_env$origin <- attr(x, "origin")
  "grates_int_period"
}

#' <grates_int_period> scale
#'
#' ggplot2 scale for <grates_int_period> vector.
#'
#' @param n.breaks Approximate number of breaks calculated using
#'   `scales::breaks_pretty` (default 6).
#' @param n Number of days used for the original grouping.
#' @param origin Original day on which the grouping began.
#'
#' @return A scale for use with ggplot2.
#' @export
scale_x_grates_int_period <- function(n.breaks = 6, n, origin) {

  # check ggplot2 is installed (this also ensures scales presence)
  check_suggests("ggplot2")

  if (missing(n)) {
    n <- grates_int_period_env$n
    if (is.null(n)) {
      abort("Please specify the `n` of the grate_int_period input")
    }
  }

  if (missing(origin)) {
    origin <- grates_int_period_env$origin
    if (is.null(origin)) {
      abort("Please specify the `origin` of the grate_int_period input")
    }
  }

  ggplot2::scale_x_continuous(
    trans = grates_int_period_trans(
      n.breaks = n.breaks,
      n = n,
      origin = origin
    )
  )
}


grates_int_period_trans <- function(n.breaks, n, origin) {
  shift <- n / 2

  # breaks function
  brks <- function(x) {
    dat <- trunc(scales::breaks_pretty(n.breaks)(as.numeric(x)))
    as.integer(as_int_period(dat, n = n, origin = origin)) - shift
  }

  # format function
  fmt <- function(x) {
    x <- x + shift
    class(x) <- c("grates_int_period", "vctrs_vctr")
    attr(x, "n") <- 1L
    format.grates_int_period(x, format = format)
  }

  # set environment variables to NULL so they don't mess other plots up
  grates_int_period_env$n <- NULL
  grates_int_period_env$origin <- NULL

  scales::trans_new(
    "grates_int_period",
    transform = as.numeric,
    inverse = as.numeric,
    breaks = brks,
    format = fmt
  )
}
