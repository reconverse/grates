grates_month_env <-  new.env(parent = emptyenv())

scale_type.grates_month <- function(x) {
  grates_month_env$n <- attr(x, "n")
  grates_month_env$origin <- attr(x, "origin")
  "grates_month"
}

#' <grates_month> scale
#'
#' ggplot2 scale for <grates_month> vector.
#'
#' @param n.breaks Approximate number of breaks calculated using
#'   `scales::breaks_pretty` (default 6).
#' @param format Format to use if "Date" scales are required. If NULL then
#'   labels are centralised and of the form "lower category bound to upper
#'   category bound". If not NULL then the value is used by `format.Date()` and
#'   can be any input acceptable by that function (defaults to "%Y-%m-%d).
#' @param bounds_format Format to use for grouped date labels. Only used if
#'   `format` is NULL.
#' @param sep Separator to use for grouped date labels.
#' @param n Number of months used for the original grouping.
#' @param origin Month since the Unix epoch used in the original grouping.
#'
#' @return A scale for use with ggplot2.
#' @export
scale_x_grates_month <- function(n.breaks = 6, format = "%Y-%m-%d",
                                bounds_format = "%Y-%b", sep = "to",
                                n, origin) {

  # check ggplot2 is installed (this also ensures scales presence)
  check_suggests("ggplot2")

  if (missing(n)) {
    n <- grates_month_env$n
    if (is.null(n)) {
      abort("Please specify the `n` of the grate_month input")
    }
  }

  if (missing(origin)) {
    origin <- grates_month_env$origin
    if (is.null(origin)) {
      abort("Please specify the `origin` of the grate_month input")
    }
  }

  ggplot2::scale_x_continuous(
    trans = grates_month_trans(
      n.breaks = n.breaks,
      format = format,
      bounds_format = bounds_format,
      sep = sep,
      n = n,
      origin = origin
    )
  )
}


grates_month_trans <- function(n.breaks, format, bounds_format, sep, n, origin) {

  if (is.null(format)) {
    shift <- 0
  } else {
    shift <- n / 2
  }

  # breaks function
  brks <- function(x) {
    dat <- scales::breaks_pretty(n.breaks)(as.numeric(x))
    as.numeric(as_month(dat, n = n, origin = origin)) - shift
  }

  # format function
  fmt <- function(x) {
    x <- x + shift
    class(x) <- c("grates_month", "vctrs_vctr")
    if (is.null(format)) {
      attr(x, "n") <- as.integer(n)
      attr(x, "origin") <- as.integer(origin)
      format.grates_month(x, format = bounds_format, sep = sep)
    } else {
      attr(x, "n") <- 1L
      attr(x, "origin") <- as.integer(origin)
      format.grates_month(x, format = format)
    }
  }

  # set environment variables to NULL so they don't mess other plots up
  grates_month_env$n <- NULL
  grates_month_env$origin <- NULL

  scales::trans_new(
    "grates_month",
    transform = as.numeric,
    inverse = as.numeric,
    breaks = brks,
    format = fmt
  )
}
