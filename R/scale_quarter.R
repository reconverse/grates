scale_type.grate_quarter <- function(x) {
  c("grate_quarter")
}

#' <grate_quarter> scale
#'
#' ggplot2 scale for <grate_quarter> vector.
#'
#' @param date_format Format to use if "Date" scales are required. If NULL then
#'   labels are centralised and of the form "lower category bound to upper
#'   category bound". If not NULL then the value is used by `format.Date()` and
#'   can be any input acceptable by that function.
#' @inheritParams scale_x_grate_month
#'
#' @return A scale for use with ggplot2.
#' @export
scale_x_grate_quarter <- function(n.breaks = 6, date_format = NULL) {

  # check ggplot2 is installed (this also ensures scales presence)
  check_suggests("ggplot2")

  ggplot2::scale_x_continuous(
    trans = grate_quarter_trans(
      n.breaks = n.breaks,
      date_format = date_format
    )
  )
}

grate_quarter_trans <- function(n.breaks, date_format) {

  if (is.null(date_format)) {
    shift <- 0
  } else {
    shift <- 3 / 2
  }

  origin <- 0
  interval <- 3
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
    if (is.null(date_format)) {
      format.grate_quarter(x)
    } else {
      x <- x + shift
      attr(x, "interval") <- 1
      class(x) <- "grate_month"
      format.grate_month(x, format = date_format)
    }
  }

  scales::trans_new(
    "grate_quarter",
    transform = as.numeric,
    inverse = as.numeric,
    breaks = brks,
    format = fmt
  )
}
