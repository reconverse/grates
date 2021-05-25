scale_type.grates_quarter <- function(x) {
  c("grates_quarter")
}

#' <grates_quarter> scale
#'
#' ggplot2 scale for <grates_quarter> vector.
#'
#' @param format Format to use if "Date" scales are required. If NULL then
#'   labels are centralised and of the form "YYYY-Qq. If not NULL then the value
#'   is used by `format.Date()` and can be any input acceptable by that function.
#' @inheritParams scale_x_grates_month
#'
#' @return A scale for use with ggplot2.
#' @export
scale_x_grates_quarter <- function(n.breaks = 6, format = NULL) {

  # check ggplot2 is installed (this also ensures scales presence)
  check_suggests("ggplot2")

  ggplot2::scale_x_continuous(
    trans = grates_quarter_trans(
      n.breaks = n.breaks,
      format = format
    )
  )
}

grates_quarter_trans <- function(n.breaks, format) {

  if (is.null(format)) {
    shift <- 0
  } else {
    shift <- 3 / 2
  }

  # breaks function
  brks <- function(x) {
    dat <- scales::breaks_pretty(n.breaks)(as.numeric(x))
    as.numeric(as_month(dat, n = 3L, origin = 0L)) - shift
  }

  # format function
  fmt <- function(x) {
    if (is.null(format)) {
      format.grates_quarter(x)
    } else {
      x <- x + shift
      attr(x, "n") <- 1L
      class(x) <- c("grates_month", "vctrs_vctr")
      format.grates_month(x, format = format)
    }
  }

  scales::trans_new(
    "grates_quarter",
    transform = as.numeric,
    inverse = as.numeric,
    breaks = brks,
    format = fmt
  )
}
