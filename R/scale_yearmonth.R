scale_type.grate_yearmonth <- function(x) "grate_yearmonth"

#' <grate_yearmonth> scale
#'
#' ggplot2 scale for <grate_yearmonth> vector.
#'
#' @inheritParams scale_x_grate_month
#'
#' @return A scale for use with ggplot2.
#' @export
scale_x_grate_yearmonth <- function(n.breaks = 6, date_format = NULL,
                                    bounds_format = "%Y-%b", sep = "to") {

  # check ggplot2 is installed (this also ensures scales presence)
  check_suggests("ggplot2")

  ggplot2::scale_x_continuous(
    trans = grate_month_trans(
      n.breaks = n.breaks,
      date_format = date_format,
      bounds_format = bounds_format,
      sep = sep,
      interval = 1,
      origin = 0
    )
  )
}
