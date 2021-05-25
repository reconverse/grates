scale_type.grates_year <- function(x) "grates_year"

#' <grates_year> scale
#'
#' ggplot2 scale for <grates_year> vector.
#'
#' @param n.breaks Approximate number of breaks calculated using
#'   `scales::breaks_pretty` (default 6).
#' @param ... Not currently used.
#'
#' @return A scale for use with ggplot2.
#' @export
scale_x_grates_year <- function(..., n.breaks = 6) {
  check_suggests("ggplot2")
  ggplot2::scale_x_continuous(..., trans = grates_year_trans(n.breaks))
}

grates_year_trans <- function(n.breaks) {

  # breaks function
  brks <- function(x) {
    as.integer(scales::pretty_breaks(n.breaks)(as.integer(x)))
  }

  # format function
  fmt <- function(x) format.grates_year(new_year(as.integer(x)))

  scales::trans_new(
    "grates_year",
    transform = as.numeric,
    inverse = as.numeric,
    breaks = brks,
    format = fmt
  )

}


