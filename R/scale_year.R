scale_type.grate_year <- function(x) "grate_year"

#' <grate_year> scale
#'
#' ggplot2 scale for <grate_year> vector.
#'
#' @param n.breaks Approximate number of breaks calculated using
#'   `scales::breaks_pretty` (default 6).
#' @param ... Not currently used.
#'
#' @return A scale for use with ggplot2.
#' @export
scale_x_grate_year <- function(..., n.breaks = 6) {
  check_suggests("ggplot2")
  ggplot2::scale_x_continuous(..., trans = grate_year_trans(n.breaks))
}

grate_year_trans <- function(n.breaks) {

  # breaks function
  brks <- function(x) scales::pretty_breaks(n.breaks)(as.integer(x))

  # format function
  fmt <- function(x) format.grate_year(new_grate_year(x))

  scales::trans_new(
    "grate_year",
    transform = as.numeric,
    inverse = as.numeric,
    breaks = brks,
    format = fmt
  )

}


