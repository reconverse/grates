#' Is object a grouped date
#'
#' @param x Grouped date object.
#'
#' @return
#' Logical.
#'
#' @examples
#' is_grate(as_yearweek(Sys.Date()))
#'
#' @export
is_grate <- function(x) {
  is_month(x) || is_quarter(x) || is_year(x) || is_yearweek(x) || is_period(x) || is_int_period(x)
}
