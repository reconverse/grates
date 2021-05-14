#'  Is object a grouped date
#'
#' @param x Grouped date object.
#'
#' @return
#' Logical.
#'
#' @name is_grate
NULL

#' @rdname is_grate
#' @export
is_grate <- function(x) {
  gm <- inherits(x, "grate_month")
  gyw <- inherits(x, "grate_yearweek")
  gy <- inherits(x, "grate_year")
  gp <-   inherits(x, "grate_period")

  gm || gyw || gy || gp
}

#' @rdname is_grate
#' @export
is_yearweek <- function(x) {
  inherits(x, "grate_yearweek")
}


#' @rdname is_grate
#' @export
is_month <- function(x) {
  inherits(x, "grate_month")
}

#' @rdname is_grate
#' @export
is_yearmonth <- function(x) {
  inherits(x, "grate_yearmonth")
}

#' @rdname is_grate
#' @export
is_yearquarter <- function(x) {
  inherits(x, "grate_yearquarter")
}

#' @rdname is_grate
#' @export
is_year <- function(x) {
  inherits(x, "grate_year")
}

#' @rdname is_grate
#' @export
is_period <- function(x) {
  inherits(x, "grate_period")
}

#' @rdname is_grate
#' @export
is_int_period <- function(x) {
  inherits(x, "grate_int_period")
}
