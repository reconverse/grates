# -------------------------------------------------------------------------
#' Access the bounding dates of a grates object
#'
# -------------------------------------------------------------------------
#' Utility functions for accessing the boundary dates for each element of
#' a grates object.
#'
# -------------------------------------------------------------------------
#' @param x grouped date vector.
#'
# -------------------------------------------------------------------------
#' @return
#' The requested start and end dates for each element in the input.
#'
# -------------------------------------------------------------------------
#' @examples
#' dates <- as.Date("2020-01-01") + 1:9
#' week <- as_isoweek(dates)
#' date_start(week)
#' date_end(week)
#'
#' period <- as_period(dates, n = 3)
#' date_start(period)
#' date_end(period)
#'
# -------------------------------------------------------------------------
#' @name boundaries
NULL

#' @rdname boundaries
#' @export
date_start <- function(x) {
    .assert_grate(x)
    as.Date(x)
}

#' @rdname boundaries
#' @export
date_end <- function(x) {
    .assert_grate(x)
    as.Date(x + 1L) - 1L
}
