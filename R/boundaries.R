#' Access the start (end) dates of a grates vector
#'
#' Utility functions for accessing the start (end) dates for each element of
#' a grates object.
#'
#' @param x grouped date vector.
#'
#' @return
#' The requested start (end) dates for each element in the input.
#'
#' @examples
#' dates <- as.Date("2020-01-01") + 1:14
#'
#' week <- as_isoweek(dates)
#' date_start(week)
#' date_end(week)
#'
#' period <- as_period(dates, n = 3)
#' date_start(period)
#' date_end(period)
#'
#' @name boundaries
NULL

.grates_classes <- c(
    "grates_yearweek" , "grates_isoweek", "grates_epiweek",
    "grates_yearmonth", "grates_month"  , "grates_yearquarter",
    "grates_year"     , "grates_period"
)

#' @rdname boundaries
#' @export
date_start <- function(x) {

    if (inherits(x, .grates_classes))
        return(as.Date(x))

    stop("`x` must be a grouped date (<grates>) object.")
}

#' @rdname boundaries
#' @export
date_end <- function(x) {

    if (inherits(x, .grates_classes))
        return(as.Date(x + 1L) - 1L)

    stop("`x` must be a grouped date (<grates>) object.")
}



