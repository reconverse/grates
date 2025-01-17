#' Access the start (end) dates of a grates vector
#'
#' Utility functions for accessing the start (end) dates for each element of
#' a grates object and also checking whether a date is contained within that
#' range
#'
#' @param x grouped date vector.
#' @param date A scalar `<date>` object.
#'
#' @return
#' For `date_start` and `date_end` The requested start (end) dates for each
#' element in the input. For `%during%` a logical vector indicating whether the
#' date was present within the range of the tested object.
#'
#' @examples
#' dates <- as.Date("2020-01-01") + 1:14
#'
#' week <- as_isoweek(dates)
#' date_start(week)
#' date_end(week)
#' dates[1L] %during% week
#'
#' period <- as_period(dates, n = 3)
#' date_start(period)
#' date_end(period)
#' dates[14L] %during% period
#'
#' @name boundaries
NULL

#' @rdname boundaries
#' @export
date_start <- function(x) {
    .assert_grate(x)
    return(as.Date(x))
}

#' @rdname boundaries
#' @export
date_end <- function(x) {
    .assert_grate(x)
    as.Date(x + 1L) - 1L
}

#' @rdname boundaries
#' @export
`%during%` <- function(date, x) {
    .assert_scalar_date(date)
    .assert_grate(x)
    start <- date_start(x)
    end <- date_end(x)
    (date >= start) & (date <= end)
}
