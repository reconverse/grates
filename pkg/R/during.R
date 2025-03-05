# -------------------------------------------------------------------------
#' Is a date covered by a grouped date
#'
# -------------------------------------------------------------------------
#' `%during%` determines whether a supplied date is within the period covered
#' by each element of a grates object.
#'
# -------------------------------------------------------------------------
#' @param date A scalar `<date>` object.
#'
#' @param x grouped date vector.
#'
# -------------------------------------------------------------------------
#' @return
#'
#' A logical vector indicating whether the date was present within the range of
#' the tested object.
# -------------------------------------------------------------------------
#' @examples
#'
#' dates <- as.Date("2020-01-01") + 1:10
#' week <- as_isoweek(dates)
#' dates[1] %during% week
#'
#' period <- as_period(dates, n = 3)
#' dates[10] %during% period
#'
# -------------------------------------------------------------------------
#' @export
`%during%` <- function(date, x) {
    .assert_scalar_date(date)
    .assert_grate(x)
    start <- date_start(x)
    end <- date_end(x)
    (date >= start) & (date <= end)
}
