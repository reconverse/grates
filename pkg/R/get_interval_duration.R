# -------------------------------------------------------------------------
#' The number of days covered
#'
# -------------------------------------------------------------------------
#' Generic utility function for returning the number of days covered by an element
#' of a grates object.
#'
# -------------------------------------------------------------------------
#' @param x A grouped date vector.
#'
#' @param ... Not currently used.
#'
# -------------------------------------------------------------------------
#' @return
#' The number of days covered from the start to end dates for each element in
#' the input.
#'
# -------------------------------------------------------------------------
#' @examples
#' # The following should be TRUE
#' identical(
#'     get_interval_duration(yearmonth(2020, 1:3)),
#'     c(31, 29, 31)
#' )
#'
# -------------------------------------------------------------------------
#' @export
get_interval_duration <- function(x, ...) {
    UseMethod("get_interval_duration")
}

# -------------------------------------------------------------------------
#' @export
get_interval_duration.default <- function(x, ...) {
    stopf("Not implemented for class [%s].", toString(class(x)))
}

# -------------------------------------------------------------------------
#' @export
get_interval_duration.grates_yearweek <- function(x, ...) {
    rep(7, length(x))
}

# -------------------------------------------------------------------------
#' @export
get_interval_duration.grates_int_period <- function(x, ...) {
    rep(attr(x, "n"), length(x))
}

# -------------------------------------------------------------------------
#' @export
get_interval_duration.grates_isoweek <- get_interval_duration.grates_yearweek

# -------------------------------------------------------------------------
#' @export
get_interval_duration.grates_epiweek <- get_interval_duration.grates_yearweek

# -------------------------------------------------------------------------
#' @export
get_interval_duration.grates_yearmonth <- function(x, ...) {
    as.double(as.Date(x + 1) - as.Date(x), units = "days")
}

# -------------------------------------------------------------------------
#' @export
get_interval_duration.grates_month <- get_interval_duration.grates_yearmonth

# -------------------------------------------------------------------------
#' @export
get_interval_duration.grates_yearquarter <- get_interval_duration.grates_yearmonth

# -------------------------------------------------------------------------
#' @export
get_interval_duration.grates_year <- get_interval_duration.grates_yearmonth

# -------------------------------------------------------------------------
#' @export
get_interval_duration.grates_period <- get_interval_duration.grates_int_period
