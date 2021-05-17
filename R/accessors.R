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
is_quarter <- function(x) {
  inherits(x, "grate_quarter")
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


#' Grouped date accessors
#'
#' Generics and methods to work with grouped date objects
#'
#' @param x A grate object.
#' @param ... Not used.
#' @return
#'
#'   - `get_date_bounds()`: A data.frame containing the upper and lower date
#'     bounds for each of the grouped date values.
#'   - `get_week()`: The corresponding week values for a <grate_yearweek> vector.
#'
#' @name grate_accessors
#'
NULL

#' @rdname grate_accessors
#' @export
get_date_bounds <- function(x, ...) {
  UseMethod("get_date_bounds")
}

#' @rdname grate_accessors
#' @export
get_date_bounds.default <- function(x, ...) {
  abort(
    sprintf("Not implemented for class %s", paste(class(x), collapse = ", "))
  )
}

#' @rdname grate_accessors
#' @export
get_date_bounds.grate_month <- function(x, ...) {
  get_bounds(x)
}

#' @rdname grate_accessors
#' @export
get_date_bounds.grate_yearweek <- function(x, ...) {
  get_bounds(x)
}

get_bounds <- function(x) {
  lower_bound <- as.Date(x)
  upper_bound <- as.Date(x + 1) - 1
  data.frame(grouping = x, lower_bound, upper_bound)
}


#' @rdname grate_accessors
#' @export
get_week <- function(x, ...) {
  UseMethod("get_week")
}

#' @rdname grate_accessors
#' @export
get_week.default <- function(x, ...) {
  abort(
    sprintf("Not implemented for class %s", paste(class(x), collapse = ", "))
  )
}

#' @rdname grate_accessors
#' @export
get_week.grate_yearweek <- function(x, ...) {
  yearweek_to_week(x)
}


#' @rdname grate_accessors
#' @export
get_month <- function(x, ...) {
  UseMethod("get_month")
}

#' @rdname grate_accessors
#' @export
get_month.default <- function(x, ...) {
  abort(
    sprintf("Not implemented for class %s", paste(class(x), collapse = ", "))
  )
}


#' @param style Either "numeric" (default) for the integer month value or
#'   "named" to return the abbreviated month name in the current locale.
#'
#' @rdname grate_accessors
#' @export
get_month.grate_month <- function(x, style = c("numeric", "named"), ...) {
  style <- match.arg(style)

  attributes(x) <- NULL
  days <- month_to_days(x)
  x <- as_utc_posixlt_from_int(days)
  mon <- x$mon + 1L

  if (style == "named") {
    month_lookup <- format(ISOdate(2000, 1:12, 1), "%b")
    return(month_lookup[mon])
  } else {
    return(mon)
  }
}


#' @rdname grate_accessors
#' @export
get_quarter <- function(x, ...) {
  UseMethod("get_quarter")
}

#' @rdname grate_accessors
#' @export
get_quarter.default <- function(x, ...) {
  abort(
    sprintf("Not implemented for class %s", paste(class(x), collapse = ", "))
  )
}

#' @rdname grate_accessors
#' @export
get_quarter.grate_quarter <- function(x, ...) {
  attributes(x) <- NULL
  days <- month_to_days(x)
  x <- as_utc_posixlt_from_int(days)
  x$mon %/% 3L +1L
}


#' @rdname grate_accessors
#' @export
get_year <- function(x, ...) {
  UseMethod("get_year")
}

#' @rdname grate_accessors
#' @export
get_year.default <- function(x, ...) {
  abort(
    sprintf("Not implemented for class %s", paste(class(x), collapse = ", "))
  )
}

#' @rdname grate_accessors
#' @export
get_year.grate_yearweek <- function(x, ...) {
  yearweek_to_year(x)
}

#' @rdname grate_accessors
#' @export
get_year.grate_month <- function(x, ...) {
  attributes(x) <- NULL
  days <- month_to_days(x)
  x <- as_utc_posixlt_from_int(days)
  x$year + 1900L
}

#' @rdname grate_accessors
#' @export
get_year.grate_quater <- function(x, ...) {
  attributes(x) <- NULL
  days <- month_to_days(x)
  x <- as_utc_posixlt_from_int(days)
  x$year + 1900L
}

#' @rdname grate_accessors
#' @export
get_year.grate_year <- function(x, ...) {
  unclass(x)
}


#' @rdname grate_accessors
#' @export
get_firstday <- function(x, ...) {
  UseMethod("get_firstday")
}

#' @rdname grate_accessors
#' @export
get_firstday.default <- function(x, ...) {
  abort(
    sprintf("Not implemented for class %s", paste(class(x), collapse = ", "))
  )
}

#' @rdname grate_accessors
#' @export
get_firstday.grate_yearweek <- function(x, ...) {
  attr(x, "firstday")
}

#' @rdname grate_accessors
#' @export
get_interval <- function(x, ...) {
  UseMethod("get_interval")
}

#' @rdname grate_accessors
#' @export
get_interval.default <- function(x, ...) {
  abort(
    sprintf("Not implemented for class %s", paste(class(x), collapse = ", "))
  )
}

#' @rdname grate_accessors
#' @export
get_interval.grate_month <- function(x, ...) {
  attr(x, "interval")
}

#' @rdname grate_accessors
#' @export
get_interval.grate_period <- function(x, ...) {
  attr(x, "interval")
}
