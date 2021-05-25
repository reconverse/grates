#' Grouped date accessors
#'
#' Generics and methods to work with grouped date objects.
#'
#' @param x A grates object.
#' @param ... Not currently used.
#' @return
#'
#'   - `get_year()`: The corresponding year values as integer for <grates_year>
#'     and <greats_quarter> objects.
#'   - `get_quarter()`: The corresponding quarter values as integer for
#'     <greats_quarter> objects.
#'
#'
#' @name grates_accessors
#'
NULL

#' @rdname grates_accessors
#' @export
get_n <- function(x, ...) {
  UseMethod("get_n")
}

#' @rdname grates_accessors
#' @export
get_n.default <- function(x, ...) {
  abort(
    sprintf("Not implemented for class %s", paste(class(x), collapse = ", "))
  )
}

#' @rdname grates_accessors
#' @export
get_n.grates_month <- function(x, ...) {
  attr(x, "n")
}

#' @rdname grates_accessors
#' @export
get_n.grates_period <- function(x, ...) {
  attr(x, "n")
}

#' @rdname grates_accessors
#' @export
get_n.grates_int_period <- function(x, ...) {
  attr(x, "n")
}

#' @rdname grates_accessors
#' @export
get_firstday <- function(x, ...) {
  UseMethod("get_firstday")
}

#' @rdname grates_accessors
#' @export
get_firstday.default <- function(x, ...) {
  abort(
    sprintf("Not implemented for class %s", paste(class(x), collapse = ", "))
  )
}

#' @rdname grates_accessors
#' @export
get_firstday.grates_yearweek <- function(x, ...) {
  attr(x, "firstday")
}

#' @rdname grates_accessors
#' @export
get_week <- function(x, ...) {
  UseMethod("get_week")
}

#' @rdname grates_accessors
#' @export
get_week.default <- function(x, ...) {
  abort(
    sprintf("Not implemented for class %s", paste(class(x), collapse = ", "))
  )
}

#' @rdname grates_accessors
#' @export
get_week.grates_yearweek <- function(x, ...) {
  yearweek_to_week(x)
}


#' @rdname grates_accessors
#' @export
get_quarter <- function(x, ...) {
  UseMethod("get_quarter")
}

#' @rdname grates_accessors
#' @export
get_quarter.default <- function(x, ...) {
  abort(
    sprintf("Not implemented for class %s", paste(class(x), collapse = ", "))
  )
}

#' @rdname grates_accessors
#' @export
get_quarter.grates_quarter <- function(x, ...) {
  tmp <- unclass(x)
  tmp <- month_to_days(3L*x)
  tmp <- as_utc_posixlt_from_int(tmp)
  tmp$mon %/% 3L +1L
}


#' @rdname grates_accessors
#' @export
get_year <- function(x, ...) {
  UseMethod("get_year")
}

#' @rdname grates_accessors
#' @export
get_year.default <- function(x, ...) {
  abort(
    sprintf("Not implemented for class %s", paste(class(x), collapse = ", "))
  )
}

#' @rdname grates_accessors
#' @export
get_year.grates_yearweek <- function(x, ...) {
  yearweek_to_year(x)
}


#' @rdname grates_accessors
#' @export
get_year.grates_quarter <- function(x, ...) {
  tmp <- unclass(x)
  tmp <- month_to_days(3L*x)
  tmp <- as_utc_posixlt_from_int(tmp)
  tmp$year + 1900L
}

#' @rdname grates_accessors
#' @export
get_year.grates_year <- function(x, ...) {
  unclass(x)
}


#' @rdname grates_accessors
#' @export
get_date_range <- function(x, ...) {
  UseMethod("get_date_range")
}

#' @rdname grates_accessors
#' @export
get_date_range.default <- function(x, ...) {
  abort(
    sprintf("Not implemented for class %s", paste(class(x), collapse = ", "))
  )
}

#' @rdname grates_accessors
#' @export
get_date_range.grates_yearweek <- function(x, ...) {
  check_dots_empty()
  get_range(x)
}

#' @rdname grates_accessors
#' @export
get_date_range.grates_month <- function(x, ...) {
  check_dots_empty()
  get_range(x)
}

#' @rdname grates_accessors
#' @export
get_date_range.grates_quarter <- function(x, ...) {
  check_dots_empty()
  get_range(x)
}

#' @rdname grates_accessors
#' @export
get_date_range.grates_period <- function(x, ...) {
  check_dots_empty()
  get_range(x)
}

#' @rdname grates_accessors
#' @export
get_date_range.grates_year <- function(x, ...) {
  check_dots_empty()
  get_range(x)
}

#' @rdname grates_accessors
#' @export
get_date_bounds.grates_int_period <- function(x, ...) {
  c(as.integer(x), as.integer(x + 1) - 1)
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------------------- INTERNALS ------------------------------ #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
get_range <- function(x) {
  c(as.Date(x), as.Date(x + 1) - 1)
}
