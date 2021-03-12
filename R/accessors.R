#' Grouped date accessors
#'
#' Generics and methods to work with grouped date objects:
#' - `get_week()` returns the corresponding week values for a yrwk vector.
#' - `get_year()` returns the year.
#' - `get_firstday()` returns the firstday attribute of a yrwk object.
#' - `get_month()` returns the month.
#'
#' @param x A yrwk, yrmon, yrqtr or period object.
#' @param ... Not used.
#' @param days Should periods be converted in to a number of days.
#'
#' @name grate_accessors
#'
#' @examples
#' x <- as_yrwk(Sys.Date())
#' get_year(x)
#' get_week(x)
#' get_firstday(x)
NULL

#' @rdname grate_accessors
#' @export
get_week <- function(x, ...) {
  UseMethod("get_week")
}

#' @rdname grate_accessors
#' @export
get_week.default <- function(x, ...) {
  stop(sprintf("Not implemented for class %s",
               paste(class(x), collapse = ", ")))
}

#' @rdname grate_accessors
#' @export
get_week.yrwk <- function(x, ...) {
  yrwk_to_week(x)
}


#' @rdname grate_accessors
#' @export
get_month <- function(x, ...) {
  UseMethod("get_month")
}

#' @rdname grate_accessors
#' @export
get_month.default <- function(x, ...) {
  stop(sprintf("Not implemented for class %s",
               paste(class(x), collapse = ", ")))
}

#' @param style Either "numeric" (default) for the integer month value or
#'   "named" to return the abbreviated month name in the current locale.
#'
#' @rdname grate_accessors
#' @export
get_month.yrmon <- function(x, style = c("numeric", "named"), ...) {
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
  stop(sprintf("Not implemented for class %s",
               paste(class(x), collapse = ", ")))
}

#' @rdname grate_accessors
#' @export
get_quarter.yrqtr <- function(x, ...) {
  attributes(x) <- NULL
  days <- month_to_days(x * 3)
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
  stop(sprintf("Not implemented for class %s",
               paste(class(x), collapse = ", ")))
}

#' @rdname grate_accessors
#' @export
get_year.yrwk <- function(x, ...) {
  yrwk_to_year(x)
}

#' @rdname grate_accessors
#' @export
get_year.yrmon <- function(x, ...) {
  attributes(x) <- NULL
  days <- month_to_days(x)
  x <- as_utc_posixlt_from_int(days)
  x$year + 1900L
}

#' @rdname grate_accessors
#' @export
get_year.yrqtr <- function(x, ...) {
  attributes(x) <- NULL
  days <- month_to_days(x * 3)
  x <- as_utc_posixlt_from_int(days)
  x$year + 1900L
}

#' @rdname grate_accessors
#' @export
get_year.yr <- function(x, ...) {
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
  stop(sprintf("Not implemented for class %s",
               paste(class(x), collapse = ", ")))
}

#' @rdname grate_accessors
#' @export
get_firstday.yrwk <- function(x, ...) {
  attr(x, "firstday")
}


#' #' @rdname grate_accessors
#' #' @export
#' get_interval.period <- function(x, days = FALSE, ...) {
#'   res <- attr(x, "interval")
#'   if (days) {
#'     res <- get_interval_days(x, attr(x, "interval"))
#'   }
#'   res
#' }
#'
#' #' @rdname grate_accessors
#' #' @export
#' get_interval.int_period <- function(x, days = FALSE, ...) {
#'   res <- attr(x, "interval")
#'   if (days) {
#'     res <- get_interval_days(x, attr(x, "interval"))
#'   }
#'   res
#' }

#' @rdname grate_accessors
#' @export
get_interval.yrwk <- function(x, days = FALSE, ...) {
  res <- sprintf("yearweek (firstday = %d)", get_firstday(x))
  if (days) {
    res <- 7L
  }
  res
}


#' @rdname grate_accessors
#' @export
get_interval.yrmon <- function(x, days = FALSE, ...) {
  res <- "1 month"
  if (days) {
    year <- get_year(x)
    month <- get_month(x)
    res <- days_in_month(year, month)
  }
  res
}


#' @rdname grate_accessors
#' @export
get_interval.yrqtr <- function(x, days = FALSE, ...) {
  res <- "1 quarter"
  if (days) {
    year <- get_year(x)
    quarter <- get_quarter(x)
    res <- days_in_quarter(year, quarter)
  }
  res
}

#' @rdname grate_accessors
#' @export
get_interval.yr <- function(x, days = FALSE, ...) {
  res <- "1 year"
  if (days) {
    year <- unclass(x)
    res <- 365 + is_leap_year(year)
  }
  res
}
#'
#'
#' #' @rdname grate_accessors
#' #' @export
#' get_firstdate <- function(x, ...) {
#'   UseMethod("get_firstdate")
#' }
#'
#' #' @rdname grate_accessors
#' #' @export
#' get_firstdate.default <- function(x, ...) {
#'   stop(sprintf("Not implemented for class %s",
#'                paste(class(x), collapse = ", ")))
#' }
#'
#' #' @rdname grate_accessors
#' #' @export
#' get_firstdate.period <- function(x, ...) {
#'   new_date(attr(x, "firstdate"))
#' }
#'
#' #' @rdname grate_accessors
#' #' @export
#' get_firstdate.int_period <- function(x, ...) {
#'   attr(x, "firstdate")
#' }


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
  inherits(x, "grate")
}

#' @rdname is_grate
#' @export
is_yrwk <- function(x) {
  inherits(x, "yrwk")
}

#' @rdname is_grate
#' @export
is_yrmon <- function(x) {
  inherits(x, "yrmon")
}

#' @rdname is_grate
#' @export
is_yrqtr <- function(x) {
  inherits(x, "yrqtr")
}

#' @rdname is_grate
#' @export
is_yr <- function(x) {
  inherits(x, "yr")
}

#' @rdname is_grate
#' @export
is_period <- function(x) {
  inherits(x, "period")
}

#' @rdname is_grate
#' @export
is_int_period <- function(x) {
  inherits(x, "int_period")
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ----------------------------- INTERNALS --------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
#
# get_interval_number <- function(x) {
#   if (!grepl("^\\d", x)) return(1L)
#   as.integer(gsub("^(\\d*).+$", "\\1", x))
# }
#
#
# get_interval_type <- function(x) {
#
#   if (!is.character(x)) {
#     return(typeof(x))
#   }
#
#   day <- "^\\s*days?\\s*$|\\sdays?\\s+|\\sdays?\\s*$"
#   if (grepl(day, x, ignore.case = TRUE)) {
#     return("day")
#   } else if (grepl("week", x, ignore.case = TRUE)) {
#     return("week")
#   }  else if (grepl("month", x, ignore.case = TRUE)) {
#     return("month")
#   } else if (grepl("quarter", x, ignore.case = TRUE)) {
#     return("quarter")
#   } else if (grepl("year", x, ignore.case = TRUE)) {
#     return("year")
#   }  else {
#     return("day")
#   }
# }
#
#
# get_days <- function(x, interval) {
#   tmp <- rep(NA, length(x))
#   tmp[!is.na(x)] <- vapply(
#     x[!is.na(x)],
#     function(y) seq.Date(new_date(y), by = interval, length.out = 2)[2],
#     double(1)
#   )
#   tmp
# }
#
#
# get_interval_days <- function(x, interval) {
#   if (is.integer(interval)) {
#     res <- interval
#   } else {
#     n <- get_interval_number(interval)
#     type <- get_interval_type(interval)
#     res <- switch(
#       type,
#       day = 1L * n,
#       week = 7L * n,
#       get_days(x, interval) - unclass(x)
#     )
#   }
#   res
# }
#
#
#
