#' Accessors for grate objects
#'
#' Generics and methods for accessing information about grouped date objects.
#'
#' @param x \R object
#' @param ... Not currently used
#'
#' @return
#' Requested value or an error if no method available.
#'
#' @examples
#' dates <- as.Date("2020-01-01") + 1:14
#' dat <- as_isoweek(dates)
#' get_week(dat)
#' get_year(dat)
#'
#' @name grouped_date_accessors
NULL

# -------------------------------------------------------------------------
#' @rdname grouped_date_accessors
#' @export
get_firstday <- function(x, ...) {
    UseMethod("get_firstday")
}

# -------------------------------------------------------------------------
#' @rdname grouped_date_accessors
#' @export
get_firstday.default <- function(x, ...) {
    stopf("Not implemented for class [%s].", toString(class(x)))
}

# -------------------------------------------------------------------------
#' @rdname grouped_date_accessors
#' @export
get_firstday.grates_yearweek_monday <- function(x, ...) {1L}

# -------------------------------------------------------------------------
#' @rdname grouped_date_accessors
#' @export
get_firstday.grates_yearweek_tuesday <- function(x, ...) {2L}

# -------------------------------------------------------------------------
#' @rdname grouped_date_accessors
#' @export
get_firstday.grates_yearweek_wednesday <- function(x, ...) {3L}

# -------------------------------------------------------------------------
#' @rdname grouped_date_accessors
#' @export
get_firstday.grates_yearweek_thursday <- function(x, ...) {4L}

# -------------------------------------------------------------------------
#' @rdname grouped_date_accessors
#' @export
get_firstday.grates_yearweek_friday <- function(x, ...) {5L}

# -------------------------------------------------------------------------
#' @rdname grouped_date_accessors
#' @export
get_firstday.grates_yearweek_saturday <- function(x, ...) {6L}

# -------------------------------------------------------------------------
#' @rdname grouped_date_accessors
#' @export
get_firstday.grates_yearweek_sunday <- function(x, ...) {7L}

# -------------------------------------------------------------------------
#' @name grouped_date_accessors
#' @export
get_week <- function(x, ...) {
    UseMethod("get_week")
}

# -------------------------------------------------------------------------
#' @rdname grouped_date_accessors
#' @export
get_week.default <- function(x, ...) {
    stopf("Not implemented for class [%s].", toString(class(x)))
}

# -------------------------------------------------------------------------
#' @rdname grouped_date_accessors
#' @export
get_week.grates_yearweek <- function(x, ...) {
    midweek <- as.Date(x) + 3L
    .seven_day_week_in_year(date = midweek)
}

# -------------------------------------------------------------------------
#' @rdname grouped_date_accessors
#' @export
get_week.grates_epiweek <- get_week.grates_yearweek

# -------------------------------------------------------------------------
#' @rdname grouped_date_accessors
#' @export
get_week.grates_isoweek <- get_week.grates_yearweek


# -------------------------------------------------------------------------
#' @name grouped_date_accessors
#' @export
get_year <- function(x, ...) {
    UseMethod("get_year")
}

# -------------------------------------------------------------------------
#' @rdname grouped_date_accessors
#' @export
get_year.default <- function(x, ...) {
    stopf("Not implemented for class [%s].", toString(class(x)))
}

# -------------------------------------------------------------------------
#' @rdname grouped_date_accessors
#' @export
get_year.grates_yearweek <- function(x, ...) {
    week <- get_week.grates_yearweek(x)
    dat <- .as_utc_posixlt_from_int(as.Date(x))
    december <- dat$mon == 11L
    january <- dat$mon == 0L
    boundary_adjustment <- integer(length(x)) # h/t Zhian Kamvar for boundary adjustment idea in aweek)
    boundary_adjustment[january  & week >= 52L] <- -1L
    boundary_adjustment[december & week == 1L]  <- 1L
    yr <- dat$year + 1900L
    yr + boundary_adjustment
}

# -------------------------------------------------------------------------
#' @rdname grouped_date_accessors
#' @export
get_year.grates_epiweek <- get_year.grates_yearweek

# -------------------------------------------------------------------------
#' @rdname grouped_date_accessors
#' @export
get_year.grates_isoweek <- get_year.grates_yearweek

# -------------------------------------------------------------------------
#' @rdname grouped_date_accessors
#' @export
get_year.grates_yearmonth <- function(x, ...) {
    x <- as.POSIXlt(x)
    x$year + 1900L
}

# -------------------------------------------------------------------------
#' @rdname grouped_date_accessors
#' @export
get_year.grates_yearquarter <- function(x, ...) {
    x <- as.POSIXlt(x)
    x$year + 1900L
}

# -------------------------------------------------------------------------
#' @rdname grouped_date_accessors
#' @export
get_year.grates_year <- function(x, ...) {
    unclass(x)
}

# -------------------------------------------------------------------------
#' @name grouped_date_accessors
#' @export
get_n <- function(x, ...) {
    UseMethod("get_n")
}

# -------------------------------------------------------------------------
#' @rdname grouped_date_accessors
#' @export
get_n.default <- function(x, ...) {
    stopf("Not implemented for class [%s].", toString(class(x)))
}

# -------------------------------------------------------------------------
#' @rdname grouped_date_accessors
#' @export
get_n.grates_month <- function(x, ...) {
    attr(x, "n")
}

# -------------------------------------------------------------------------
#' @rdname grouped_date_accessors
#' @export
get_n.grates_period <- function(x, ...) {
    attr(x, "n")
}

# -------------------------------------------------------------------------
#' @name grouped_date_accessors
#' @export
get_offset <- function(x, ...) {
    UseMethod("get_offset")
}

# -------------------------------------------------------------------------
#' @rdname grouped_date_accessors
#' @export
get_offset.default <- function(x, ...) {
    stopf("Not implemented for class [%s].", toString(class(x)))
}

# -------------------------------------------------------------------------
#' @rdname grouped_date_accessors
#' @export
get_offset.grates_period <- function(x, ...) {
    attr(x, "offset")
}
