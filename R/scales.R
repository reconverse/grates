#' Scales for ggplot2
#'
#' ggplot2 scales grouped date objects; `yrwk`, `yrmon`, `yrqtr` and `period`.
#'
#' @param n Approximate number of breaks calculated using
#'   `scales::breaks_pretty` (default 5).
#' @param ... Other arguments passed to [`ggplot2::scale_x_continuous()`].
#'
#' @name grate-scales
NULL


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# --------------------------------- YRWK ---------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

# When adding a geom, ggplot2 will look for a relevant method to the generic
# scale_type.  It will then dispatch based on the output of this function to
# one of scale_x_yrwk_fd_xxx defined below.
# I think this is a cleaner approach then the one we are forced to employ with
# the `period` class.

scale_type.yrwk <- function(x) {
  fd <- attr(x, "firstday")
  sprintf("yrwk_fd_%d", fd)
}

yrwk_trans <- function(n = 5, firstday) {

  # breaks function
  brks <- function(x) scales::breaks_pretty(n)(as.numeric(x))

  # format function
  fmt <- function(x) {
    attr(x, "firstday") <- firstday
    format.yrwk(x)
  }

  scales::trans_new(
    "yrwk",
    transform = as.numeric,
    inverse = as.numeric,
    breaks = brks,
    format = fmt
  )
}


#' @param firstday Integer value of the first weekday: 1 (Monday) to 7
#'   (Sunday).
#' @rdname grate-scales
#' @export
scale_x_yrwk <- function(..., n = 5, firstday) {

  # check ggplot2 is installed (this also ensures scales presence)
  check_suggests("ggplot2")

  if (missing(firstday)) {
    stop(
      "Please provide a value of `firstday` corresponding to the week in the given data",
      call. = FALSE
    )
  }

  if (!is.wholenumber(firstday) || firstday < 1 || firstday > 7) {
    stop("`x` must be a whole number between 1 and 7 (inclusive)", call. = FALSE)
  }

  ggplot2::scale_x_continuous(..., trans = yrwk_trans(n = n, firstday = firstday))


}



#' Hidden scales
#'
#' Wrappers around [scale_x_yrwk()] with pre-specified firstday argument.
#'
#' @inheritParams grate-scales
#'
#' @name hidden-scales
NULL

#' @export
#' @keywords internal
#' @rdname hidden-scales
scale_x_yrwk_fd_1 <- function(..., n = 5) scale_x_yrwk(..., n = n, firstday = 1)

#' @export
#' @keywords internal
#' @rdname hidden-scales
scale_x_yrwk_fd_2 <- function(..., n = 5) scale_x_yrwk(..., n = n, firstday = 2)

#' @export
#' @keywords internal
#' @rdname hidden-scales
scale_x_yrwk_fd_3 <- function(..., n = 5) scale_x_yrwk(..., n = n, firstday = 3)

#' @export
#' @keywords internal
#' @rdname hidden-scales
scale_x_yrwk_fd_4 <- function(..., n = 5) scale_x_yrwk(..., n = n, firstday = 4)

#' @export
#' @keywords internal
#' @rdname hidden-scales
scale_x_yrwk_fd_5 <- function(..., n = 5) scale_x_yrwk(..., n = n, firstday = 5)

#' @export
#' @keywords internal
#' @rdname hidden-scales
scale_x_yrwk_fd_6 <- function(..., n = 5) scale_x_yrwk(..., n = n, firstday = 6)

#' @export
#' @keywords internal
#' @rdname hidden-scales
scale_x_yrwk_fd_7 <- function(..., n = 5) scale_x_yrwk(..., n = n, firstday = 7)


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# --------------------------------- YRMON --------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #


scale_type.yrmon <- function(x) c("yrmon")

yrmon_trans <- function(n = 5) {

  # breaks function
  brks <- function(x) scales::breaks_pretty(n)(as.numeric(x))

  # format function
  fmt <- function(x) format.yrmon(new_yrmon(x))

  scales::trans_new(
    "yrmon",
    transform = as.numeric,
    inverse = as.numeric,
    breaks = brks,
    format = fmt
  )
}


#' @rdname grate-scales
#' @export
scale_x_yrmon <- function(..., n = 5) {

  # check ggplot2 is installed (this also ensures scales presence)
  check_suggests("ggplot2")

  ggplot2::scale_x_continuous(..., trans = yrmon_trans(n))

}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# --------------------------------- YRQTR --------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

scale_type.yrqtr <- function(x) "yrqtr"

yrqtr_trans <- function(n = 5) {

  # breaks function
  brks <- function(x) scales::pretty_breaks(n)(as.numeric(x))

  # format function
  fmt <- function(x) format.yrqtr(new_yrqtr(x))

  scales::trans_new(
    "yrqtr",
    transform = as.numeric,
    inverse = as.numeric,
    breaks = brks,
    format = fmt
  )

}

#' @rdname grate-scales
#' @export
scale_x_yrqtr <- function(..., n = 5) {

  # probably a little pointless but you never know
  check_suggests("ggplot2")

  ggplot2::scale_x_continuous(..., trans = yrqtr_trans(n))
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ---------------------------------- YR ----------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #


scale_type.yr <- function(x) "yr"


yr_trans <- function(n = 5) {

  # breaks function
  brks <- function(x) scales::pretty_breaks(n)(as.integer(x))

  # format function
  fmt <- function(x) format.yr(new_yr(x))

  scales::trans_new(
    "yr",
    transform = as.numeric,
    inverse = as.numeric,
    breaks = brks,
    format = fmt
  )

}


#' @rdname grate-scales
#' @export
scale_x_yr <- function(..., n = 5) {

  # probably a little pointless but you never know
  check_suggests("ggplot2")

  ggplot2::scale_x_continuous(..., trans = yr_trans(n))
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# --------------------------------- PERIOD -------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

# DETAILS
# It is hard to get attributes from the data within `scale_x_period` so we have
# come up with the following approach:
#   1)  Create an environment within the package called `scale_env`.
#   2)  When we dispatch to a geom we use the `scale_type` to generic to set any
#       attributes that we may need in `scale_x_period` (i.e. `firstdate` and
#       `interval`).
#   3)  Within `period_trans`, once these environment variables have been used
#       we must then set them to NULL.  This ensures that they will only be used
#       for the geom that called the scale_type generic.
#   4)  If scale_x_period is called by the user then they must specify values
#       for the `interval` and the `firstdate`.  This is done by checking
#       whether the scale_env variables are missing and then conditioning on
#       whether they are NULL.

scale_env <-  new.env(parent = emptyenv())

scale_type.period <- function(x) {
  scale_env$firstdate <- as.Date(min(x, na.rm = TRUE))
  scale_env$interval <- attr(x, "interval")
  "period"
}

period_trans_integer <- function(n = 5, interval, firstdate) {

  # transform function
  trans <- new_date

  # inverse function
  inv <- as.numeric

  # breaks function
  brks <- function(x) {
    dat <- trunc(scales::pretty_breaks(n)(new_date(x)))
    m <- min(dat, na.rm = TRUE)
    if (m < firstdate) {
      firstdate <- m - (interval - ((as.numeric(firstdate) - as.numeric(m)) %% interval))
    }
    tmp <- as_period(dat, interval = interval, firstdate = new_date(firstdate))
    as.Date(tmp) - interval / 2
  }

  # format function
  fmt <- function(x) {
    format(new_date(round(x + interval / 2)))
  }

  # set environment variables to NULL so they don't mess other plots up
  scale_env$firstdate <- NULL
  scale_env$interval <- NULL

  scales::trans_new(
    "period",
    transform = trans,
    inverse = inv,
    breaks = brks,
    format = fmt
  )
}


period_trans_day <- function(n = 5, interval, firstdate) {
  interval <- get_interval_number(interval)
  period_trans_integer(n = n, interval = interval, firstdate = firstdate)
}


period_trans_week <- function(n = 5, firstday, scale) {

  # transform function
  trans <- function(x) (as.numeric(x) + 4 - firstday) %/% 7

  # inverse function
  inv <- function(x) x * 7 + firstday - 4

  # breaks function
  brks <- function(x) scales::pretty_breaks(n)(new_date(x)) - scale * 7 / 2

  # format function
  fmt <- function(x) format(round(new_date(x + scale * 7 / 2)))

  scales::trans_new(
    "period",
    transform = trans,
    inverse = inv,
    breaks = brks,
    format = fmt
  )
}


period_trans_month <- function(n = 5, interval, firstdate) {

  # transform function
  trans <- new_date

  # inverse function
  inv <- as.numeric

  # breaks function
  brks <- function(x) {
    dat <- scales::pretty_breaks(n)(new_date(x))
    m <- min(dat, na.rm = TRUE)
    while (m < unclass(firstdate)) {
      firstdate <- as_period(new_date(firstdate), interval, new_date(firstdate)) - 1
    }
    tmp <- as_period(new_date(dat), interval = interval, firstdate = new_date(firstdate))
    as.Date(tmp) - get_interval_days(tmp - 1, interval) / 2
  }

  # format function
  fmt <- function(x) {
    tmp <- new_date(x + get_interval_days(x, interval) / 2 + 1) # +1 slightly hacky
    format(as.Date(as_yrmon(tmp)))
  }

  # set environment variables to NULL so they don't mess other plots up
  scale_env$firstdate <- NULL
  scale_env$interval <- NULL

  scales::trans_new(
    "period",
    transform = trans,
    inverse = inv,
    breaks = brks,
    format = fmt
  )
}


period_trans_quarter <- function(n = 5, interval, firstdate) {

  # transform function
  trans <- new_date

  # inverse function
  inv <- as.numeric

  # breaks function
  brks <- function(x) {
    dat <- scales::pretty_breaks(n)(new_date(x))
    m <- min(dat, na.rm = TRUE)
    while (m < unclass(firstdate)) {
      firstdate <- as_period(new_date(firstdate), interval, new_date(firstdate)) - 1
    }
    tmp <- as_period(new_date(dat), interval = interval, firstdate = new_date(firstdate))
    as.Date(tmp) - get_interval_days(tmp - 1, interval) / 2
  }

  # format function
  fmt <- function(x) {
    tmp <- new_date(x + get_interval_days(x, interval) / 2 + 1) # +1 slightly hacky
    format(as.Date(as_yrqtr(tmp)))
  }

  # set environment variables to NULL so they don't mess other plots up
  scale_env$firstdate <- NULL
  scale_env$interval <- NULL

  scales::trans_new(
    "period",
    transform = trans,
    inverse = inv,
    breaks = brks,
    format = fmt
  )
}


period_trans_year <- function(n = 5, interval, firstdate) {

  # transform function
  trans <- new_date

  # inverse function
  inv <- as.numeric

  # breaks function
  brks <- function(x) {
    dat <- scales::pretty_breaks(n)(new_date(x))
    m <- min(dat, na.rm = TRUE)
    while (m < unclass(firstdate)) {
      firstdate <- as_period(new_date(firstdate), interval, new_date(firstdate)) - 1
    }
    tmp <- as_period(new_date(dat), interval = interval, firstdate = new_date(firstdate))
    as.Date(tmp) - get_interval_days(tmp - 1, interval) / 2
  }

  # format function
  fmt <- function(x) {
    tmp <- new_date(x + get_interval_days(x, interval) / 2 + 1) # +1 slightly hacky
    format(as.Date(as_yr(tmp)))
  }

  # set environment variables to NULL so they don't mess other plots up
  scale_env$firstdate <- NULL
  scale_env$interval <- NULL

  scales::trans_new(
    "period",
    transform = trans,
    inverse = inv,
    breaks = brks,
    format = fmt
  )
}


period_trans_general <- function(n = 5, interval, firstdate) {

  # transform function
  trans <- new_date

  # inverse function
  inv <- as.numeric

  # breaks function
  brks <- function(x) {
    dat <- scales::pretty_breaks(n)(new_date(x))
    m <- min(dat, na.rm = TRUE)
    while (m < unclass(firstdate)) {
      firstdate <- as_period(new_date(firstdate), interval, new_date(firstdate)) - 1
    }
    tmp <- as_period(new_date(dat), interval = interval, firstdate = new_date(firstdate))
    as.Date(tmp) - get_interval_days(tmp - 1, interval) / 2
  }

  # format function
  fmt <- function(x) {
    format(new_date(x + get_interval_days(x, interval) / 2))

  }

  # set environment variables to NULL so they don't mess other plots up
  scale_env$firstdate <- NULL
  scale_env$interval <- NULL

  scales::trans_new(
    "period",
    transform = trans,
    inverse = inv,
    breaks = brks,
    format = fmt
  )
}


#' @param interval interval ga
#' @param firstdate date the period is anchored / started from
#' @rdname grate-scales
#' @export
scale_x_period <- function(..., n = 5, interval, firstdate) {

  # probably a little pointless but you never know
  check_suggests("ggplot2")

  if (missing(interval)) {
    interval <- scale_env$interval
    if (is.null(interval)) {
      stop("Please specify the `interval` of the period data", call. = FALSE)
    }
  }

  if (missing(firstdate)) {
    firstdate <- scale_env$firstdate
    if (is.null(firstdate)) {
      stop("Please specify the `firstdate` used for the period data", call. = FALSE)
    }
  }

  if (is.numeric(interval)) {
    ggplot2::scale_x_continuous(..., trans = period_trans_integer(n, interval, firstdate))
  } else {
    type = get_interval_type(interval)
    n_int <- get_interval_number(interval)
    firstday = as_utc_posixlt_from_int(firstdate)$wday
    firstday = firstday <- 1L + (firstday + 6L) %% 7
    switch(
      type,
      day = {
        ggplot2::scale_x_continuous(
          ...,
          trans = period_trans_day(n, interval, firstdate)
        )
      },
      week = {
        ggplot2::scale_x_continuous(
          ...,
          trans = period_trans_week(n = n, firstday = firstday, scale = n_int)
        )
      },
      month = {
        ggplot2::scale_x_continuous(
          ...,
          trans = period_trans_month(n, interval, firstdate)
        )
      },
      quarter = {
        ggplot2::scale_x_continuous(
          ...,
          trans = period_trans_quarter(n, interval, firstdate)
        )
      },
      year = {
        ggplot2::scale_x_continuous(
          ...,
          trans = period_trans_year(n, interval, firstdate)
        )
      },
      ggplot2::scale_x_continuous(
        ...,
        trans = period_trans_general(n, interval, firstdate)
      )
    )
  }
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# --------------------------------- PERIOD -------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
scale_type.int_period <- function(x) {
  scale_env$int_firstdate <- min(x, na.rm = TRUE)
  scale_env$int_interval <- attr(x, "interval")
  "int_period"
}

int_period_trans <- function(n = 5, interval, firstdate) {

  # transform function
  trans <- as.integer

  # inverse function
  inv <- as.integer

  # breaks function
  brks <- function(x) {
    dat <- trunc(scales::pretty_breaks(n)(as.integer(x)))
    m <- min(dat, na.rm = TRUE)
    if (m < firstdate) {
      firstdate <- m - (interval - ((as.numeric(firstdate) - as.numeric(m)) %% interval))
    }
    as_int_period(dat, interval = interval, firstdate = firstdate)
  }

  # format function
  fmt <- function(x) {
    format(as_int_period(x, interval, firstdate))
  }

  # set environment variables to NULL so they don't mess other plots up
  scale_env$int_firstdate <- NULL
  scale_env$int_interval <- NULL

  scales::trans_new(
    "int_period",
    transform = trans,
    inverse = inv,
    breaks = brks,
    format = fmt
  )
}


#' @param interval interval ga
#' @param firstdate date the period is anchored / started from
#' @rdname grate-scales
#' @export
scale_x_int_period <- function(..., n = 5, interval, firstdate) {

  # probably a little pointless but you never know
  check_suggests("ggplot2")

  if (missing(interval)) {
    interval <- scale_env$int_interval
    if (is.null(interval)) {
      stop("Please specify the `interval` of the period data", call. = FALSE)
    }
  }

  if (missing(firstdate)) {
    firstdate <- scale_env$int_firstdate
    if (is.null(firstdate)) {
      stop("Please specify the `firstdate` used for the period data", call. = FALSE)
    }
  }

  ggplot2::scale_x_continuous(..., trans = int_period_trans(n, interval, firstdate))


}

