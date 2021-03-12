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
