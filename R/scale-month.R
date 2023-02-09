grates_month_env <-  new.env(parent = emptyenv())

# -------------------------------------------------------------------------
#' month scale
#'
# -------------------------------------------------------------------------
#' ggplot2 scale for a month vector.
#'
# -------------------------------------------------------------------------
#' @param n.breaks `[integer]`
#'
#' Approximate number of breaks calculated using `scales::breaks_pretty`
#' (default 6L).
#'
#' @param format
#'
#' Format to use if "Date" scales are required.
#'
#' If NULL then labels are centralised and of the form "lower category bound to
#' upper category bound".
#'
#' If not NULL then the value is used by `format.Date()` and can be any input
#' acceptable by that function (defaults to "%Y-%m-%d).
#'
#' @param bounds_format
#'
#' Format to use for grouped date labels. Only used if `format` is NULL.
#'
#' @param sep `[character]`
#'
#' Separator to use for grouped date labels.
#'
#' @param n `[integer]`
#'
#' Number of months used for the original grouping.
#'
#' @param ...
#'
#' Not currently used.
#'
# -------------------------------------------------------------------------
#' @return
#' A scale for use with ggplot2.
#'
# -------------------------------------------------------------------------
#' @export
scale_x_grates_month <- function(
        ...,
        n.breaks = 6L,
        format = "%Y-%m-%d",
        bounds_format = "%Y-%b",
        sep = "to",
        n
) {

    check_suggests("ggplot2")
    check_suggests("scales") # precautionary but overkill as currently a dependency of ggplot2

    if (missing(n))
        n <- grates_month_env$n

    if (is.null(n))
        stop("Please specify the `n` of the grate_month input")

    if (!is.integer(n)) {
        if (!.is_whole(n))
            stop("`n` must be an integer of length 1.")
        n <- as.integer(n)
    }
    if (n <= 1L)
        stop("`n` must be greater than 1. Did you mean to call scale_x_grates_yearmonth?")

    # set environment variables to NULL so they don't mess other plots up
    grates_month_env$n <- NULL

    ggplot2::scale_x_continuous(
        trans = .grates_month_trans(
            n.breaks = n.breaks,
            format = format,
            bounds_format = bounds_format,
            sep = sep,
            n = n
        )
    )
}

# ------------------------------------------------------------------------- #
scale_type.grates_month <- function(x) {

    # -------------------------------------------------------------------------
    # -------------------------------------------------------------------------
    # TODO - remove this if https://github.com/tidyverse/ggplot2/issues/4705
    #        gets resolved
    if (!"grates" %in% .packages())
        stop("<grates_month> object found, but grates package is not attached.\n  Please attach via `library(grates)`.")
    # -------------------------------------------------------------------------
    # -------------------------------------------------------------------------

    grates_month_env$n <- attr(x, "n")
    "grates_month"
}

# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------------------- INTERNALS ------------------------------ #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
.grates_month_trans <- function(n.breaks, format, bounds_format, sep, n) {

    shift <- if (is.null(format)) 0 else 0.5

    # breaks function
    brks <- function(x) {
        dat <- scales::breaks_pretty(n.breaks)(as.numeric(x))
        dat <- as.integer(floor(dat))
        as.numeric(new_month(dat, n = n)) - shift
    }

    # format function
    fmt <- function(x) {
        x <- new_month(x + shift, n)
        if (is.null(format)) {
            format.grates_month(x, format = bounds_format, sep = sep)
        } else {
            x <- as.Date.grates_month(x)
            format(x, format)
        }
    }

    scales::trans_new(
        "grates_month",
        transform = as.numeric,
        inverse = as.numeric,
        breaks = brks,
        format = fmt
    )
}
