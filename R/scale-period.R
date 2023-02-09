grates_period_env <-  new.env(parent = emptyenv())

# -------------------------------------------------------------------------
#' period scale
#'
# -------------------------------------------------------------------------
#' ggplot2 scale for an `<grates_period>` vector.
#'
# -------------------------------------------------------------------------
#' @param n.breaks `[integer]`
#'
#' Approximate number of breaks calculated using `scales::breaks_pretty`
#' (default 6L).
#'
#' @param format
#'
#' Format to use for dates.
#'
#' Value is used by `format.Date()` and can be any input acceptable by that
#' function.
#'
#' @param n `[integer]`
#'
#' Number of days in each period.
#'
#' @param offset `[integer]`
#'
#' Number of days used in original grouping for the offset from the Unix Epoch.
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
scale_x_grates_period <- function(..., n.breaks = 6L, format = "%Y-%m-%d", n, offset) {

    check_suggests("ggplot2")
    check_suggests("scales") # precautionary but overkill as currently a dependency of ggplot2

    if (...length()) {
        dot_names <- names(list(...))
        if (any(dot_names == "origin"))
            stop("The `origin` argument is now defunct. Please use `offset`.")
    }

    if (missing(n))
        n <- grates_period_env$n

    if (is.null(n))
        stop("Please provide a value of `n` corresponding to the given data")

    if (length(n) != 1L)
        stop("`n` first day must be an integer of length 1.")

    if (!is.integer(n)) {
        if (!.is_whole(n))
            stop("`n` must be an integer of length 1.")
        n <- as.integer(n)
    }

    if (missing(offset))
        offset <- grates_period_env$offset

    if (is.null(offset))
        stop("Please provide a value for `offset` corresponding to the given data")

    if (length(offset) != 1L)
        stop("`offset` first day must be an integer of length 1.")

    if (!is.integer(offset)) {
        if (!.is_whole(offset))
            stop("`offset` must be an integer of length 1.")
        offset <- as.integer(offset)
    }

    # set environment variables to NULL so they don't mess other plots up
    grates_period_env$n <- NULL
    grates_period_env$offset <- NULL

    ggplot2::scale_x_continuous(
        trans = .grates_period_trans(
            n.breaks = n.breaks,
            n = n,
            offset = offset,
            format = format
        )
    )
}

# -------------------------------------------------------------------------
scale_type.grates_period <- function(x) {

    # -------------------------------------------------------------------------
    # -------------------------------------------------------------------------
    # TODO - remove this if https://github.com/tidyverse/ggplot2/issues/4705
    #        gets resolved
    if (!"grates" %in% .packages())
        stop("<grates_period> object found, but grates package is not attached.\n  Please attach via `library(grates)`.")
    # -------------------------------------------------------------------------
    # -------------------------------------------------------------------------

    grates_period_env$n <- attr(x, "n")
    grates_period_env$offset <- attr(x, "offset")
    "grates_period"
}

# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------------------- INTERNALS ------------------------------ #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
.grates_period_trans <- function(n.breaks, format, n, offset) {

    shift <- 0.5

    # breaks function
    brks <- function(x) {
        dat <- scales::breaks_pretty(n.breaks)(as.numeric(x))
        dat <- as.integer(floor(dat))
        as.numeric(new_period(dat, n = n, offset = offset)) - shift
    }

    # format function
    fmt <- function(x) {
        x <- new_period(x + shift, n = n, offset = offset)
        x <- as.Date.grates_period(x)
        format(x, format)
    }

    scales::trans_new(
        "grates_period",
        transform = as.numeric,
        inverse = as.numeric,
        breaks = brks,
        format = fmt
    )
}
