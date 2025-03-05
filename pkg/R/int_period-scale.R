grates_int_period_env <-  new.env(parent = emptyenv())

# -------------------------------------------------------------------------
#' Integer-period scale (Experimental)
#'
# -------------------------------------------------------------------------
#' ggplot2 scale for an integer-period vector.
#'
# -------------------------------------------------------------------------
#' @param breaks
#'
#' A `<grates_int_period>` vector of the desired breaks.
#'
#' @param n.breaks `[integer]`
#'
#' Approximate number of breaks calculated using `scales::breaks_pretty`
#' (default 6L).
#'
#' Will only have an effect if `breaks = waiver()`.
#'
#' @param centre
#'
#' Only applicable to an `int_period` object with `n > 1`.
#'
#' If FALSE labels are place at the edge of the bounds.
#'
#' If TRUE then labels are centralised and of the form `[lower, upper]`
#'
#' @param n `[integer]`
#'
#' Number used for the original grouping.
#'
#' @param ...
#'
#' Not currently used.
#'
# -------------------------------------------------------------------------
#' @return
#'
#' A scale for use with ggplot2.
#'
# -------------------------------------------------------------------------
#' @export
scale_x_grates_int_period <- function(
    ...,
    breaks = ggplot2::waiver(),
    n.breaks = 6L,
    centre = FALSE,
    n
) {
    .check_suggests("ggplot2")
    .check_suggests("scales") # precautionary but overkill as currently a dependency of ggplot2

    if (!(is.logical(centre) && length(centre) == 1L) || anyNA(centre))
        stop("`centre` must be boolean (TRUE/FALSE).")

    if (missing(n))
        n <- grates_int_period_env$n

    if (is.null(n))
        stop("Please specify the `n` of the grate_int_period input")

    if (!is.integer(n)) {
        if (!.is_whole(n))
            stop("`n` must be an integer of length 1.")
        n <- as.integer(n)
    }
    if (n < 1L)
        stop("`n` must be >= 1.")

    # set environment variables to NULL so they don't mess other plots up
    grates_int_period_env$n <- NULL

    # ggplot2 3.5.0 deprecated the `trans` argument in favour of `transform`.
    # We could just force a minimum ggplot2 version and avoid this branching
    # but it's relatively low effort so leaving for now.
    # TODO - revisit.
    if (utils::packageVersion("ggplot2") < '3.5.0') {
        ggplot2::scale_x_continuous(
            trans = .grates_int_period_trans(
                breaks = breaks,
                n.breaks = n.breaks,
                centre = centre,
                n = n
            )
        )
    } else {
        ggplot2::scale_x_continuous(
            transform = .grates_int_period_trans(
                breaks = breaks,
                n.breaks = n.breaks,
                centre = centre,
                n = n
            )
        )
    }

}

# ------------------------------------------------------------------------- #
#' @exportS3Method ggplot2::scale_type
scale_type.grates_int_period <- function(x) {

    # -------------------------------------------------------------------------
    # -------------------------------------------------------------------------
    # TODO - remove this if https://github.com/tidyverse/ggplot2/issues/4705
    #        gets resolved
    if (!"grates" %in% .packages())
        stop("<grates_int_period> object found, but grates package is not attached.\n  Please attach via `library(grates)`.")
    # -------------------------------------------------------------------------
    # -------------------------------------------------------------------------

    grates_int_period_env$n <- attr(x, "n")
    "grates_int_period"
}

# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------------------- INTERNALS ------------------------------ #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
.grates_int_period_trans <- function(breaks, n.breaks, centre, n) {

    shift <- if (isTRUE(centre) || n == 1) 0 else 0.5

    # breaks function
    brks <- function(x) {
        if (inherits(breaks, "waiver")) {
            dat <- scales::breaks_pretty(n.breaks)(as.numeric(x))
            dat <- as.integer(floor(dat))
            dat <- as.numeric(new_int_period(dat, n = n))
        } else {
            dat <- as.numeric(breaks)
        }
        dat - shift
    }

    # format function
    fmt <- function(x) {
        x <- new_int_period(x + shift, n)
        if (isTRUE(centre)) {
            format.grates_int_period(x)
        } else {
            x <- as.integer.grates_int_period(x)
            format(x)
        }
    }

    scales::trans_new(
        "grates_int_period",
        transform = as.numeric,
        inverse = as.numeric,
        breaks = brks,
        format = fmt
    )
}
