# -------------------------------------------------------------------------
#' epiweek scale
#'
# -------------------------------------------------------------------------
#' ggplot2 scale for an `<grates_epiweek>` vector.
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
#' If NULL (default) then labels are in the standard yearweek format (YYYY-Www).
#'
#' If not NULL then the value is used by `format.Date()` and can be any input
#' acceptable by that function.
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
scale_x_grates_epiweek <- function(..., n.breaks = 6L, format = NULL) {

    .check_suggests("ggplot2")
    .check_suggests("scales") # precautionary but overkill as currently a dependency of ggplot2


    ggplot2::scale_x_continuous(
        trans = .grates_epiweek_trans(
            n.breaks = n.breaks,
            format = format
        )
    )
}

# -------------------------------------------------------------------------
scale_type.grates_epiweek <- function(x) {

    # -------------------------------------------------------------------------
    # -------------------------------------------------------------------------
    # TODO - remove this if https://github.com/tidyverse/ggplot2/issues/4705
    #        gets resolved
    if (!"grates" %in% .packages())
        stop("<grates_epiweek> object found, but grates package is not attached.\n  Please attach via `library(grates)`.")
    # -------------------------------------------------------------------------
    # -------------------------------------------------------------------------

    "grates_epiweek"
}

# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------------------- INTERNALS ------------------------------ #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
.grates_epiweek_trans <- function(n.breaks, format) {

    shift <- if (is.null(format)) 0 else 0.5

    # breaks function
    brks <- function(x) {
        dat <- scales::breaks_pretty(n.breaks)(as.numeric(x))
        dat <- as.integer(floor(dat))
        as.numeric(new_epiweek(dat)) - shift
    }

    # format function
    fmt <- function(x) {
        x <- new_epiweek(x + shift)
        if (is.null(format)) {
            format.grates_yearweek(x)
        } else {
            x <- as.Date.grates_epiweek(x)
            format(x, format)
        }
    }

    scales::trans_new(
        "grates_epiweek",
        transform = as.numeric,
        inverse = as.numeric,
        breaks = brks,
        format = fmt
    )
}
