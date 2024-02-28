# -------------------------------------------------------------------------
#' epiweek scale
#'
# -------------------------------------------------------------------------
#' ggplot2 scale for an `<grates_epiweek>` vector.
#'
# -------------------------------------------------------------------------
#' @param breaks
#'
#' A `<grates_epiweek>` vector of the desired breaks.
#'
#' @param n.breaks `[integer]`
#'
#' Approximate number of breaks calculated using `scales::breaks_pretty`
#' (default 6L).
#'
#' Will only have an effect if `breaks = waiver()`.
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
scale_x_grates_epiweek <- function(..., breaks = ggplot2::waiver(), n.breaks = 6L, format = NULL) {

    .check_suggests("ggplot2")
    .check_suggests("scales") # precautionary but overkill as currently a dependency of ggplot2


    # ggplot2 3.5.0 deprecated the `trans` argument in favour of `transform`.
    # We could just force a minimum ggplot2 version and avoid this branching
    # but it's relatively low effort so leaving for now.
    # TODO - revisit.
    if (utils::packageVersion("ggplot2") < '3.5.0') {
        ggplot2::scale_x_continuous(
            trans = .grates_epiweek_trans(
                breaks = breaks,
                n.breaks = n.breaks,
                format = format
            )
        )
    } else {
        ggplot2::scale_x_continuous(
            transform = .grates_epiweek_trans(
                breaks = breaks,
                n.breaks = n.breaks,
                format = format
            )
        )
    }
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
.grates_epiweek_trans <- function(breaks, n.breaks, format) {

    shift <- if (is.null(format)) 0 else 0.5

    # breaks function
    brks <- function(x) {
        if (!inherits(breaks, "waiver")) {
            dat <- as.numeric(breaks)
        } else {
            dat <- scales::breaks_pretty(n.breaks)(as.numeric(x))
            dat <- as.integer(floor(dat))
            dat <- as.numeric(new_isoweek(dat))
        }
        dat - shift
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
