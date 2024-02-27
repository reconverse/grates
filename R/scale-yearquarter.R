# -------------------------------------------------------------------------
#' yearquarter scale
#'
# -------------------------------------------------------------------------
#' ggplot2 scale for a yearquarter vector.
#'
# -------------------------------------------------------------------------
#' @param breaks
#'
#' A `<grates_yearquarter>` vector of the desired breaks.
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
scale_x_grates_yearquarter <- function(..., breaks = ggplot2::waiver(), n.breaks = 6L, format = NULL) {

    .check_suggests("ggplot2")
    .check_suggests("scales") # precautionary but overkill as currently a dependency of ggplot2

    suppressWarnings(
        ggplot2::scale_x_continuous(
            trans = .grates_yearquarter_trans(
                breaks = breaks,
                n.breaks = n.breaks,
                format = format
            )
        ),
        classes = "lifecycle_warning_deprecated"
    )
}


# -------------------------------------------------------------------------
scale_type.grates_yearquarter <- function(x) {

    # -------------------------------------------------------------------------
    # -------------------------------------------------------------------------
    # TODO - remove this if https://github.com/tidyverse/ggplot2/issues/4705
    #        gets resolved
    if (!"grates" %in% .packages())
        stop("<grates_yearquarter> object found, but grates package is not attached.\n  Please attach via `library(grates)`.")
    # -------------------------------------------------------------------------
    # -------------------------------------------------------------------------

    "grates_yearquarter"

}

# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------------------- INTERNALS ------------------------------ #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

.grates_yearquarter_trans <- function(breaks, n.breaks, format) {

    shift <- if (is.null(format)) 0 else 0.5

    # breaks function
    brks <- function(x) {
        if (!inherits(breaks,"waiver")) {
            dat <- as.numeric(breaks)
        } else {
            dat <- scales::breaks_pretty(n.breaks)(as.numeric(x))
            dat <- as.integer(floor(dat))
            dat <- as.numeric(new_yearquarter(dat))
        }
        dat - shift
    }

    # format function
    fmt <- function(x) {
        x <- new_yearquarter(x + shift)
        if (is.null(format)) {
            format.grates_yearquarter(x)
        } else {
            x <- as.Date.grates_yearquarter(x)
            format(x, format)
        }
    }

    scales::trans_new(
        "grates_yearquarter",
        transform = as.numeric,
        inverse = as.numeric,
        breaks = brks,
        format = fmt
    )
}
