#' epiweek scale
#'
#' ggplot2 scale for an `<grates_epiweek>` vector.
#'
#' @param n.breaks Approximate number of breaks calculated using
#' `scales::breaks_pretty` (default 6).
#'
#' @param format Format to use if "Date" scales are required. If NULL (default)
#' then labels are in the standard yearweek format (YYYY-Www). If not NULL
#' then the value is used by `format.Date()` and can be any input acceptable
#' by that function.
#'
#' @param ... Not currently used.
#'
#' @return A scale for use with ggplot2.
#'
#' @export
scale_x_grates_epiweek <- function(..., n.breaks = 6, format = NULL) {

    check_suggests("ggplot2")
    check_suggests("scales") # precautionary but overkill as currently a dependency of ggplot2


    ggplot2::scale_x_continuous(
        trans = .grates_epiweek_trans(
            n.breaks = n.breaks,
            format = format
        )
    )
}

# -------------------------------------------------------------------------
scale_type.grates_epiweek <- function(x) "grates_epiweek"

# -------------------------------------------------------------------------
.grates_epiweek_trans <- function(n.breaks, format) {

    shift <- if (is.null(format)) 0 else 0.5

    # breaks function
    brks <- function(x) {
        dat <- scales::breaks_pretty(n.breaks)(as.numeric(x))
        dat <- as.integer(floor(dat))
        as.numeric(epiweek(dat)) - shift
    }

    # format function
    fmt <- function(x) {
        x <- epiweek(x + shift)
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
