grates_period_env <-  new.env(parent = emptyenv())

# -------------------------------------------------------------------------
#' Period scale
#'
# -------------------------------------------------------------------------
#' ggplot2 scale for an `<grates_period>` vector.
#'
# -------------------------------------------------------------------------
#' @param breaks
#'
#' A `<grates_period>` vector of the desired breaks.
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
#' @examplesIf requireNamespace("outbreaks") && requireNamespace("ggplot2")
#'
#' # use simulated linelist data from the outbreaks package
#' linelist <- outbreaks::ebola_sim_clean$linelist
#'
#' # Calculate the total of infections across 14 day periods offset from
#' # the first date
#' x <- linelist$date_of_infection
#' x <- as_period(x, n = 14, offset = min(x, na.rm = TRUE))
#' dat <- aggregate(list(cases = x), by = list(period = x), FUN = length)
#' head(dat)
#'
#' (period_plot <- ggplot2::ggplot(dat, ggplot2::aes(period, cases)) +
#'     ggplot2::geom_col(width = 1, colour = "white") +
#'     ggplot2::theme_bw() +
#'     ggplot2::theme(
#'         axis.text.x = ggplot2::element_text(
#'             angle = 45,
#'             hjust = 1
#'         )
#'     ) +
#'     ggplot2::xlab(""))
#'
#' # To change defaults we must explicitly state the value of n and
#' # offset when calling the scale function
#' period_plot + scale_x_grates_period(
#'     n.breaks = 2,
#'     n = 14,
#'     offset = min(x, na.rm = TRUE)
#' )
#'
# -------------------------------------------------------------------------
#' @export
scale_x_grates_period <- function(..., breaks = ggplot2::waiver(), n.breaks = 6L, format = "%Y-%m-%d", n, offset) {

    .check_suggests("ggplot2")
    .check_suggests("scales") # precautionary but overkill as currently a dependency of ggplot2

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

    if (inherits(offset, "Date")) {
        offset <- floor(as.numeric(offset))
    }

    if (!is.integer(offset)) {
        if (!.is_whole(offset))
            stop("`offset` must be an integer of length 1.")
        offset <- as.integer(offset)
    }

    # set environment variables to NULL so they don't mess other plots up
    grates_period_env$n <- NULL
    grates_period_env$offset <- NULL

    # ggplot2 3.5.0 deprecated the `trans` argument in favour of `transform`.
    # We could just force a minimum ggplot2 version and avoid this branching
    # but it's relatively low effort so leaving for now.
    # TODO - revisit.
    if (utils::packageVersion("ggplot2") < '3.5.0') {
        ggplot2::scale_x_continuous(
            trans = .grates_period_trans(
                breaks = breaks,
                n.breaks = n.breaks,
                n = n,
                offset = offset,
                format = format
            )
        )
    } else {
        ggplot2::scale_x_continuous(
            transform = .grates_period_trans(
                breaks = breaks,
                n.breaks = n.breaks,
                n = n,
                offset = offset,
                format = format
            )
        )
    }
}

# -------------------------------------------------------------------------
#' @exportS3Method ggplot2::scale_type
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
.grates_period_trans <- function(breaks, n.breaks, format, n, offset) {

    shift <- 0.5

    # breaks function
    brks <- function(x) {
        if (inherits(breaks, "waiver")) {
            dat <- scales::breaks_pretty(n.breaks)(as.numeric(x))
            dat <- as.integer(floor(dat))
            dat <- as.numeric(new_period(dat, n = n, offset = offset))
        } else {
            dat <- as.numeric(breaks)
        }
        dat - shift
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
