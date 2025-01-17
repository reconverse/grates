# -------------------------------------------------------------------------
#' yearweek scale
#'
# -------------------------------------------------------------------------
#' ggplot2 scale for an `<grates_yearweek>` vector.
#'
# -------------------------------------------------------------------------
#' @param breaks
#'
#' A `<grates_yearweek>` vector of the desired breaks.
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
#' If "week" then the labels are of the form Www (e.g. W37).
#'
#' Otherwise the value is used by `format.Date()` and can be any input
#' acceptable by that function.
#'
#' @param firstday `[integer]`
#'
#' Integer value of the first weekday: 1 (Monday) to 7 (Sunday).
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
scale_x_grates_yearweek <- function(
    ...,
    breaks = ggplot2::waiver(),
    n.breaks = 6L,
    firstday,
    format = NULL
) {

    .check_suggests("ggplot2")
    .check_suggests("scales") # precautionary but overkill as currently a dependency of ggplot2

    if (missing(firstday))
        stop("Please provide a value of `firstday` corresponding to the given data.")

    if (length(firstday) != 1L)
        stop("`firstday` must be an integer of length 1.")

    if (!is.integer(firstday)) {
        if (!.is_whole(firstday))
            stop("`firstday` must be an integer of length 1.")
        firstday <- as.integer(firstday)
    }

    if (firstday < 1L || firstday > 7L || is.na(firstday))
        stop("`firstday` must be an integer between 1 (Monday) and 7 (Sunday).")


    # ggplot2 3.5.0 deprecated the `trans` argument in favour of `transform`.
    # We could just force a minimum ggplot2 version and avoid this branching
    # but it's relatively low effort so leaving for now.
    # TODO - revisit.
    if (utils::packageVersion("ggplot2") < '3.5.0') {
        ggplot2::scale_x_continuous(
            trans = .grates_yearweek_trans(
                breaks = breaks,
                n.breaks = n.breaks,
                firstday = firstday,
                format = format
            )
        )
    } else {
        ggplot2::scale_x_continuous(
            transform = .grates_yearweek_trans(
                breaks = breaks,
                n.breaks = n.breaks,
                firstday = firstday,
                format = format
            )
        )
    }
}

# -------------------------------------------------------------------------
#' @export
#' @rdname scale_x_grates_yearweek
scale_x_grates_yearweek_monday <- function(..., breaks = ggplot2::waiver(), n.breaks = 6, format = NULL) {
    scale_x_grates_yearweek(..., breaks = breaks, n.breaks = n.breaks, firstday = 1L, format = format)
}

# -------------------------------------------------------------------------
#' @export
#' @rdname scale_x_grates_yearweek
scale_x_grates_yearweek_isoweek <- function(..., breaks = ggplot2::waiver(), n.breaks = 6, format = NULL) {
    scale_x_grates_yearweek(..., breaks = breaks, n.breaks = n.breaks, firstday = 1L, format = format)
}

# -------------------------------------------------------------------------
#' @export
#' @rdname scale_x_grates_yearweek
scale_x_grates_yearweek_tuesday <- function(..., breaks = ggplot2::waiver(), n.breaks = 6, format = NULL) {
    scale_x_grates_yearweek(..., breaks = breaks, n.breaks = n.breaks, firstday = 2L, format = format)
}

# -------------------------------------------------------------------------
#' @export
#' @rdname scale_x_grates_yearweek
scale_x_grates_yearweek_wednesday <- function(..., breaks = ggplot2::waiver(), n.breaks = 6, format = NULL) {
    scale_x_grates_yearweek(..., breaks = breaks, n.breaks = n.breaks, firstday = 3L, format = format)
}

# -------------------------------------------------------------------------
#' @export
#' @rdname scale_x_grates_yearweek
scale_x_grates_yearweek_thursday <- function(..., breaks = ggplot2::waiver(), n.breaks = 6, format = NULL) {
    scale_x_grates_yearweek(..., breaks = breaks, n.breaks = n.breaks, firstday = 4L, format = format)
}

# -------------------------------------------------------------------------
#' @export
#' @rdname scale_x_grates_yearweek
scale_x_grates_yearweek_friday <- function(..., breaks = ggplot2::waiver(), n.breaks = 6, format = NULL) {
    scale_x_grates_yearweek(..., breaks = breaks, n.breaks = n.breaks, firstday = 5L, format = format)
}

# -------------------------------------------------------------------------
#' @export
#' @rdname scale_x_grates_yearweek
scale_x_grates_yearweek_saturday <- function(..., breaks = ggplot2::waiver(), n.breaks = 6, format = NULL) {
    scale_x_grates_yearweek(..., breaks = breaks, n.breaks = n.breaks, firstday = 6L, format = format)
}

# -------------------------------------------------------------------------
#' @export
#' @rdname scale_x_grates_yearweek
scale_x_grates_yearweek_sunday <- function(..., breaks = ggplot2::waiver(), n.breaks = 6, format = NULL) {
    scale_x_grates_yearweek(..., breaks = breaks, n.breaks = n.breaks, firstday = 7L, format = format)
}

# -------------------------------------------------------------------------
#' @export
#' @rdname scale_x_grates_yearweek
scale_x_grates_yearweek_epiweek <- function(..., breaks = ggplot2::waiver(), n.breaks = 6, format = NULL) {
    scale_x_grates_yearweek(..., breaks = breaks, n.breaks = n.breaks, firstday = 7L, format = format)
}

# -------------------------------------------------------------------------
# When adding a geom, ggplot2 will look for a relevant method to the generic
# scale_type.  It will then dispatch based on the output of this function to
# one of scale_x_yearweek_xxx defined below.
# registered in .onLoad() (see zzz.R)
#' @exportS3Method ggplot2::scale_type
scale_type.grates_yearweek <- function(x) {

    # -------------------------------------------------------------------------
    # -------------------------------------------------------------------------
    # TODO - remove this if https://github.com/tidyverse/ggplot2/issues/4705
    #        gets resolved
    if (!"grates" %in% .packages())
        stop("<grates_yearweek> object found, but grates package is not attached.\n  Please attach via `library(grates)`.")
    # -------------------------------------------------------------------------
    # -------------------------------------------------------------------------

    if (inherits(x, "grates_yearweek_monday"))
        return("grates_yearweek_monday")
    if (inherits(x, "grates_yearweek_tuesday"))
        return("grates_yearweek_tuesday")
    if (inherits(x, "grates_yearweek_wednesday"))
        return("grates_yearweek_wednesday")
    if (inherits(x, "grates_yearweek_thursday"))
        return("grates_yearweek_thursday")
    if (inherits(x, "grates_yearweek_friday"))
        return("grates_yearweek_friday")
    if (inherits(x, "grates_yearweek_saturday"))
        return("grates_yearweek_saturday")
    if (inherits(x, "grates_yearweek_sunday"))
        return("grates_yearweek_sunday")
    stop("Invalid <grates_yearweek> object - class corrupted.")
}

# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------------------- INTERNALS ------------------------------ #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
.grates_yearweek_trans <- function(breaks, n.breaks, firstday, format) {

    shift <- if (is.null(format) || format == "week") 0 else 0.5

    # breaks function
    brks <- function(x) {
        if (inherits(breaks, "waiver")) {
            dat <- scales::breaks_pretty(n.breaks)(as.numeric(x))
            dat <- as.integer(floor(dat))
            dat <- as.numeric(new_yearweek(dat, firstday = firstday))
        } else {
            dat <- as.numeric(breaks)
        }
        dat - shift
    }

    # format function
    fmt <- function(x) {
        x <- new_yearweek(x + shift, firstday = firstday)
        if (is.null(format)) {
            format.grates_yearweek(x)
        } else if (format == "week") {
            x <- format.grates_yearweek(x)
            sub(pattern = ".*-", replacement = "", x = x, perl = TRUE)
        } else {
            x <- as.Date.grates_yearweek(x)
            format(x, format)
        }
    }

    scales::trans_new(
        "grates_yearweek",
        transform = as.numeric,
        inverse = as.numeric,
        breaks = brks,
        format = fmt
    )
}
