#' Defunct functions in package grates
#'
#' The functions or variables listed here are now defunct, i.e. no longer
#' available.
#'
#' * The `<grates_int_period>` class and it's associated functions were removed
#'   in version 1.0.0.
#'
#' @name grates-defunct
#' @keywords internal
NULL

#' @export
#' @rdname grates-defunct
#' @keywords internal
int_period <- function(x = integer(), n = 1L, origin = 0L) {
    .Defunct(
        package = "grates",
        msg = "As of grates version 1.0.0, the int_period function is defunct"
    )
}

#' @export
#' @rdname grates-defunct
#' @keywords internal
as_int_period <- function(x, ...) {
    .Defunct(
        package = "grates",
         msg = "As of grates version 1.0.0, the as_int_period function is defunct"
    )
}

#' @export
#' @rdname grates-defunct
#' @keywords internal
is_int_period <- function(x) {
    .Defunct(
        package = "grates",
        msg = "As of grates version 1.0.0, the is_int_period function is defunct"
    )
}

#' @export
#' @rdname grates-defunct
#' @keywords internal
scale_x_grates_int_period <- function(n.breaks = 6, n, origin) {
    .Defunct(
        package = "grates",
        msg = "As of grates version 1.0.0, the scale_x_grates_int_period function is defunct"
    )
}
