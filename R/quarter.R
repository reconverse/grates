# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------------------- QUARTER -------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' Construct a grates_quarter object
#'
#' `quarter()` is a constructor for a <grates_quarter> object
#'
#' @param x Integer vector representing the number of quarters (starting at 0),
#'   since the Unix epoch (1970-01-01).
#'
#' @examples
#' quarter(0:3)
#'
#' @references The algorithm to convert between dates and months relative to the
#'   UNIX Epoch comes from the work of Davis Vaughan in the unreleased
#'   [datea](https://github.com/DavisVaughan/datea/) package.
#'
#' @export
quarter <- function(x = integer()) {
  x <- vec_cast(x, integer())
  new_quarter(x)
}

#' @rdname quarter
#' @export
is_quarter <- function(x) inherits(x, "grates_quarter")

# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------ AS_QUARTER ------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' Convert an object to grates_quarter object
#'
#' - Date, POSIXct, and POSIXlt are converted, with the timezone respected,
#'   using [clock::as_date()].
#' - Character input is parsed using [clock::date_parse()].
#'
#' @param x An object to convert.
#' @param ... Not currently used.
#'
#' @return A `grates_quarter` object.
#'
#' @examples
#' as_quarter(Sys.Date())
#' as_quarter(as.POSIXct("2019-03-04 01:01:01", tz = "America/New_York"))
#' as_quarter("2019-05-03")
#'
#' @note Internally `grates_quarter` objects are stored as the number of
#'   quarters (starting at 0) since the Unix Epoch (1970-01-01)
#'
#' @export
as_quarter <- function(x, ...) {
  UseMethod("as_quarter")
}

#' @rdname as_quarter
#' @export
as_quarter.default <- function(x, ...) {
  check_dots_empty()
  vec_cast(x, new_quarter())
}

#' @inheritParams clock::date_parse
#' @rdname as_quarter
#' @export
as_quarter.character <- function(x, format = NULL, locale = clock_locale(), ...) {
  check_dots_empty()
  x <- date_parse(x, format = format, locale = locale)
  if (all(is.na(x))) abort("Unable to parse any entries of x as Dates")
  vec_cast(x, new_quarter())
}

#' @inheritParams clock::date_parse
#' @rdname as_month
#' @export
as_quarter.factor <- function(x, format = NULL, locale = clock_locale(), ...) {
  check_dots_empty()
  x <- as.character(x)
  as_quarter.character(x, format = format, locale = locale)
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------ FORMATING -------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' Format a grates_quarter object
#'
#' @param x A grates_quarter object.
#' @param ... Not currently used.
#'
#' @export
format.grates_quarter <- function(x, ...) {
  if (length(x) == 0) return(character(0))
  days <- month_to_days(as.integer(3L * unclass(x)))
  out <- as_utc_posixlt_from_int(days)
  out <- sprintf("%04d-Q%d", out$year + 1900L, out$mon %/% 3L +1)
  out[is.na(x)] <- NA_character_
  out
}

#' @export
vec_ptype_abbr.grates_quarter <- function(x, ...) "qtr"

#' @export
obj_print_data.grates_quarter <- function(x, ...) {
  print(format(x), quote = FALSE)
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------ PROTOTYPES ------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
vec_ptype2.grates_quarter.grates_quarter <- function(x, y, ..., x_arg = "", y_arg = "") {
  new_quarter()
}

# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------------------- CASTING -------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
vec_cast.grates_quarter.grates_quarter <- function(x, to, ...) x

#' @export
vec_cast.grates_quarter.Date <- function(x, to, ...) {
  q <- vec_cast.grates_month.Date(x, new_month(n = 3L, origin = 0L))
  q <- vec_cast(unclass(q)/3L, integer())
  new_quarter(q)
}

#' @export
vec_cast.Date.grates_quarter <- function(x, to, ...) {
  x <- 3L * unclass(x)
  x <- new_month(x, n=3L, origin = 0L )
  vec_cast.Date.grates_month(x, to)
}

#' @export
vec_cast.grates_quarter.POSIXct <- function(x, to, ...) {
  q <- vec_cast.grates_month.POSIXct(x, new_month(n = 3L, origin = 0L))
  q <- vec_cast(unclass(q)/3L, integer())
  new_quarter(q)
}

#' @export
vec_cast.grates_quarter.POSIXlt <- vec_cast.grates_quarter.POSIXct

#' @export
vec_cast.POSIXct.grates_quarter <- function(x, to, ...) {
  x <- 3L * unclass(x)
  x <- new_month(x, n=3L, origin = 0L )
  vec_cast.POSIXct.grates_month(x, to)
}

#' @export
vec_cast.POSIXlt.grates_quarter <- function(x, to, ...) {
  x <- 3L * unclass(x)
  x <- new_month(x, n=3L, origin = 0L )
  vec_cast.POSIXlt.grates_month(x, to)
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# --------------------------- OTHER CONVERSIONS --------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
as.character.grates_quarter <- function(x, ...) {
  check_dots_empty()
  format(x)
}

#' @export
as.list.grates_quarter <- function(x, ...) {
  check_dots_empty()
  out <- lapply(unclass(x),  new_quarter)
  setNames(out, names(x))
}

#' @export
as.double.grates_quarter <- function(x, ...) {
  check_dots_empty()
  out <- as.double(unclass(x))
  setNames(out, names(x))
}

#' @export
as.numeric.grates_quarter <- function(x, ...) {
  check_dots_empty()
  out <- as.numeric(unclass(x))
  setNames(out, names(x))
}

#' @export
as.integer.grates_quarter <- function(x, ...) {
  check_dots_empty()
  out <- as.integer(unclass(x))
  setNames(out, names(x))
}

#' @export
as.data.frame.grates_month <- as.data.frame.vector

# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# --------------------------------- MATH ---------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

is_nan_grates_quarter <- function(x) vector("logical", vec_size(x))

is_finite_grates_quarter <- function(x) !vec_detect_missing(x)

is_infinite_grates_quarter <- function(x) vector("logical", vec_size(x))

#' @export
#' @method vec_math grates_quarter
vec_math.grates_quarter <- function(.fn, .x, ...) {
  switch(
    .fn,
    "is.nan" = is_nan_grates_quarter(.x),
    "is.finite" = is_finite_grates_quarter(.x),
    "is.infinite" = is_infinite_grates_quarter(.x),
    abort(sprintf("`%s()` is not supported for <grates_quarter>", .fn))
  )
}

#' @export
quantile.grates_quarter <- function(x, type = 1, ...) {
  q <- as.integer(quantile(unclass(x), type = type, ...))
  new_quarter(q)
}

# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------ ARITHMETIC ------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
#' @method vec_arith grates_quarter
vec_arith.grates_quarter <- function(op, x, y, ...) {
  UseMethod("vec_arith.grates_quarter", y)
}

#' @export
#' @method vec_arith.grates_quarter default
vec_arith.grates_quarter.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}

#' @export
#' @method vec_arith.grates_quarter grates_quarter
vec_arith.grates_quarter.grates_quarter <- function(op, x, y, ...) {
  switch(
    op,
    "-" = vec_arith_base(op, x, y),
    stop_incompatible_op(op, x, y)
  )
}

#' @export
#' @method vec_arith.grates_quarter numeric
vec_arith.grates_quarter.numeric <- function(op, x, y, ...) {
  y <- vec_cast(y, integer())
  switch(
    op,
    "+" = ,
    "-" = new_quarter(as.integer(vec_arith_base(op, x, y))),
    stop_incompatible_op(op, x, y)
  )
}

#' @export
#' @method vec_arith.numeric grates_quarter
vec_arith.numeric.grates_quarter <- function(op, x, y, ...) {
  x <- vec_cast(x, integer())
  switch(
    op,
    "+" = new_quarter(as.integer(vec_arith_base(op, x, y))),
    stop_incompatible_op(op, x, y)
  )
}

#' @export
#' @method vec_arith.grates_quarter MISSING
vec_arith.grates_quarter.MISSING <- function(op, x, y, ...) {
  switch(
    op,
    `+` = x,
    stop_incompatible_op(op, x, y)
  )
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------------------- INTERNALS ------------------------------ #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
new_quarter <- function(x = integer()) {
  vec_assert(x, integer())
  new_vctr(x, class = "grates_quarter")
}
