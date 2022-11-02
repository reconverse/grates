# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# --------------------------------- YEAR ---------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' Construct a grates_year object
#'
#' `year()` is a constructor for a <grates_year> object
#'
#' @param x Integer vector representing the year.
#'
#' @examples
#' year(2021)
#'
#' @export
year <- function(x = integer()) {
  x <- vec_cast(x, integer())
  new_year(x)
}

#' @rdname year
#' @export
is_year <- function(x) inherits(x, "grates_year")


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------- AS_YEAR --------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' Convert an object to grates_year object
#'
#' - Date, POSIXct, and POSIXlt are converted, with the timezone respected,
#'   using [clock::as_date()].
#' - Character input is parsed using [clock::date_parse()].
#'
#' @param x An object to convert.
#' @param ... Not currently used.
#'
#' @return A `grates_year` object.
#'
#' @examples
#' as_year(Sys.Date())
#' as_year(as.POSIXct("2019-03-04 01:01:01", tz = "America/New_York"), interval = 2)
#' as_year("2019-05-03")
#'
#' @export
as_year <- function(x, ...) {
  UseMethod("as_year")
}

#' @rdname as_year
#' @export
as_year.default <- function(x, ...) vec_cast(x, new_year())

#' @inheritParams clock::date_parse
#' @rdname as_year
#' @export
as_year.character <- function(x, format = NULL, locale = clock_locale(), ...) {
  check_dots_empty()
  x <- date_parse(x, format = format, locale = locale)
  if (all(is.na(x))) abort("Unable to parse any entries of x as Dates")
  vec_cast(x, new_year())
}

#' @inheritParams clock::date_parse
#' @rdname as_year
#' @export
as_year.factor <- function(x, format = NULL, locale = clock_locale(), ...) {
  check_dots_empty()
  x <- as.character(x)
  as_year.character(x, format = format, locale = locale)
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------ FORMATING -------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' Format a grates_year object
#'
#' @param x A grates_year object.
#' @param ... Not currently used.
#'
#' @export
format.grates_year <- function(x, ...) {
  if (length(x) == 0) return(character(0))
  out <- format(unclass(x))
  out[is.na(x)] <- NA_character_
  out
}

#' @export
vec_ptype_abbr.grates_year <- function(x, ...) "year"

#' @export
obj_print_data.grates_year <- function(x, ...) print(format(x), quote = FALSE)

# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------ PROTOTYPES ------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
vec_ptype2.grates_year.grates_year <- function(x, y, ...) new_year()

# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------------------- CASTING -------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
vec_cast.grates_year.grates_year <- function(x, to, ...) x

#' @export
vec_cast.grates_year.Date <- function(x, to, ...) {
  x <- date_group(x, precision = "month", n = 1L) # floor to start of month
  x <- as_utc_posixlt_from_int(x) # convert to posixlt
  yr <- x$year + 1900L # calculate the year
  new_year(yr)
}

#' @export
vec_cast.Date.grates_year <- function(x, to, ...) {
  date_build(year = unclass(x), invalid = "error")
}

#' @export
vec_cast.grates_year.POSIXct <- function(x, to, ...) {
  x <- as.POSIXlt(x)
  yr <- x$year + 1900L # calculate the year
  new_year(yr)
}

#' @export
vec_cast.grates_year.POSIXlt <- function(x, to, ...) {
  yr <- x$year + 1900L # calculate the year
  new_year(yr)
}

#' @export
vec_cast.POSIXct.grates_year <- function(x, to, ...) {
  tz <- date_zone(to)
  date_time_build(
    year = unclass(x),
    zone = tz,
    invalid = "error",
    nonexistent = "error",
    ambiguous = "error"
  )
}

#' @export
vec_cast.POSIXlt.grates_year <- function(x, to, ...) {
  out <- vec_cast.POSIXct.grates_year(x, to)
  as.POSIXlt(out)
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# --------------------------- OTHER CONVERSIONS --------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
as.character.grates_year <- function(x, ...) {
  check_dots_empty()
  format(x)
}

#' @export
as.list.grates_year <- function(x, ...) {
  check_dots_empty()
  out <- lapply(unclass(x), new_year)
  setNames(out, names(x))
}

#' @export
as.double.grates_year <- function(x, ...) {
  check_dots_empty()
  out <- as.double(unclass(x))
  setNames(out, names(x))
}

#' @export
as.numeric.grates_year <- function(x, ...) {
  check_dots_empty()
  out <- as.numeric(unclass(x))
  setNames(out, names(x))
}

#' @export
as.integer.grates_year <- function(x, ...) {
  check_dots_empty()
  out <- unclass(x)
  setNames(out, names(x))
}

#' @export
as.data.frame.grates_year <- as.data.frame.vector


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# --------------------------------- MATH ---------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

is_nan_grates_year <- function(x) vector("logical", vec_size(x))

is_finite_grates_year <- function(x) !vec_detect_missing(x)

is_infinite_grates_year <- function(x) vector("logical", vec_size(x))

#' @export
#' @method vec_math grates_year
vec_math.grates_year <- function(.fn, .x, ...) {
  switch(
    .fn,
    "is.nan" = is_nan_grates_year(.x),
    "is.finite" = is_finite_grates_year(.x),
    "is.infinite" = is_infinite_grates_year(.x),
    abort(sprintf("`%s()` is not supported for <grates_year>", .fn))
  )
}

#' @export
quantile.grates_year <- function(x, type = 1, ...) {
  q <- as.integer(quantile(unclass(x), type = type, ...))
  new_year(q)
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------ ARITHMETIC ------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
#' @method vec_arith grates_year
vec_arith.grates_year <- function(op, x, y, ...) {
  UseMethod("vec_arith.grates_year", y)
}

#' @export
#' @method vec_arith.grates_year default
vec_arith.grates_year.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}

#' @export
#' @method vec_arith.grates_year grates_year
vec_arith.grates_year.grates_year <- function(op, x, y, ...) {
  switch(
    op,
    "-" = vec_arith_base(op, x, y),
    stop_incompatible_op(op, x, y)
  )
}

#' @export
#' @method vec_arith.grates_year numeric
vec_arith.grates_year.numeric <- function(op, x, y, ...) {
  y <- vec_cast(y, integer())
  switch(
    op,
    "+" = ,
    "-" = new_year(as.integer(vec_arith_base(op, x, y))),
    stop_incompatible_op(op, x, y)
  )
}

#' @export
#' @method vec_arith.numeric grates_year
vec_arith.numeric.grates_year <- function(op, x, y, ...) {
  x <- vec_cast(x, integer())
  switch(
    op,
    "+" = new_year(as.integer(vec_arith_base(op, x, y))),
    stop_incompatible_op(op, x, y)
  )
}

#' @export
#' @method vec_arith.grates_year MISSING
vec_arith.grates_year.MISSING <- function(op, x, y, ...) {
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
new_year <- function(x = integer()) {
  vec_assert(x, ptype = integer())
  new_vctr(x, class = "grates_year")
}

