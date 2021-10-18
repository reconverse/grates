# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------------------- PERIOD --------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' Construct a grates_period object
#'
#' `period()` is a constructor for a <grates_period> object
#'
#' @param x Integer vector representing the number of days since the Unix
#'   epoch (1970-01-01) at the beginning of the specified grouping.
#' @param n An integer indicating the (fixed) number of days used for
#'   grouping; defaults to 1.
#' @param origin Day since the Unix epoch where grouping begins (default 0).
#'
#' @note To allow easy comparison between <grates_period> object, the `origin`
#'   will be stored as it's value modulo `n` (i.e `origin <- origin %% n`).
#'
#' @return a <grates_period> vector.
#'
#' @examples
#' period(1, n = 2, origin = 1)
#'
#' @export
period <- function(x = integer(), n = 1L, origin = 0L) {

  # ensure inputs are all integer
  x <- vec_cast(x, integer())
  n <- vec_cast(n, integer())
  origin <- vec_cast(origin, integer())

  stopifnot("`n` must be >= 1" = n > 0)

  # check that the input data is compatible with the specified n
  if (!is_valid_period_interval(x, n)) {
    abort("`n` not compatible with specified data `x`")
  }

  # check that origin is compatible with with combination of x and n
  origin <- origin %% n
  if(!all(x %% n == origin)) {
    abort("`origin` not compatible with specified `x` and `n")
  }

  new_period(x, n = n, origin = origin)
}

#' @rdname period
#' @export
is_period <- function(x) inherits(x, "grates_period")



# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------ AS_PERIOD -------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' Convert an object to grates_period object
#'
#' - Date, POSIXct, and POSIXlt are converted, with the timezone respected,
#'   using [clock::as_date()].
#' - Character input is parsed using [clock::date_parse()].
#'
#' @param x An object to convert.
#' @param n An integer indicating the (fixed) number of days used for
#'   grouping; defaults to 1.
#' @param origin Month since the Unix epoch where grouping begins (default 0).
#' @param ... Not currently used.
#'
#' @return A `grates_period` object.
#'
#' @examples
#' as_period(Sys.Date())
#' as_period(as.POSIXct("2019-03-04 01:01:01", tz = "America/New_York"), interval = 2)
#' as_period("2019-05-03")
#'
#' @note Internally `grates_period` objects are stored as the number of days
#'   (starting at 0) since the Unix Epoch (1970-01-01) to the earliest day in
#'   the specified grouping.
#'
#' @export
as_period <- function(x, ...) {
  UseMethod("as_period")
}

#' @rdname as_period
#' @export
as_period.default <- function(x, n = 1L, origin = 0L, ...) {
  n <- vec_cast(n, integer())
  origin <- vec_cast(origin, integer())
  vec_cast(x, new_period(n = n, origin = origin))
}

#' @inheritParams clock::date_parse
#' @rdname as_period
#' @export
as_period.character <- function(x, n = 1L, origin = 0L, format = NULL,
                               locale = clock_locale(), ...) {
  check_dots_empty()
  n <- vec_cast(n, integer())
  origin <- vec_cast(origin, integer())
  x <- date_parse(x, format = format, locale = locale)
  if (all(is.na(x))) abort("Unable to parse any entries of x as Dates")
  vec_cast(x, new_period(n = n, origin = origin))
}

#' @inheritParams clock::date_parse
#' @rdname as_period
#' @export
as_period.factor <- function(x, n = 1L, origin = 0L, format = NULL,
                            locale = clock_locale(), ...) {
  check_dots_empty()
  x <- as.character(x)
  as_period.character(x,n = n, origin = origin, format = format, locale = locale)
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------ FORMATING -------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' Format a grates_period object
#'
#' @param x A grates_period object.
#' @param format The format to use for the bounds of each grates_period entry.
#' @param sep Where more than one day is grouped with others, `sep` is placed
#'   between the upper and lower bounds when printing.
#' @param ... Not currently used.
#'
#' @export
format.grates_period <- function(x, format = "%Y-%m-%d", sep = "to", ...) {
  if (length(x) == 0) return(character(0))
  n <- attr(x, "n")
  if (n > 1) {
    out <- sprintf(
      "%s %s %s",
      format.Date(vec_cast(x, new_date()), format = format),
      sep,
      format.Date(vec_cast(x + 1, new_date()) - 1, format = format)
    )
  } else {
    out <- format.Date(vec_cast(x, new_date()), format = format)
  }
  out[is.na(x)] <- NA_character_
  out
}

#' @export
vec_ptype_abbr.grates_period <- function(x, ...) "period"

#vec_ptype_full.grates_period <- function(x, ...) sprintf("period: n = %d", attr(x, "n"))

#' @export
obj_print_data.grates_period <- function(x, format = "%Y-%m-%d", sep = "to", ...) {
  print(format(x, format = format, sep = sep), quote = FALSE)
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------ PROTOTYPES ------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
vec_ptype2.grates_period.grates_period <- function(x, y, ...) {
  # check compatibility of n
  nx <- attr(x, "n")
  ny <- attr(y, "n")
  if (nx != ny) abort("Can't combine <grates_period>'s with different `n`")

  # check compatibility of the origin
  ox <- attr(x, "origin")
  oy <- attr(y, "origin")
  if (ox != oy) abort("Can't combine <grates_period>'s with different `origin`")

  # prototype
  new_period(n = nx, origin = ox)
}

# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------------------- CASTING -------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
vec_cast.grates_period.grates_period <- function(x, to, ...) {
  # check compatibility of n
  nx <- attr(x, "n")
  nto <- attr(to, "n")
  if (nx != nto) stop_incompatible_cast(x, to)

  # check compatibility of the origin
  ox <- attr(x, "origin")
  oto <- attr(to, "origin")
  if (ox != oto) stop_incompatible_cast(x, to)

  x
}

#' @export
vec_cast.grates_period.Date <- function(x, to, ...) {

  # Ensure no fractional days
  x <- as.integer(trunc(x))

  # Adjust the origin for dates around Unix Epoch or prior
  n <- attr(to, "n")
  origin <- attr(to, "origin") %% n
  min <- min(x, na.rm = TRUE)
  while(min < origin) {
    origin <- origin - n
  }

  # generate sequence of month groupings staring at origin
  period_starts <- seq.int(from = origin, to = max(x, na.rm = TRUE), by = n)
  idx <- cut(x, breaks = c(period_starts, Inf), labels = FALSE, right = FALSE)
  out <- period_starts[idx]

  # rescale origin modulo n
  origin <- min(out, na.rm = TRUE) %% n

  new_period(out, n = n, origin = origin)
}

#' @export
vec_cast.Date.grates_period <- function(x, to, ...) {
  new_date(as.double(x))
}

#' @export
vec_cast.grates_period.POSIXct <- function(x, to, ...) {
  x <- as_date(x)
  n <- attr(to, "n")
  origin <- attr(to, "origin")
  vec_cast.grates_period.Date(x, new_period(n = n, origin = origin))
}

#' @export
vec_cast.grates_period.POSIXlt <- vec_cast.grates_period.POSIXct

#' @export
vec_cast.POSIXct.grates_period <- function(x, to, ...) {
  out <- vec_cast.Date.grates_period(x, to)
  tz <- date_zone(to)
  out <- as_zoned_time(out, zone = tz, nonexistent = "roll-forward", ambiguous = "latest")
  as_date_time(out)
}

#' @export
vec_cast.POSIXlt.grates_period <- function(x, to, ...) {
  out <- vec_cast.Date.grates_period(x, to)
  tz <- date_zone(to)
  out <- as_zoned_time(out, zone = tz, nonexistent = "roll-forward", ambiguous = "latest")
  as.POSIXlt(out)
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# --------------------------- OTHER CONVERSIONS --------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
as.character.grates_period <- function(x, format = "%Y-%m-%d", sep = "to", ...) {
  check_dots_empty()
  format(x, format = format, sep = sep)
}

#' @export
as.list.grates_period <- function(x, ...) {
  check_dots_empty()
  n <- attr(x, "n")
  origin <- attr(x, "origin")
  out <- lapply(unclass(x), new_period, n = n, origin = origin)
  setNames(out, names(x))
}

#' @export
as.double.grates_period <- function(x, ...) {
  check_dots_empty()
  out <- as.double(unclass(x))
  setNames(out, names(x))
}

#' @export
as.numeric.grates_period <- function(x, ...) {
  check_dots_empty()
  out <- as.numeric(unclass(x))
  setNames(out, names(x))
}

#' @export
as.integer.grates_period <- function(x, ...) {
  check_dots_empty()
  attributes(x) <- NULL
  out <- unclass(x)
  setNames(out, names(x))
}

#' @export
as.data.frame.grates_period <- as.data.frame.vector


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# --------------------------------- MATH ---------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

is_nan_grates_period <- function(x) vector("logical", vec_size(x))

is_finite_grates_period <- function(x) !vec_equal_na(x)

is_infinite_grates_period <- function(x) vector("logical", vec_size(x))

#' @export
#' @method vec_math grates_period
vec_math.grates_period <- function(.fn, .x, ...) {
  switch(
    .fn,
    "is.nan" = is_nan_grates_period(.x),
    "is.finite" = is_finite_grates_period(.x),
    "is.infinite" = is_infinite_grates_period(.x),
    abort(sprintf("`%s()` is not supported for <grates_period>", .fn))
  )
}


#' @export
quantile.grates_period <- function(x, type = 1, ...) {
  n <- attr(x, "n")
  origin <- attr(x, "origin")
  periods <- as.integer(quantile(unclass(x), type = type, ...))
  as_period(periods, n = n, origin = origin)
}

# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------ ARITHMETIC ------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
#' @method vec_arith grates_period
vec_arith.grates_period <- function(op, x, y, ...) {
  UseMethod("vec_arith.grates_period", y)
}

#' @export
#' @method vec_arith.grates_period default
vec_arith.grates_period.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}

#' @export
#' @method vec_arith.grates_period grates_period
vec_arith.grates_period.grates_period <- function(op, x, y, ...) {

  # check compatibility of n
  nx <- attr(x, "n")
  ny <- attr(y, "n")
  if (nx != ny) stop_incompatible_type(x, y)

  # check compatibility of the origin
  ox <- attr(x, "origin")
  oy <- attr(y, "origin")
  if (nx != ny) stop_incompatible_type(x, y)

  switch(
    op,
    "-" = vec_arith_base(op, x, y) / nx,
    stop_incompatible_op(op, x, y)
  )
}

#' @export
#' @method vec_arith.grates_period numeric
vec_arith.grates_period.numeric <- function(op, x, y, ...) {
  n <- attr(x, "n")
  y <- vec_cast(y, integer())
  origin <- attr(x, "origin")
  switch(
    op,
    "+" = ,
    "-" = new_period(as.integer(vec_arith_base(op, x, n*y)), n = n, origin = origin),
    stop_incompatible_op(op, x, y)
  )
}

#' @export
#' @method vec_arith.numeric grates_period
vec_arith.numeric.grates_period <- function(op, x, y, ...) {
  n <- attr(y, "n")
  x <- vec_cast(x, integer())
  origin <- attr(y, "origin")
  switch(
    op,
    "+" = new_period(as.integer(vec_arith_base(op, n*x, y)), n = n, origin = origin),
    stop_incompatible_op(op, x, y)
  )
}

#' @export
#' @method vec_arith.grates_period MISSING
vec_arith.grates_period.MISSING <- function(op, x, y, ...) {
  switch(op,
         `+` = x,
         stop_incompatible_op(op, x, y)
  )
}

# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------------------- INTERNALS ------------------------------ #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
new_period <- function(x = integer(), n = 1L, origin = 0L) {
  vec_assert(x, ptype = integer())
  vec_assert(n, ptype = integer(), size = 1L)
  vec_assert(origin, ptype = integer(), size = 1L)
  new_vctr(x, n = n, origin = origin, class = "grates_period")
}

is_valid_period_interval <- function(x = integer(), n = integer()) {
  x <- x[!is.na(x)]
  if (length(x)) all(diff(x) %% n == 0) else TRUE
}

