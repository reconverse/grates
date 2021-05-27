# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------ INT_PERIOD ------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' Construct a grates_int_period object
#'
#' `period()` is a constructor for a <grates_int_period> object
#'
#' @param x Integer vector representing the number of days at the beginning of
#'   the specified grouping.
#' @param n An integer indicating the (fixed) number of days used for
#'   grouping; defaults to 1.
#' @param origin Day on which the grouping begins (default 0).
#'
#' @note To allow easy comparison between <grates_int_period> object, the `origin`
#'   will be stored as it's value modulo `n` (i.e `origin <- origin %% n`).
#'
#' @return a <grates_int_period> vector.
#'
#' @examples
#' int_period(1, n = 2, origin = 1)
#'
#' @export
int_period <- function(x = integer(), n = 1L, origin = 0L) {

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

  new_int_period(x, n = n, origin = origin)
}

#' @rdname int_period
#' @export
is_int_period <- function(x) inherits(x, "grates_int_period")



# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ---------------------------- AS_INT_PERIOD ------------------------------ #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' Convert an object to grates_int_period object
#'
#' - Date, POSIXct, and POSIXlt are converted, with the timezone respected,
#'   using [clock::as_date()].
#' - Character input is parsed using [clock::date_parse()].
#'
#' @param x An object to convert.
#' @param n An integer indicating the (fixed) number of days used for
#'   grouping; defaults to 1.
#' @param origin Day on which the grouping begins (default 0).
#' @param ... Not currently used.
#'
#' @return A `grates_int_period` object.
#'
#' @examples
#' as_int_period(0:10, n = 2)
#'
#' @note Internally `grates_int_period` objects are stored as the number of days
#'   to the earliest day in the specified grouping.
#'
#' @export
as_int_period <- function(x, ...) {
  UseMethod("as_int_period")
}

#' @rdname as_int_period
#' @export
as_int_period.default <- function(x, n = 1L, origin = 0L, ...) {
  n <- vec_cast(n, integer())
  origin <- vec_cast(origin, integer())
  vec_cast(x, new_int_period(n = n, origin = origin))
}

# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------ FORMATING -------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' Format a grates_int_period object
#'
#' @param x A grates_int_period object.
#' @param format The format to use for the bounds of each grates_int_period entry.
#' @param sep Where more than one day is grouped with others, `sep` is placed
#'   between the upper and lower bounds when printing.
#' @param ... Not currently used.
#'
#' @export
format.grates_int_period <- function(x, sep = "to", ...) {
  if (length(x) == 0) return(character(0))
  n <- attr(x, "n")
  if (n > 1) {
    out <- sprintf("%d %s %d", as.integer(x), sep, as.integer(x+1) - 1)
  } else {
    out <- format(as.integer(x))
  }
  out[is.na(x)] <- NA_character_
  out
}

#' @export
vec_ptype_abbr.grates_int_period <- function(x, ...) "period"

#vec_ptype_full.grates_int_period <- function(x, ...) sprintf("period: n = %d", attr(x, "n"))

#' @export
obj_print_data.grates_int_period <- function(x, ...)print(format(x), quote = FALSE)


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------ PROTOTYPES ------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
vec_ptype2.grates_int_period.grates_int_period <- function(x, y, ...) {
  # check compatibility of n
  nx <- attr(x, "n")
  ny <- attr(y, "n")
  if (nx != ny) abort("Can't combine <grates_int_period>'s with different `n`")

  # check compatibility of the origin
  ox <- attr(x, "origin")
  oy <- attr(y, "origin")
  if (ox != oy) abort("Can't combine <grates_int_period>'s with different `origin`")

  # prototype
  new_int_period(n = nx, origin = ox)
}

# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------------------- CASTING -------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
vec_cast.grates_int_period.grates_int_period <- function(x, to, ...) {
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
vec_cast.grates_int_period.integer <- function(x, to, ...) {

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

  new_int_period(out, n = n, origin = origin)
}

#' @export
vec_cast.integer.grates_int_period <- function(x, to, ...) {
  out <- as.integer(unclass(x))
  setNames(out, names(x))
}

#' @export
vec_cast.grates_int_period.double <- function(x, to, ...) {
  x <- vec_cast(x, integer())
  vec_cast.grates_int_period.integer(x, to)
}

#' @export
vec_cast.double.grates_int_period <- function(x, to, ...) {
  out <- as.double(unclass(x))
  setNames(out, names(x))
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# --------------------------- OTHER CONVERSIONS --------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
as.character.grates_int_period <- function(x, ...) {
  check_dots_empty()
  out <- format(x)
  setNames(out, names(x))
}

#' @export
as.list.grates_int_period <- function(x, ...) {
  check_dots_empty()
  n <- attr(x, "n")
  origin <- attr(x, "origin")
  out <- lapply(unclass(x), new_int_period, n = n, origin = origin)
  setNames(out, names(x))
}

#' @export
as.integer.grates_int_period <- function(x, ...) {
  check_dots_empty()
  vec_cast.integer.grates_int_period(x)
}

#' @export
as.double.grates_int_period <- function(x, ...) {
  check_dots_empty()
  vec_cast.double.grates_int_period(x)
}

#' @export
as.numeric.grates_int_period <- as.double.grates_int_period

#' @export
as.data.frame.grates_int_period <- as.data.frame.vector


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# --------------------------------- MATH ---------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

is_nan_grates_int_period <- function(x) vector("logical", vec_size(x))

is_finite_grates_int_period <- function(x) !vec_equal_na(x)

is_infinite_grates_int_period <- function(x) vector("logical", vec_size(x))

#' @export
#' @method vec_math grates_int_period
vec_math.grates_int_period <- function(.fn, .x, ...) {
  switch(
    .fn,
    "is.nan" = is_nan_grates_int_period(.x),
    "is.finite" = is_finite_grates_int_period(.x),
    "is.infinite" = is_infinite_grates_int_period(.x),
    abort(sprintf("`%s()` is not supported for <grates_int_period>", .fn))
  )
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------ ARITHMETIC ------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
#' @method vec_arith grates_int_period
vec_arith.grates_int_period <- function(op, x, y, ...) {
  UseMethod("vec_arith.grates_int_period", y)
}

#' @export
#' @method vec_arith.grates_int_period default
vec_arith.grates_int_period.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}

#' @export
#' @method vec_arith.grates_int_period grates_int_period
vec_arith.grates_int_period.grates_int_period <- function(op, x, y, ...) {

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
#' @method vec_arith.grates_int_period numeric
vec_arith.grates_int_period.numeric <- function(op, x, y, ...) {
  n <- attr(x, "n")
  y <- vec_cast(y, integer())
  origin <- attr(x, "origin")
  switch(
    op,
    "+" = ,
    "-" = new_int_period(as.integer(vec_arith_base(op, x, n*y)), n = n, origin = origin),
    stop_incompatible_op(op, x, y)
  )
}

#' @export
#' @method vec_arith.numeric grates_int_period
vec_arith.numeric.grates_int_period <- function(op, x, y, ...) {
  n <- attr(y, "n")
  x <- vec_cast(x, integer())
  origin <- attr(y, "origin")
  switch(
    op,
    "+" = new_int_period(as.integer(vec_arith_base(op, n*x, y)), n = n, origin = origin),
    stop_incompatible_op(op, x, y)
  )
}

#' @export
#' @method vec_arith.grates_int_period MISSING
vec_arith.grates_int_period.MISSING <- function(op, x, y, ...) {
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
new_int_period <- function(x = integer(), n = 1L, origin = 0L) {
  vec_assert(x, ptype = integer())
  vec_assert(n, ptype = integer(), size = 1L)
  vec_assert(origin, ptype = integer(), size = 1L)
  new_vctr(x, n = n, origin = origin, class = "grates_int_period")
}

