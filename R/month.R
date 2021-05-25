# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# --------------------------------- MONTH --------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' Construct a grates_month object
#'
#' `month()` is a constructor for a <grates_month> object
#'
#' @param x Integer vector representing the number of months since the Unix
#'   epoch (1970-01-01).
#' @param n Number of months that are being grouped by (default 1).
#' @param origin Month since the Unix epoch where grouping begins (default 0).
#'
#' @examples
#' month(1)
#' month(c(4, 7), n = 3, origin = 1)
#'
#' @references The algorithm to convert between dates and months relative to the
#'   UNIX Epoch comes from the work of Davis Vaughan in the unreleased
#'   [datea](https://github.com/DavisVaughan/datea/) package.
#'
#' @export
month <- function(x = integer(), n = 1L, origin = 0L) {

  # ensure inputs are all integer
  x <- vec_cast(x, integer())
  n <- vec_cast(n, integer())
  origin <- vec_cast(origin, integer())

  stopifnot("`n` must be >= 1" = n > 0)

  # check that the input data is compatible with the specified n
  if (!is_valid_month_interval(x, n)) {
    abort("`n` not compatible with specified data `x`")
  }

  # check that origin is compatible with with combination of x and n
  origin <- origin %% n
  if(!all(x %% n == origin)) {
    abort("`origin` not compatible with specified `x` and `n")
  }

  new_month(x, n = n, origin = origin)
}

#' @rdname month
#' @export
is_month <- function(x) inherits(x, "grates_month")



# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------- AS_MONTH -------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' Convert an object to grates_month object
#'
#' - Date, POSIXct, and POSIXlt are converted, with the timezone respected,
#'   using [clock::as_date()].
#' - Character input is parsed using [clock::date_parse()].
#'
#' @param x An object to convert.
#' @param n Number of months that are being grouped by (default 1).
#' @param origin Month since the Unix epoch where grouping begins (default 0).
#' @param ... Not currently used.
#'
#' @return A `grates_month` object.
#'
#' @examples
#' as_month(Sys.Date())
#' as_month(as.POSIXct("2019-03-04 01:01:01", tz = "America/New_York"), interval = 2)
#' as_month("2019-05-03")
#'
#' @note Internally `grates_month` objects are stored as the number of months
#'   (starting at 0) since the Unix Epoch (1970-01-01) to the earliest month in
#'   the grouping. Precision is only to the month level (i.e. the day of the
#'   month is always dropped).
#'
#' @references The algorithm to convert between dates and months relative to the
#'   UNIX Epoch comes from the work of Davis Vaughan in the unreleased
#'   [datea](https://github.com/DavisVaughan/datea/) package.
#'
#' @export
as_month <- function(x, ...) {
  UseMethod("as_month")
}

#' @rdname as_month
#' @export
as_month.default <- function(x, n = 1L, origin = 0L, ...) {
  n <- vec_cast(n, integer())
  origin <- vec_cast(origin, integer())
  vec_cast(x, new_month(n = n, origin = origin))
}

#' @inheritParams clock::date_parse
#' @rdname as_month
#' @export
as_month.character <- function(x, n = 1L, origin = 0L, format = NULL,
                               locale = clock_locale(), ...) {
  check_dots_empty()
  n <- vec_cast(n, integer())
  origin <- vec_cast(origin, integer())
  x <- date_parse(x, format = format, locale = locale)
  if (all(is.na(x))) abort("Unable to parse any entries of x as Dates")
  vec_cast(x, new_month(n = n, origin = origin))
}

#' @inheritParams clock::date_parse
#' @rdname as_month
#' @export
as_month.factor <- function(x, n = 1L, origin = 0L, format = NULL,
                            locale = clock_locale(), ...) {
  check_dots_empty()
  x <- as.character(x)
  as_month.character(x,n = n, origin = origin, format = format, locale = locale)
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------ FORMATING -------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' Format a grates_month object
#'
#' @param x A grates_month object.
#' @param format The format to use for the bounds of each grates_month entry.
#' @param sep Where more than one month is grouped with others, `sep` is placed
#'   between the upper and lower bounds when printing.
#' @param ... Not currently used.
#'
#' @export
format.grates_month <- function(x, format = "%Y-%b", sep = "to", ...) {
  if (length(x) == 0) return(character(0))
  n <- attr(x, "n")
  if (n > 1) {
    out <- sprintf(
      "%s %s %s",
      format.Date(vec_cast(x, new_date(0)), format = format),
      sep,
      format.Date(vec_cast(x + 1, new_date(0)) - 1, format = format)
    )
  } else {
    out <- format.Date(vec_cast(x, new_date(0)), format = format)
  }
  out[is.na(x)] <- NA_character_
  out
}

#' @export
vec_ptype_abbr.grates_month <- function(x, ...) "mnth"

#vec_ptype_full.grates_month <- function(x, ...) sprintf("month<%d>", attr(x, "n"))

#' @export
obj_print_data.grates_month <- function(x, format = "%Y-%b", sep = "to", ...) {
  print(format(x, format = format, sep = sep), quote = FALSE)
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------ PROTOTYPES ------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
vec_ptype2.grates_month.grates_month <- function(x, y, ...) {
  # check compatibility of n
  nx <- attr(x, "n")
  ny <- attr(y, "n")
  if (nx != ny) abort("Can't combine <grates_month>'s with different `n`")

  # check compatibility of the origin
  ox <- attr(x, "origin")
  oy <- attr(y, "origin")
  if (ox != oy) abort("Can't combine <grates_month>'s with different `origin`")

  # prototype
  new_month(n = nx, origin = ox)
}

# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------------------- CASTING -------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
vec_cast.grates_month.grates_month <- function(x, to, ...) {
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
vec_cast.grates_month.Date <- function(x, to, ...) {

  # floor to start of month
  x <- date_group(x, precision = "month", n = 1L)

  # convert to posixlt
  x <- as_utc_posixlt_from_int(x)

  # calculate the year
  yr <- x$year + 1900L

  # calculate the month relative to unix epoch
  mon <- (yr - 1970L) * 12L + x$mon

  # Adjust the origin for dates around Unix Epoch or prior
  n <- attr(to, "n")
  origin <- attr(to, "origin") %% n
  min <- min(mon, na.rm = TRUE)
  while(min < origin) {
    origin <- origin - n
  }

  # generate sequence of month groupings staring at origin
  months <- seq(from = origin, to = max(mon, na.rm = TRUE), by = n)
  idx <- cut(mon, breaks = c(months, Inf), labels = FALSE, right = FALSE)
  out <- months[idx]

  # rescale origin modulo n
  origin <- min(out, na.rm = TRUE) %% n

  new_month(out, n = n, origin = origin)
}

#' @export
vec_cast.Date.grates_month <- function(x, to, ...) {
  days <- month_to_days(unclass(x))
  new_date(days)
}

#' @export
vec_cast.grates_month.POSIXct <- function(x, to, ...) {
  x <- as_date(x)
  n <- attr(to, "n")
  origin <- attr(to, "origin")
  vec_cast.grates_month.Date(x, new_month(n = n, origin = origin))
}

#' @export
vec_cast.grates_month.POSIXlt <- vec_cast.grates_month.POSIXct

#' @export
vec_cast.POSIXct.grates_month <- function(x, to, ...) {
  out <- vec_cast.Date.grates_month(x, to)
  tz <- date_zone(to)
  out <- as_zoned_time(out, zone = tz, nonexistent = "roll-forward", ambiguous = "latest")
  as_date_time(out)
}

#' @export
vec_cast.POSIXlt.grates_month <- function(x, to, ...) {
  out <- vec_cast.Date.grates_month(x, to)
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
as.character.grates_month <- function(x, format = "%Y-%b", sep = "to", ...) {
  check_dots_empty()
  format(x, format = format, sep = sep)
}

#' @export
as.list.grates_month <- function(x, ...) {
  check_dots_empty()
  n <- attr(x, "n")
  origin <- attr(x, "origin")
  out <- lapply(unclass(x), new_month, n = n, origin = origin)
  setNames(out, names(x))
}

#' @export
as.double.grates_month <- function(x, ...) {
  check_dots_empty()
  out <- as.double(unclass(x))
  setNames(out, names(x))
}

#' @export
as.numeric.grates_month <- function(x, ...) {
  check_dots_empty()
  out <- as.numeric(unclass(x))
  setNames(out, names(x))
}

#' @export
as.integer.grates_month <- function(x, ...) {
  check_dots_empty()
  out <- as.integer(unclass(x))
  unclass(x)
}

#' @export
as.data.frame.grates_month <- as.data.frame.vector


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# --------------------------------- MATH ---------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

is_nan_grates_month <- function(x) vector("logical", vec_size(x))

is_finite_grates_month <- function(x) !vec_equal_na(x)

is_infinite_grates_month <- function(x) vector("logical", vec_size(x))

#' @export
#' @method vec_math grates_month
vec_math.grates_month <- function(.fn, .x, ...) {
  switch(
    .fn,
    "is.nan" = is_nan_grates_month(.x),
    "is.finite" = is_finite_grates_month(.x),
    "is.infinite" = is_infinite_grates_month(.x),
    abort(sprintf("`%s()` is not supported for <grates_month>", .fn))
  )
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------ ARITHMETIC ------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
#' @method vec_arith grates_month
vec_arith.grates_month <- function(op, x, y, ...) {
  UseMethod("vec_arith.grates_month", y)
}

#' @export
#' @method vec_arith.grates_month default
vec_arith.grates_month.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}

#' @export
#' @method vec_arith.grates_month grates_month
vec_arith.grates_month.grates_month <- function(op, x, y, ...) {

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
#' @method vec_arith.grates_month numeric
vec_arith.grates_month.numeric <- function(op, x, y, ...) {
  n <- attr(x, "n")
  y <- vec_cast(y, integer())
  origin <- attr(x, "origin")
  switch(
    op,
    "+" = ,
    "-" = new_month(as.integer(vec_arith_base(op, x, n*y)), n = n, origin = origin),
    stop_incompatible_op(op, x, y)
  )
}

#' @export
#' @method vec_arith.numeric grates_month
vec_arith.numeric.grates_month <- function(op, x, y, ...) {
  n <- attr(y, "n")
  x <- vec_cast(x, integer())
  origin <- attr(y, "origin")
  switch(
    op,
    "+" = new_month(as.integer(vec_arith_base(op, n*x, y)), n = n, origin = origin),
    stop_incompatible_op(op, x, y)
  )
}

#' @export
#' @method vec_arith.grates_month MISSING
vec_arith.grates_month.MISSING <- function(op, x, y, ...) {
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
new_month <- function(x = integer(), n = 1L, origin = 0L) {
  vec_assert(x, ptype = integer())
  vec_assert(n, ptype = integer(), size = 1L)
  vec_assert(origin, ptype = integer(), size = 1L)
  new_vctr(x, n = n, origin = origin, class = "grates_month")
}

delayedAssign(
  "DAYS_BEFORE_MONTH",
  c(0L, 31L, 59L, 90L, 120L, 151L, 181L, 212L, 243L, 273L, 304L, 334L)
)

is_valid_month_interval <- function(x = integer(), n = integer()) {
  x <- x[!is.na(x)]
  if (length(x)) all(diff(x) %% n == 0) else TRUE
}

month_to_days <- function(month) {
  year <- month %/% 12L + 1970L
  month <- month %% 12L + 1L
  days_before_year(year) + days_before_month(year, month) - 719162L
}

days_before_year <- function(year = integer()) {
  year <- year - 1L
  (year * 365) + (year %/% 4) - (year %/% 100) + (year %/% 400)
}

days_before_month <- function(year, month) {
  DAYS_BEFORE_MONTH[month] + ((month > 2) & is_leap_year(year))
}

is_leap_year <- function(year) {
  ((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0)
}
