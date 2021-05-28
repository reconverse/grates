# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------- YEARWEEK -------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' Construct a grates_yearweek object
#'
#' `yearweek()` is a constructor for a <grates_yearweek> object.
#'
#' @param x Integer vector representing the number of weeks.
#' @param firstday An integer representing the day the week starts on from 1
#'   (Monday) to 7 (Sunday).
#'
#' <grates_yearweek> objects are stored as the number of weeks (startint at 0)
#'   from the date of the specified `firstday` nearest the Unix Epoch
#'   (1970-01-01). That is, the number of seven day periods from:
#'
#'     - 1969-12-29 for `firstday` equal to 1 (Monday)
#'     - 1969-12-30 for `firstday` equal to 2 Tuesday
#'     - 1969-12-31 for `firstday` equal to 3 Wednesday
#'     - 1970-01-01 for `firstday` equal to 4 Thursday
#'     - 1970-01-02 for `firstday` equal to 5 Friday
#'     - 1970-01-03 for `firstday` equal to 6 Saturday
#'     - 1970-01-04 for `firstday` equal to 7 Sunday
#'
#' @examples
#' yearweek(0:5)
#'
#' @export
yearweek <- function(x = integer(), firstday = 1L) {

  # ensure inputs are all integer
  x <- vec_cast(x, integer())
  firstday <- vec_cast(firstday, integer())

  # ensure firstday is of length 1 and between 1 and 7
  if (!vec_is(firstday, size = 1L) || firstday < 1L || firstday > 7L) {
    abort("`firstday` must be a single integer between 1 (Monday) and 7 (Sunday) inclusive")
  }

  new_yearweek(x, firstday = firstday)
}

#' @rdname yearweek
#' @export
is_yearweek <- function(x) inherits(x, "grates_yearweek")


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------ AS_YEARWEEK ------------------------------ #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' Convert an object to grates_yearweek
#'
#' @description
#' - Date, POSIXct, and POSIXlt are converted, with the timezone respected,
#'   using [clock::as_date()].
#'
#' - Character input is parsed by two methods. Firstly, if `format = NULL` then
#'   character input is first checked to see if it is in the format of "YYYY-Www"
#'   (e.g. "2021-W03") and parsed accordingly. If it is in a different format,
#'   or `format` is non-NULL then it is parsed  using [clock::date_parse()].
#'
#' @param x An object to coerce to yearweekly.
#' @param firstday An integer representing the day the week starts on from 1
#'   (Monday) to 7 (Sunday).
#' @inheritParams clock::date_parse
#' @param ... Not currently used.
#'
#' @note Internally `grates_yearweek` objects are stored as the number of weeks
#'   from the date of the `firstday` nearest the Unix Epoch (1970-01-01).  That
#'   is:
#'
#'     - 1969-12-29 for `firstday` as Monday
#'     - 1969-12-30 for `firstday` as Tuesday
#'     - 1969-12-31 for `firstday` as Wednesday
#'     - 1970-01-01 for `firstday` as Thursday
#'     - 1970-01-02 for `firstday` as Friday
#'     - 1970-01-03 for `firstday` as Saturday
#'     - 1970-01-04 for `firstday` as Sunday
#'
#' @return A `grates_yearweek` object.
#'
#' @examples
#' as_yearweek(Sys.Date())
#' as_yearweek(as.POSIXct("2019-03-04 01:01:01", tz = "America/New_York"))
#' as_yearweek("2019-05-03", firstday = 5L)
#' as_yearweek("2021-W03", format = NULL)
#'
#' @seealso [clock::date_parse()]
#'
#' @export
as_yearweek <- function(x, firstday = 1L, ...) {
  # ensure firstday is of length 1 and between 1 and 7
  if (!vec_is(firstday, size = 1L) || firstday < 1L || firstday > 7L) {
    abort("`firstday` must be a single integer between 1 (Monday) and 7 (Sunday) inclusive")
  }

  UseMethod("as_yearweek")
}

#' @rdname as_yearweek
#' @export
as_yearweek.default <- function(x, firstday = 1L, ...) {
  firstday <- vec_cast(firstday, integer())
  vec_cast(x, new_yearweek(firstday = firstday))
}

#' @inheritParams clock::date_parse
#' @rdname as_yearweek
#' @export
as_yearweek.character <- function(x, firstday = 1L, format = "%Y-%m-%d",
                                  locale = clock_locale(), ...) {
  check_dots_empty()
  firstday <- vec_cast(firstday, integer())

  # We add an additional parsing method for a custom YYYY-Www format before
  # using clock to parse if this fails
  if (is.null(format)) {
    yrwk_pattern <- "(^\\d{4}-W([0][1-9]|[1-4][0-9]|5[0-3])$)"
    allowed <- grepl(yrwk_pattern, trimws(x))
    allowed[is.na(x)] <- TRUE
    if (all(allowed)) {
      # remove additional whitespace and parse
      dat <- trimws(x)
      out <- parse_yrwk_string(dat, firstday)
    } else {
      out <- date_parse(x, locale = locale)
    }
  }
  else {
    out <- date_parse(x, format = format, locale = locale)
  }

  if (all(is.na(out))) abort("Unable to parse any entries of x as Dates")
  vec_cast(out, new_yearweek(firstday = firstday))
}

#' @inheritParams clock::date_parse
#' @rdname as_yearweek
#' @export
as_yearweek.factor <- function(x, firstday = 1L, format = NULL,
                            locale = clock_locale(), ...) {
  check_dots_empty()
  x <- as.character(x)
  as_yearweek.character(x, firstday = firstday, format = format, locale = locale)
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------ FORMATING -------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' Format a grates_yearweek object
#'
#' @param x A grates_yearweek object.
#' @param ... Not currently used.
#'
#' @export
format.grates_yearweek <- function(x, ...) {
  if (length(x) == 0) return(character(0))
  wk <- yearweek_to_week(x)
  yr <- yearweek_to_year(x)
  out <- sprintf("%04d-W%02d", yr, wk)
  out[is.na(x)] <- NA_character_
  out
}

#' @export
vec_ptype_abbr.grates_yearweek <- function(x, ...) "yrwk"

vec_ptype_full.grates_yearweek <- function(x, ...) {
  sprintf("yearweek<fd:%d>", attr(x, "firstday"))
}

#' @export
obj_print_data.grates_yearweek <- function(x, ...) {
  print(format(x), quote = FALSE)
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------ PROTOTYPES ------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
vec_ptype2.grates_yearweek.grates_yearweek <- function(x, y, ...) {
  fdx <- attr(x, "firstday")
  fdy <- attr(y, "firstday")
  if (fdx != fdy) abort("Can't combine <grates_yearweek>'s with different `firstday`")
  new_yearweek(firstday = fdx)
}

# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------------------- CASTING -------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
vec_cast.grates_yearweek.grates_yearweek <- function(x, to, ...) {
  fdx <- attr(x, "firstday")
  fdto <- attr(to, "firstday")
  if (fdx != fdto) stop_incompatible_cast(x, to)
  x
}

#' @export
vec_cast.grates_yearweek.Date <- function(x, to, ...) {

  # Ensure no fractional days
  x <- as.integer(trunc(x))

  # calculate the week number (relative to firstday nearest unix epoch)
  firstday <- attr(to, "firstday")
  weeknumber <- (x + 4 - firstday) %/% 7
  new_yearweek(as.integer(weeknumber), firstday = firstday)
}

#' @export
vec_cast.Date.grates_yearweek <- function(x, to, ...) {
  firstday <- attr(x, "firstday")
  weeknumber <- unclass(x)
  new_date((7 * weeknumber) + (firstday - 4))
}

#' @export
vec_cast.grates_yearweek.POSIXct <- function(x, to, ...) {
  x <- as_date(x)
  firstday <- attr(to, "firstday")
  vec_cast.grates_yearweek.Date(x, new_yearweek(firstday = firstday))
}

#' @export
vec_cast.grates_yearweek.POSIXlt <- vec_cast.grates_yearweek.POSIXct

#' @export
vec_cast.POSIXct.grates_yearweek <- function(x, to, ...) {
  out <- vec_cast.Date.grates_yearweek(x, to)
  tz <- date_zone(to)
  out <- as_zoned_time(out, zone = tz, nonexistent = "roll-forward", ambiguous = "latest")
  as_date_time(out)
}

#' @export
vec_cast.POSIXlt.grates_yearweek <- function(x, to, ...) {
  out <- vec_cast.Date.grates_yearweek(x, to)
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
as.character.grates_yearweek <- function(x, ...) {
  check_dots_empty()
  format(x)
}

#' @export
as.list.grates_yearweek <- function(x, ...) {
  check_dots_empty()
  firstday <- attr(x, "firstday")
  out <- lapply(unclass(x), new_yearweek, firstday = firstday)
  setNames(out, names(x))
}

#' @export
as.double.grates_yearweek <- function(x, ...) {
  check_dots_empty()
  out <- as.double(unclass(x))
  setNames(out, names(x))
}

#' @export
as.numeric.grates_yearweek <- function(x, ...) {
  check_dots_empty()
  out <- as.numeric(unclass(x))
  setNames(out, names(x))
}

#' @export
as.integer.grates_yearweek <- function(x, ...) {
  check_dots_empty()
  out <- as.integer(unclass(x))
  setNames(out, names(x))
}

#' @export
as.data.frame.grates_yearweek <- as.data.frame.vector


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# --------------------------------- MATH ---------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

is_nan_grates_yearweek <- function(x) vector("logical", vec_size(x))

is_finite_grates_yearweek <- function(x) !vec_equal_na(x)

is_infinite_grates_yearweek <- function(x) vector("logical", vec_size(x))

#' @export
#' @method vec_math grates_yearweek
vec_math.grates_yearweek <- function(.fn, .x, ...) {
  switch(
    .fn,
    "is.nan" = is_nan_grates_yearweek(.x),
    "is.finite" = is_finite_grates_yearweek(.x),
    "is.infinite" = is_infinite_grates_yearweek(.x),
    abort(sprintf("`%s()` is not supported for <grates_yearweek>", .fn))
  )
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# --------------------------------- MATH ---------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

is_nan_grates_yearweek <- function(x) vector("logical", vec_size(x))

is_finite_grates_yearweek <- function(x) !vec_equal_na(x)

is_infinite_grates_yearweek <- function(x) vector("logical", vec_size(x))

#' @export
#' @method vec_math grates_yearweek
vec_math.grates_yearweek <- function(.fn, .x, ...) {
  switch(
    .fn,
    "is.nan" = is_nan_grates_yearweek(.x),
    "is.finite" = is_finite_grates_yearweek(.x),
    "is.infinite" = is_infinite_grates_yearweek(.x),
    abort(sprintf("`%s()` is not supported for <grates_yearweek>", .fn))
  )
}

#' @export
quantile.grates_yearweek <- function(x, type = 1, ...) {
  firstday <- attr(x, "firstday")
  weeks <- as.integer(quantile(unclass(x), type = type, ...))
  new_yearweek(weeks, firstday = firstday)
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------ ARITHMETIC ------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
#' @method vec_arith grates_yearweek
vec_arith.grates_yearweek <- function(op, x, y, ...) {
  UseMethod("vec_arith.grates_yearweek", y)
}

#' @export
#' @method vec_arith.grates_yearweek default
vec_arith.grates_yearweek.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}

#' @export
#' @method vec_arith.grates_yearweek grates_yearweek
vec_arith.grates_yearweek.grates_yearweek <- function(op, x, y, ...) {

  if (attr(x, "firstday") != attr(y, "firstday")) stop_incompatible_type(x, y)
  switch(
    op,
    "-" = vec_arith_base(op, x, y),
    stop_incompatible_op(op, x, y)
  )
}

#' @export
#' @method vec_arith.grates_yearweek numeric
vec_arith.grates_yearweek.numeric <- function(op, x, y, ...) {
  firstday <- attr(x, "firstday")
  y <- vec_cast(y, integer())
  switch(
    op,
    "+" = ,
    "-" = new_yearweek(as.integer(vec_arith_base(op, x, y)), firstday = firstday),
    stop_incompatible_op(op, x, y)
  )
}

#' @export
#' @method vec_arith.numeric grates_yearweek
vec_arith.numeric.grates_yearweek <- function(op, x, y, ...) {
  firstday <- attr(y, "firstday")
  x <- vec_cast(x, integer())
  switch(
    op,
    "+" = new_yearweek(as.integer(vec_arith_base(op, x, y)), firstday = firstday),
    stop_incompatible_op(op, x, y)
  )
}

#' @export
#' @method vec_arith.grates_yearweek MISSING
vec_arith.grates_yearweek.MISSING <- function(op, x, y, ...) {
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

new_yearweek <- function(x = integer(), firstday = 1L) {
  vec_assert(x, ptype = integer())
  vec_assert(firstday, ptype = integer(), size = 1L)
  new_vctr(x, firstday = firstday, class = "grates_yearweek")
}

yearweek_to_week <- function(x) {
  firstday <- attr(x, "firstday")
  weeknumber <- as.numeric(x)
  x <- new_date((7 * weeknumber) + (firstday - 4))
  midweek <- x + 3
  seven_day_week_in_year(date = midweek)
}

yearweek_to_year <- function(x) {
  firstday <- attr(x, "firstday")
  weeknumber <- as.numeric(x)
  x <- new_date((7 * weeknumber) + (firstday - 4))

  # calculate week
  midweek <- x + 3
  week <- seven_day_week_in_year(date = midweek)

  # calculate year (
  dat <- as_utc_posixlt_from_int(x)
  december <- dat$mon == 11L
  january <- dat$mon == 0L
  boundary_adjustment <- integer(length(x)) # h/t Zhian Kamvar for boundary adjustment idea in aweek)
  boundary_adjustment[january  & week >= 52] <- -1L
  boundary_adjustment[december & week == 1]  <- 1L
  yr = dat$year + 1900L
  yr + boundary_adjustment
}

seven_day_week_in_year <- function(date) {
  x <- as_utc_posixlt_from_int(date)
  yr <- x$year + 1900L
  jan1 <- date_build(year = yr, invalid = "error")
  res <- 1 + (unclass(date) - unclass(jan1)) %/% 7
  attributes(res) <- NULL
  res
}

parse_yrwk_string <- function(x, firstday) {

  # pull out the year and week from string
  year <- as.integer(substr(x, 1, 4))
  week <- as.integer(substr(x, 7, 8))

  # check weeks are valid relative to years
  cond <- (week > last_week_in_year(year = year, firstday = firstday))
  if (any(cond, na.rm = TRUE)) {
    idx <- which(cond)
    abort(c(
      "Some weeks are invalid for the given week_start",
      i = sprintf("The first invalid year-week combination is %d-%d", year[idx], week[idx])
    ))
  }

  numeric_yrwk_to_date(year = year, week = week, firstday = firstday)
}


last_week_in_year <- function(year = integer(), firstday = 1L) {
  x <- as.Date(sprintf("%d-12-28", year))
  wday <- strptime(sprintf("%d-12-28", year), format="%Y-%m-%d", tz = "UTC")$wday
  wday <- 1L + (wday + (7L - firstday)) %% 7L
  midweek <- x + (4L - wday)
  seven_day_week_in_year(date = midweek)
}

numeric_yrwk_to_date <- function(year = integer(), week = integer(), firstday = integer()) {
  jan4 <- strptime(sprintf("%d-01-04", year), format="%Y-%m-%d", tz = "UTC")
  wday <- jan4$wday
  out <- jan4 - ((wday + 7L - firstday) %% 7) * 86400
  out <- out + (week - 1) * 7L * 86400
  as.Date(out)
}
