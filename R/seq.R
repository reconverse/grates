# TODO - these implement a very basic set of seq functions. They are a
# temporary implementation to fix a problem with incidence2. They will be
# improved in a future release.

#' @export
seq.grates_month <- function(from, to, ...) {
  if (!inherits(to, "grates_month")) {
    abort("`to` must be a grates_month object")
  }
  n <- attr(from, "n")
  origin <- attr(from, "origin")
  to <- vec_cast(to, from)
  from <- as.integer(from)
  to <- as.integer(to)
  out <- seq.int(from = from, to = to, by = n)
  new_month(out, n = n, origin = origin)
}

#' @export
seq.grates_year <- function(from, to, ...) {
  if (!inherits(to, "grates_year")) {
    abort("`to` must be a grates_year object")
  }
  out <- NextMethod()
  new_year(out)
}

#' @export
seq.grates_quarter <- function(from, to, ...) {
  if (!inherits(to, "grates_quarter")) {
    abort("`to` must be a grates_year object")
  }
  out <- NextMethod()
  new_quarter(out)
}

#' @export
seq.grates_yearweek <- function(from, to, ...) {
  if (!inherits(to, "grates_yearweek")) {
    abort("`to` must be a grates_yearweek object")
  }
  firstday <- attr(from, "firstday")
  to <- vec_cast(to, from) # will error if not possible
  out <- NextMethod()
  new_yearweek(out, firstday = firstday)
}

#' @export
seq.grates_period <- function(from, to, ...) {
  if (!inherits(to, "grates_period")) {
    abort("`to` must be a grates_period object")
  }
  n <- attr(from, "n")
  origin <- attr(from, "origin")
  to <- vec_cast(to, from)
  from <- as.integer(from)
  to <- as.integer(to)
  out <- seq.int(from = from, to = to, by = n)
  new_period(out, n = n, origin = origin)
}

