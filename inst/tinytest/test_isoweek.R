dates <- seq.Date(from = as.Date("1969-12-29"), length.out = 4, by = "week")
storage.mode(dates) <- "double" # needed for R <= 4.1.3
yw4 <- new_isoweek(0:3)

# constructor -------------------------------------------------------------
expect_identical(as.Date(yw4), dates)

# formatting --------------------------------------------------------------
expect_identical(format(yw4), c("1970-W01", "1970-W02", "1970-W03", "1970-W04"))
expect_identical(format(new_isoweek()), character())

# pre-epoch dates ---------------------------------------------------------
dates <- seq.Date(from = as.Date("1900-01-04"), length.out = 4, by = "week")
storage.mode(dates) <- "double" # needed for R <= 4.1.3
dates2 <- seq.Date(from = as.Date("1900-01-04") - 28L, length.out = 4, by = "week")
storage.mode(dates2) <- "double" # needed for R <= 4.1.3
expect_identical(as.Date(as_isoweek(dates)), dates - 3L)
expect_identical(as.Date(as_isoweek(dates) - 4L), dates2 - 3L)

# january 1 dates ---------------------------------------------------------
# Slightly modified version of tests from Zhian Kamvar's aweek package.
# This checks that the tricky behaviour at the end/beginning of the year is
# handled correctly.

yrs <- 2001:2019
dates <- sprintf("%d-01-01", yrs)
dates <- as.Date(c(dates, NA_character_))

# ISO week (firstday monday)
dates_iso    <- as_isoweek(dates)
expected <- c(
    "2001-W01", "2002-W01", "2003-W01", "2004-W01", "2004-W53",
    "2005-W52", "2007-W01", "2008-W01", "2009-W01", "2009-W53",
    "2010-W52", "2011-W52", "2013-W01", "2014-W01", "2015-W01",
    "2015-W53", "2016-W52", "2018-W01", "2019-W01", NA
)
expect_identical(as.character(dates_iso), expected)

# POSIXlt -----------------------------------------------------------------
nz <- as.POSIXlt("2021-01-03 02:00:00", tz = "NZ")
utc <- as.POSIXlt("2020-12-28 00:00:00", tz = "UTC")
result <- as.POSIXlt(as_isoweek(nz))
expect_identical(result, utc)

pos <- as.POSIXlt("2021-01-04", tz = "UTC")
expect_identical(as.Date(as_isoweek(pos)), as.Date("2021-01-04"))

# POSIXct -----------------------------------------------------------------
nz <- as.POSIXct(as.POSIXlt("2021-01-03", tz = "NZ"))
dat <- as_isoweek(nz)
result <- as.POSIXct(dat)
expect_identical(result, as.POSIXct(as.POSIXlt("2020-12-28", tz = "UTC")))

dat <- "2021-01-04"
pos <- as.POSIXct(as.POSIXlt("2021-01-04", tz = "UTC"))
res7 <- as_isoweek(pos)
expect_identical(as.Date(res7), as.Date("2021-01-04"))

# character ---------------------------------------------------------------
dat <- "2021-01-04"
res <- as_isoweek(dat)
expect_identical(as.Date(res), as.Date(dat))

dat <- as.factor("2021-01-04")
res <- as_isoweek(dat)
expect_identical(as.Date(res), as.Date(dat))


# as_isoweek errors -------------------------------------------------------
expect_error(as_isoweek("bob"))

expect_error(
    as_isoweek(TRUE),
    "Not implemented for class [logical].",
    fixed = TRUE
)

# as.POSIXct --------------------------------------------------------------
dat <- "2021-01-04"
res <- as.POSIXct(as_isoweek(dat))
expect_identical(res, as.POSIXct(as.POSIXlt(dat), tz = "UTC"))
expect_error(
    as.POSIXct(as_isoweek(dat), tz = "GMT"),
    "<grates_isoweek> objects can only be converted to UTC. If other timezones are required, first convert to <Date> and then proceed as desired.",
    fixed = TRUE
)

# as.POSIXlt --------------------------------------------------------------
dat <- "2021-01-04"
res <- as.POSIXlt(as_isoweek(dat))
expect_inherits(res, "POSIXlt")
expect_identical(julian(res), julian(as.POSIXlt("2021-01-04", tz = "UTC")))
expect_error(
    as.POSIXlt(as_isoweek(dat), tz = "GMT"),
    "<grates_isoweek> objects can only be converted to UTC. If other timezones are required, first convert to <Date> and then proceed as desired.",
    fixed = TRUE
)

# as.character ------------------------------------------------------------
dat <- "2020-12-28"
res <- as.character(as_isoweek(dat))
expect_identical(res, "2020-W53")


# as.list -----------------------------------------------------------------
dat <- as_isoweek(c("2020-12-28", "2021-01-04"))
res <- list(as_isoweek("2020-12-28"), as_isoweek("2021-01-04"))
expect_identical(res, as.list(dat))

# accessors ---------------------------------------------------------------
dat <- as_isoweek(as.Date("2020-12-28"))
expect_identical(get_year(dat), 2020L)
expect_identical(get_week(dat), 53)
expect_error(get_year("bob"))
expect_error(get_week("bob"))


# is_yearweek -----------------------------------------------------------------
dat <- as_isoweek(Sys.Date())
expect_true(is_isoweek(dat))
expect_false(is_yearweek("bob"))

# subsetting --------------------------------------------------------------
x <- Sys.Date()
dat <- as_isoweek(x) + 0:1
dat <- setNames(dat, c("a", "b"))
expect_identical(dat[1], c(a=as_isoweek(x)))
expect_identical(dat[[2]], as_isoweek(x + 7))
dat[1] <- as_isoweek(x+14)
expect_identical(dat[1], c(a=as_isoweek(x + 14)))
dat[[2]] <- dat[[1]]
expect_identical(dat[[2]], dat[[1]])
expect_error(
    dat[1] <- "bob",
    "Can only assign <grates_isoweek> objects in to an <grates_isoweek> object.",
    fixed = TRUE
)
expect_error(
    dat[1] <- as_yearweek(x, 7),
    "Can only assign <grates_isoweek> objects in to an <grates_isoweek> object.",
    fixed = TRUE
)

# combine -----------------------------------------------------------------
x <- Sys.Date()
dat <- as_isoweek(x)
expect_identical(c(dat, dat), as_isoweek(c(x, x)))
dat2 <- as_yearweek(x, 7)
expect_error(
    c(dat, "bob"),
    "Unable to combine <grates_isoweek> objects with other classes.",
    fixed = TRUE
)
expect_error(
    c(dat, dat2),
    "Unable to combine <grates_isoweek> objects with other classes.",
    fixed = TRUE
)


# comparison operators ----------------------------------------------------
x <- Sys.Date()
dat <- as_isoweek(x)
expect_true(dat == dat)
expect_false(dat != dat)
expect_true(dat <= dat + 1)
expect_true(dat >= dat - 1)
expect_true(dat < dat + 1)
expect_true(dat > dat - 1)
expect_true(dat != dat + 1)
expect_error(
    dat == 1,
    "Can only compare <grates_isoweek> objects with <grates_isoweek> objects.",
    fixed = TRUE
)

# addition ----------------------------------------------------------------
x <- as.Date("2021-01-05")
y <- as.Date("2021-01-04")
dat1 <- as_isoweek(x)
dat2 <- dat1 + 0:1
expect_identical(as.Date(dat2), c(y, y + 7))
expect_identical(dat2, 0:1 + dat1)
expect_identical(+dat1, dat1)
expect_error(
    dat1 + 1.5,
    "Can only add integers to <grates_isoweek> objects.",
    fixed = TRUE
)
expect_error(
    dat1 + dat1,
    "Cannot add <grates_isoweek> objects to each other.",
    fixed = TRUE
)

# subtraction -------------------------------------------------------------
x <- as.Date("2021-01-05")
y <- as.Date("2021-01-04")
dat1 <- as_isoweek(x)
dat2 <- dat1 - 0:1
expect_identical(as.Date(dat2), c(y, y - 7))
expect_identical(as.integer(dat2 - dat1), c(0L, -1L))
expect_error(
    1 - dat1,
    "Can only subtract from a <grates_isoweek> object, not vice-versa.",
    fixed = TRUE
)
expect_error(
    -dat1,
    "Cannot negate a <grates_isoweek> object.",
    fixed = TRUE
)
expect_error(
    dat1 - 1.5,
    "Can only subtract whole numbers and other <grates_isoweek> objects from <grates_isoweek> objects.",
    fixed = TRUE
)

# Other operations error
x <- as_isoweek(as.Date("2021-01-05"))
expect_error(dat * 3)
expect_error(dat / 3)
expect_error(dat ^ 3)
expect_error(dat %% 3)
expect_error(dat %/% 3)
expect_error(dat & 3)
expect_error(dat | 3)
expect_error(!dat)

# Math
x <- as_isoweek(as.Date("2021-01-04"))
dat <- c(x + 0:1, new_isoweek(NA_integer_))
expect_identical(is.nan(dat), c(FALSE, FALSE, FALSE))
expect_identical(is.finite(dat), c(TRUE, TRUE, FALSE))
expect_identical(is.infinite(dat), c(FALSE, FALSE, FALSE))
expect_error(abs(dat))

# miscellaneous -----------------------------------------------------------
expect_identical(new_isoweek(-1.5), new_isoweek(-2L))
expect_error(new_isoweek("bob"), "`x` must be integer.", fixed = TRUE)
dat <- Sys.Date()
dat <- c(dat, dat - 45L)
dat <- as_isoweek(dat)
expect_identical(rep(dat, 2L), c(dat, dat))
expect_identical(rep(dat, each = 2L), c(dat[[1]], dat[[1]], dat[[2]], dat[[2]]))
expect_identical(unique(c(dat, dat)), dat)
dat <- as_isoweek(as.Date("1970-01-04"))
expect_identical(
    seq(dat, dat + 6L, by = 2L),
    new_isoweek(c(0L, 2L, 4L, 6L))
)
expect_error(
    seq(dat, dat + 11, by = 2.5),
    "`by` must be an integer of length 1.",
    fixed = TRUE
)

expect_error(
    seq(dat, as.integer(dat + 11), by = 2.5),
    "`to` must be a <grates_isoweek> object of length 1.",
    fixed = TRUE
)
expect_identical(as.integer(new_isoweek(100L)), 100L)
expect_identical(as.double(new_isoweek(100L)), 100)
expect_identical(min(c(dat, dat+11)), dat)
expect_identical(max(c(dat, dat+11)), dat+11)
expect_identical(range(seq(dat, dat + 12, by = 2L)), c(dat, dat+12))
expect_error(
    any(dat),
    "`any()` is not supported for <grates_isoweek> objects.",
    fixed = TRUE
)
