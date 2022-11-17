dates <- seq.Date(from = as.Date("1970-01-01"), length.out = 4, by = "month")
storage.mode(dates) <- "double" # needed for R <= 4.1.3
ym4 <- yearmonth(0:3)

# constructor -------------------------------------------------------------
expect_identical(as.Date(ym4), dates)

# formatting --------------------------------------------------------------
expect_identical(format(ym4), format(as.Date(ym4), "%Y-%b"))
expect_identical(format(yearmonth()), character())

# pre-epoch dates ---------------------------------------------------------
dates <- seq.Date(from = as.Date("1900-01-01"), length.out = 4, by = "month")
storage.mode(dates) <- "double" # needed for R <= 4.1.3
dates2 <- seq.Date(from = as.Date("1899-09-01"), length.out = 4, by = "month")
storage.mode(dates2) <- "double" # needed for R <= 4.1.3
expect_identical(as.Date(as_yearmonth(dates)), dates)
expect_identical(as.Date(as_yearmonth(dates) - 4), dates2)

# POSIXlt -----------------------------------------------------------------
nz <- as.POSIXlt("2021-01-04 02:00:00", tz = "NZ")
utc <- as.POSIXlt("2021-01-01 00:00:00", tz = "UTC")
result <- as.POSIXlt(as_yearmonth(nz))

expect_identical(result, utc)

dat <- "2021-01-01"
pos <- as.POSIXlt("2021-01-04", tz = "UTC")
res1 <- as_yearmonth(pos)

expect_identical(as.Date(res1), as.Date(dat))


# POSIXct -----------------------------------------------------------------
nz <- as.POSIXct(as.POSIXlt("2021-01-04", tz = "NZ"))
dat <- as_yearmonth(nz)
result <- as.POSIXct(dat)
expect_identical(result, as.POSIXct(as.POSIXlt("2021-01-01", tz = "UTC")))

pos <- as.POSIXct(as.POSIXlt("2021-01-04", tz = "UTC"))
res1 <- as_yearmonth(pos)
expect_identical(as.Date(res1), as.Date("2021-01-01"))


# character ---------------------------------------------------------------
dat <- "2021-01-04"
res <- as_yearmonth(dat)
expect_identical(as.Date(res), as.Date("2021-01-01"))

dat <- as.factor("2021-01-04")
res <- as_yearmonth(dat)
expect_identical(as.Date(res), as.Date("2021-01-01"))


# as_yearmon errors ------------------------------------------------------
expect_error(
    as_yearmonth(TRUE),
    "Not implemented for class [logical].",
    fixed = TRUE
)
expect_error(as_yearmonth("bob"))


# as.POSIXct --------------------------------------------------------------
dat <- "2021-01-01"
res <- as.POSIXct(as_yearmonth(dat))
expect_identical(res, as.POSIXct(as.POSIXlt(dat), tz = "UTC"))
expect_error(
    as.POSIXct(as_yearmonth(dat), tz = "GMT"),
    "<grates_yearmonth> objects can only be converted to UTC. If other timezones are required, first convert to <Date> and then proceed as desired.",
    fixed = TRUE
)

# as.POSIXlt --------------------------------------------------------------
dat <- "2021-01-01"
res <- as.POSIXlt(as_yearmonth(dat))
expect_inherits(res, "POSIXlt")
expect_identical(julian(res), julian(as.POSIXlt(dat, tz = "UTC")))
expect_error(
    as.POSIXlt(as_yearmonth(dat), tz = "GMT"),
    "<grates_yearmonth> objects can only be converted to UTC. If other timezones are required, first convert to <Date> and then proceed as desired.",
    fixed = TRUE
)

# as.character ------------------------------------------------------------
dat <- "2020-12-28"
res <- as.character(as_yearmonth(dat))
expect_identical(res, as.character(format(as.Date(dat), format = "%Y-%b")))


# as.list -----------------------------------------------------------------
dat <- as_yearmonth(c("2020-12-28", "2021-01-04"))
res <- list(as_yearmonth("2020-12-28"), as_yearmonth("2021-01-04"))
expect_identical(res, as.list(dat))

# accessors ---------------------------------------------------------------
dat <- as_yearmonth(as.Date("2020-12-28"))
expect_identical(get_year(dat), 2020L)

# is_yearmon -----------------------------------------------------------------
dat <- as_yearmonth(Sys.Date())
expect_true(is_yearmonth(dat))
expect_false(is_yearmonth("bob"))

# subsetting --------------------------------------------------------------
x <- Sys.Date()
dat <- as_yearmonth(x) + 0:1
dat <- setNames(dat, c("a", "b"))
expect_identical(dat[1], c(a=as_yearmonth(x)))
expect_identical(dat[[2]], as_yearmonth(x + 33))
dat[1] <- as_yearmonth(x+33)
expect_identical(dat[1], c(a=as_yearmonth(x + 33)))
dat[[2]] <- dat[[1]]
expect_identical(dat[[2]], dat[[1]])
expect_error(
    dat[1] <- "bob",
    "Can only assign <grates_yearmonth> objects in to an <grates_yearmonth> object.",
    fixed = TRUE
)


# combine -----------------------------------------------------------------
x <- Sys.Date()
dat <- as_yearmonth(x)
expect_identical(c(dat, dat), as_yearmonth(c(x, x)))
expect_error(
    c(dat, "bob"),
    "Unable to combine <grates_yearmonth> objects with other classes.",
    fixed = TRUE
)

# comparison operators ----------------------------------------------------
x <- Sys.Date()
dat <- as_yearmonth(x)
expect_true(dat == dat)
expect_false(dat != dat)
expect_true(dat == dat)
expect_true(dat <= dat + 1)
expect_true(dat >= dat - 1)
expect_true(dat < dat + 1)
expect_true(dat > dat - 1)
expect_true(dat != dat + 1)
expect_error(
    dat == TRUE,
    "Can only compare <grates_yearmonth> objects with <grates_yearmonth> objects.",
    fixed = TRUE
)

# addition ----------------------------------------------------------------
x <- as.Date("2021-01-05")
y <- as.Date("2021-01-04")
dat1 <- as_yearmonth(x)
dat2 <- dat1 + 0:1
expect_identical(as.Date(dat2), as.Date(c("2021-01-01", "2021-02-01")))
expect_identical(dat2, 0:1 + dat1)
expect_identical(+dat1, dat1)
expect_error(
    dat1 + 1.5,
    "Can only add integers to <grates_yearmonth> objects.",
    fixed = TRUE
)
expect_error(
    dat1 + dat1,
    "Cannot add <grates_yearmonth> objects to each other.",
    fixed = TRUE
)

# subtraction -------------------------------------------------------------
x <- as.Date("2021-01-05")
y <- as.Date("2021-01-04")
dat1 <- as_yearmonth(x)
dat2 <- dat1 - 0:1
expect_identical(as.Date(dat2), as.Date(c("2021-01-01", "2020-12-01")))
expect_identical(as.integer(dat2 - dat1), c(0L, -1L))
expect_error(
    1 - dat1,
    "Can only subtract from a <grates_yearmonth> object, not vice-versa.",
    fixed = TRUE
)
expect_error(
    -dat1,
    "Cannot negate a <grates_yearmonth> object.",
    fixed = TRUE
)
expect_error(
    dat1 - 1.5,
    "Can only subtract whole numbers and other <grates_yearmonth> objects from <grates_yearmonth> objects.",
    fixed = TRUE
)

# Other operations error
x <- as_yearmonth(as.Date("2021-01-05"))
expect_error(dat * 3)
expect_error(dat / 3)
expect_error(dat ^ 3)
expect_error(dat %% 3)
expect_error(dat %/% 3)
expect_error(dat & 3)
expect_error(dat | 3)
expect_error(!dat)

# Math
x <- as_yearmonth(as.Date("2021-01-05"))
dat <- c(x + 0:1, yearmonth(NA_integer_))
expect_identical(is.nan(dat), c(FALSE, FALSE, FALSE))
expect_identical(is.finite(dat), c(TRUE, TRUE, FALSE))
expect_identical(is.infinite(dat), c(FALSE, FALSE, FALSE))
expect_error(abs(dat))

# miscellaneous -----------------------------------------------------------
expect_identical(yearmonth(-1.5), yearmonth(-2L))
expect_error(yearmonth("bob"), "`x` must be integer.", fixed = TRUE)
expect_error(
    as_yearmonth(NA_character_),
    "Unable to parse any entries of `x` as Dates.",
    fixed = TRUE
)
dat <- Sys.Date()
dat <- c(dat, dat - 45L)
dat <- as_yearmonth(dat)
expect_identical(rep(dat, 2L), c(dat, dat))
expect_identical(rep(dat, each = 2L), c(dat[[1]], dat[[1]], dat[[2]], dat[[2]]))
expect_identical(unique(c(dat, dat)), dat)
dat <- as_yearmonth(as.Date("1970-01-01"))
expect_identical(
    seq(dat, dat + 11, by = 2L),
    yearmonth(c(0L, 2L, 4L, 6L, 8L, 10L))
)
expect_error(
    seq(dat, dat + 11, by = 2.5),
    "`by` must be an integer of length 1.",
    fixed = TRUE
)
expect_error(
    seq(dat, as.integer(dat + 11), by = 2.5),
    "`to` must be a <grates_yearmonth> object of length 1.",
    fixed = TRUE
)
expect_identical(as.integer(yearmonth(100L)), 100L)
expect_identical(as.double(yearmonth(100L)), 100)
expect_identical(min(c(dat, dat+11)), dat)
expect_identical(max(c(dat, dat+11)), dat+11)
expect_identical(range(seq(dat, dat + 12, by = 2L)), c(dat, dat+12))
expect_error(
    any(dat),
    "`any()` is not supported for <grates_yearmonth> objects.",
    fixed = TRUE
)
