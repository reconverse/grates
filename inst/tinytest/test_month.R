dates <- seq.Date(from = as.Date("1970-01-01"), length.out = 6L, by = "2 months")
dat <- month(0:5, n=2L)
dates5 <- seq.Date(from = as.Date("1970-01-01"), length.out = 6L, by = "5 months")
dat5 <- month(0:5, n=5L)

# constructor -------------------------------------------------------------
expect_identical(as.Date(dat), dates)
expect_identical(as.Date(dat5), dates5)

# formatting --------------------------------------------------------------
dates2 <- seq.Date(from = as.Date("1970-01-01"), length.out = 7L, by = "2 months")
dates2 <- dates2[-1L]
expect_identical(
    format(dat),
    sprintf("%s to %s", format(dates, "%Y-%b"), format(dates2-1, "%Y-%b"))
)
expect_identical(format(yearmonth()), character())

# pre-epoch dates ---------------------------------------------------------
dates <- seq.Date(from = as.Date("1900-01-01"), length.out = 3L, by = "2 months")
expect_identical(as.Date(as_month(dates, n = 2L)), dates)

# POSIXlt -----------------------------------------------------------------
nz <- as.POSIXlt("2021-01-04 02:00:00", tz = "NZ")
utc <- as.POSIXlt("2021-01-01 00:00:00", tz = "UTC")
result <- as.POSIXlt(as_month(nz, n = 3L))
expect_identical(result, utc)

dat <- "2021-01-01"
pos <- as.POSIXlt("2021-01-04", tz = "UTC")
res1 <- as_month(pos, n = 3L)

expect_identical(as.Date(res1), as.Date(dat))

# POSIXct -----------------------------------------------------------------
nz <- as.POSIXct(as.POSIXlt("2021-01-04", tz = "NZ"))
dat <- as_month(nz, n = 2L)
result <- as.POSIXct(dat)
expect_identical(result, as.POSIXct(as.POSIXlt("2021-01-01", tz = "UTC")))

pos <- as.POSIXct(as.POSIXlt("2021-01-04", tz = "UTC"))
res1 <- as_yearmonth(pos)
expect_identical(as.Date(res1), as.Date("2021-01-01"))


# character ---------------------------------------------------------------
dat <- "2021-01-04"
res <- as_month(dat, n = 6L)
expect_identical(as.Date(res), as.Date("2021-01-01"))

dat <- as.factor("2021-01-04")
res <- as_month(dat, n = 3L)
expect_identical(as.Date(res), as.Date("2021-01-01"))

# as_month errors --------------------------------------------------------
expect_error(
    as_month(TRUE),
    "Not implemented for class [logical].",
    fixed = TRUE
)

expect_error(
    as_month(Sys.Date(), n = 1L),
    "`n` must be greater than 1. If single month groupings are required please use `as_yearmonth()`.",
    fixed = TRUE
)

expect_error(as_month("bob", n = 2L))

# as.POSIXct --------------------------------------------------------------
dat <- "2021-01-01"
res <- as.POSIXct(as_month(dat, n = 2L))
expect_identical(res, as.POSIXct(as.POSIXlt(dat), tz = "UTC"))
expect_error(
    as.POSIXct(as_month(dat, n = 2L), tz = "GMT"),
    "<grates_month> objects can only be converted to UTC. If other timezones are required, first convert to <Date> and then proceed as desired.",
    fixed = TRUE
)

# as.POSIXlt --------------------------------------------------------------
dat <- "2021-01-01"
res <- as.POSIXlt(as_month(dat, n = 3L))
expect_inherits(res, "POSIXlt")
expect_identical(julian(res), julian(as.POSIXlt(dat, tz = "UTC")))
expect_error(
    as.POSIXlt(as_month(dat, n = 3L), tz = "GMT"),
    "<grates_month> objects can only be converted to UTC. If other timezones are required, first convert to <Date> and then proceed as desired.",
    fixed = TRUE
)

# as.character ------------------------------------------------------------
x <- "2020-12-28"
dat <- as_month("2020-12-28", n = 2L)
res <- as.character(dat)
expect_identical(
    res,
    sprintf("%s to %s", format(as.Date(x)-30L, "%Y-%b"), format(as.Date(x), "%Y-%b"))
)

# as.list -----------------------------------------------------------------
dat <- as_month(c("2020-12-28", "2021-01-04"), n = 7L)
res <- list(as_month("2020-12-28", n = 7L), as_month("2021-01-04", n = 7L))
expect_identical(res, as.list(dat))

# is_month -----------------------------------------------------------------
dat <- as_month(Sys.Date(), n = 2L)
expect_true(is_month(dat))
expect_false(is_month("bob"))

# subsetting --------------------------------------------------------------
x <- Sys.Date()
dat <- as_month(x, n = 5L) + 0:1
dat <- setNames(dat, c("a", "b"))
expect_identical(dat[1], c(a=as_month(x, n = 5L)))
expect_identical(dat[[2]], as_month(x + 33, n = 5L))
dat[1] <- as_month(x+33, n = 5L)
expect_identical(dat[1], c(a=as_month(x + 33, n = 5L)))
dat[[2]] <- dat[[1]]
expect_identical(dat[[2]], dat[[1]])
expect_error(
    dat[1] <- "bob",
    "Can only assign <grates_month> objects in to an <grates_month> object.",
    fixed = TRUE
)

# combine -----------------------------------------------------------------
x <- Sys.Date()
dat <- as_month(x, n = 2L)
expect_identical(c(dat, dat), as_month(c(x, x), n = 2L))
expect_error(
    c(dat, "bob"),
    "Unable to combine <grates_month> objects with other classes.",
    fixed = TRUE
)

# comparison operators ----------------------------------------------------
x <- Sys.Date()
dat <- as_month(x, n = 7L)
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
    "Can only compare <grates_month> objects with <grates_month> objects.",
    fixed = TRUE
)

# addition ----------------------------------------------------------------
x <- as.Date("2021-01-05")
y <- as.Date("2021-01-04")
dat1 <- as_month(x, n = 2L)
dat2 <- dat1 + 0:1
expect_identical(as.Date(dat2), as.Date(c("2021-01-01", "2021-03-01")))
expect_identical(dat2, 0:1 + dat1)
expect_identical(+dat1, dat1)
expect_error(
    dat1 + 1.5,
    "Can only add integers to <grates_month> objects.",
    fixed = TRUE
)
expect_error(
    dat1 + dat1,
    "Cannot add <grates_month> objects to each other.",
    fixed = TRUE
)

# subtraction -------------------------------------------------------------
x <- as.Date("2021-01-05")
y <- as.Date("2021-01-04")
dat1 <- as_month(x, n = 2L)
dat2 <- dat1 - 0:1
expect_identical(as.Date(dat2), as.Date(c("2021-01-01", "2020-11-01")))
expect_identical(as.integer(dat2 - dat1), c(0L, -1L))
expect_error(
    1 - dat1,
    "Can only subtract from a <grates_month> object, not vice-versa.",
    fixed = TRUE
)
expect_error(
    -dat1,
    "Cannot negate a <grates_month> object.",
    fixed = TRUE
)
expect_error(
    dat1 - 1.5,
    "Can only subtract whole numbers and other <grates_month> objects from <grates_month> objects.",
    fixed = TRUE
)

# Other operations error
x <- as_month(as.Date("2021-01-05"), n = 2L)
expect_error(dat * 3)
expect_error(dat / 3)
expect_error(dat ^ 3)
expect_error(dat %% 3)
expect_error(dat %/% 3)
expect_error(dat & 3)
expect_error(dat | 3)
expect_error(!dat)

# Math
x <- as_month(as.Date("2021-01-05"), n = 2L)
dat <- c(x + 0:1, month(NA_integer_, n = 2L))
expect_identical(is.na(dat), c(FALSE, FALSE, TRUE))
expect_identical(is.nan(dat), c(FALSE, FALSE, FALSE))
expect_identical(is.finite(dat), c(TRUE, TRUE, FALSE))
expect_identical(is.infinite(dat), c(FALSE, FALSE, FALSE))
expect_error(abs(dat))
