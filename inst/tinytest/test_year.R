dates <- seq.Date(from = as.Date("0000-01-01"), length.out = 4, by = "year")
yr4 <- year(0:3)

# constructor -------------------------------------------------------------
expect_identical(as.Date(yr4), dates)

# formatting --------------------------------------------------------------
expect_identical(format(yr4), as.character(yr4))
expect_identical(format(year()), character())

# pre-epoch dates ---------------------------------------------------------
dates <- seq.Date(from = as.Date("1900-01-01"), length.out = 4, by = "year")
dates2 <- seq.Date(from = as.Date("1896-01-01"), length.out = 4, by = "year")
expect_identical(as.Date(as_year(dates)), dates)
expect_identical(as.Date(as_year(dates) - 4), dates2)

# POSIXlt -----------------------------------------------------------------
nz <- as.POSIXlt("2021-01-04 02:00:00", tz = "NZ")
utc <- as.POSIXlt("2021-01-01 00:00:00", tz = "UTC")
result <- as.POSIXlt(as_year(nz))

expect_identical(result, utc)

dat <- "2021-01-01"
pos <- as.POSIXlt("2021-02-04", tz = "UTC")
res1 <- as_year(pos)

expect_identical(as.Date(res1), as.Date(dat))


# POSIXct -----------------------------------------------------------------
nz <- as.POSIXct(as.POSIXlt("2021-01-04", tz = "NZ"))
dat <- as_year(nz)
result <- as.POSIXct(dat)
expect_identical(result, as.POSIXct(as.POSIXlt("2021-01-01", tz = "UTC")))

pos <- as.POSIXct(as.POSIXlt("2021-03-04", tz = "UTC"))
res1 <- as_year(pos)
expect_identical(as.Date(res1), as.Date("2021-01-01"))


# character ---------------------------------------------------------------
dat <- "2021-01-04"
res <- as_year(dat)
expect_identical(as.Date(res), as.Date("2021-01-01"))

dat <- as.factor("2021-02-04")
res <- as_year(dat)
expect_identical(as.Date(res), as.Date("2021-01-01"))


# as_yearmon errors ------------------------------------------------------
expect_error(
    as_year(TRUE),
    "Not implemented for class [logical].",
    fixed = TRUE
)
expect_error(as_year("bob"))


# as.POSIXct --------------------------------------------------------------
dat <- "2021-01-01"
res <- as.POSIXct(as_year(dat))
expect_identical(res, as.POSIXct(as.POSIXlt(dat), tz = "UTC"))
expect_error(
    as.POSIXct(as_year(dat), tz = "GMT"),
    "<grates_year> objects can only be converted to UTC. If other timezones are required, first convert to <Date> and then proceed as desired.",
    fixed = TRUE
)

# as.POSIXlt --------------------------------------------------------------
dat <- "2021-01-01"
res <- as.POSIXlt(as_year(dat))
expect_inherits(res, "POSIXlt")
expect_identical(julian(res), julian(as.POSIXlt(dat, tz = "UTC")))
expect_error(
    as.POSIXlt(as_year(dat), tz = "GMT"),
    "<grates_year> objects can only be converted to UTC. If other timezones are required, first convert to <Date> and then proceed as desired.",
    fixed = TRUE
)

# as.character ------------------------------------------------------------
dat <- "2020-12-28"
res <- as.character(as_year(dat))
expect_identical(res, "2020")


# as.list -----------------------------------------------------------------
dat <- as_year(c("2020-12-28", "2021-01-04"))
res <- list(as_year("2020-12-28"), as_year("2021-01-04"))
expect_identical(res, as.list(dat))

# accessors ---------------------------------------------------------------
dat <- as_year(as.Date("2020-12-28"))
expect_identical(get_year(dat), 2020L)

# is_yearmon -----------------------------------------------------------------
dat <- as_year(Sys.Date())
expect_true(is_year(dat))
expect_false(is_year("bob"))

# subsetting --------------------------------------------------------------
x <- Sys.Date()
x2 <- seq.Date(x, by="year", length.out=2)[2]
dat <- as_year(x) + 0:1
dat <- setNames(dat, c("a", "b"))
expect_identical(dat[1], c(a=as_year(x)))
expect_identical(dat[[2]], as_year(x2))
dat[1] <- as_year(x2)
expect_identical(dat[1], c(a=as_year(x2)))
dat[[2]] <- dat[[1]]
expect_identical(dat[[2]], dat[[1]])
expect_error(
    dat[1] <- "bob",
    "Can only assign <grates_year> objects in to an <grates_year> object.",
    fixed = TRUE
)


# combine -----------------------------------------------------------------
x <- Sys.Date()
dat <- as_year(x)
expect_identical(c(dat, dat), as_year(c(x, x)))
expect_error(
    c(dat, "bob"),
    "Unable to combine <grates_year> objects with other classes.",
    fixed = TRUE
)

# comparison operators ----------------------------------------------------
x <- Sys.Date()
dat1 <- as_year(x)
dat2 <- as_year(x)
expect_true(dat1 == dat2)
expect_false(dat1 != dat2)
expect_true(dat1 == dat1)
expect_true(dat1 <= dat1 + 1)
expect_true(dat1 >= dat1 - 1)
expect_true(dat1 < dat1 + 1)
expect_true(dat1 > dat1 - 1)
expect_true(dat1 != dat1 + 1)
expect_error(
    dat1 == TRUE,
    "Can only compare <grates_year> objects with <grates_year> objects.",
    fixed = TRUE
)

# addition ----------------------------------------------------------------
x <- as.Date("2021-01-05")
y <- as.Date("2021-01-04")
dat1 <- as_year(x)
dat2 <- dat1 + 0:1
expect_identical(as.Date(dat2), as.Date(c("2021-01-01", "2022-01-01")))
expect_identical(dat2, 0:1 + dat1)
expect_identical(+dat1, dat1)
expect_error(
    dat1 + 1.5,
    "Can only add integers to <grates_year> objects.",
    fixed = TRUE
)
expect_error(
    dat1 + dat1,
    "Cannot add <grates_year> objects to each other.",
    fixed = TRUE
)

# subtraction -------------------------------------------------------------
x <- as.Date("2021-01-05")
y <- as.Date("2021-01-04")
dat1 <- as_year(x)
dat2 <- dat1 - 0:1
expect_identical(as.Date(dat2), as.Date(c("2021-01-01", "2020-01-01")))
expect_identical(as.integer(dat2 - dat1), c(0L, -1L))
expect_error(
    1 - dat1,
    "Can only subtract from a <grates_year> object, not vice-versa.",
    fixed = TRUE
)
expect_error(
    -dat1,
    "Cannot negate a <grates_year> object.",
    fixed = TRUE
)
expect_error(
    dat1 - 1.5,
    "Can only subtract whole numbers and other <grates_year> objects from <grates_year> objects.",
    fixed = TRUE
)

# Other operations error
x <- as_year(as.Date("2021-01-05"))
expect_error(dat * 3)
expect_error(dat / 3)
expect_error(dat ^ 3)
expect_error(dat %% 3)
expect_error(dat %/% 3)
expect_error(dat & 3)
expect_error(dat | 3)
expect_error(!dat)

# Math
x <- as_year(as.Date("2021-01-05"))
dat <- c(x + 0:1, year(NA_integer_))
expect_identical(is.nan(dat), c(FALSE, FALSE, FALSE))
expect_identical(is.finite(dat), c(TRUE, TRUE, FALSE))
expect_identical(is.infinite(dat), c(FALSE, FALSE, FALSE))
expect_error(abs(dat))
