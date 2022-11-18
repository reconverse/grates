dates <- seq.Date(from = as.Date("1970-01-01"), length.out = 4, by = "3 months")
storage.mode(dates) <- "double" # needed for R <= 4.1.3
yq4 <- new_yearquarter(0:3)

# constructor -------------------------------------------------------------
expect_identical(as.Date(yq4), dates)

# formatting --------------------------------------------------------------
expect_identical(format(yq4), c("1970-Q1", "1970-Q2", "1970-Q3", "1970-Q4"))
expect_identical(format(new_yearquarter()), character())

# pre-epoch dates ---------------------------------------------------------
dates <- seq.Date(from = as.Date("1900-01-01"), length.out = 4, by = "3 months")
storage.mode(dates) <- "double" # needed for R <= 4.1.3
dates2 <- seq.Date(from = as.Date("1899-01-01"), length.out = 4, by = "3 month")
storage.mode(dates2) <- "double" # needed for R <= 4.1.3
expect_identical(as.Date(as_yearquarter(dates)), dates)
expect_identical(as.Date(as_yearquarter(dates) - 4), dates2)

# POSIXlt -----------------------------------------------------------------
nz <- as.POSIXlt("2021-01-04 02:00:00", tz = "NZ")
utc <- as.POSIXlt("2021-01-01 00:00:00", tz = "UTC")
result <- as.POSIXlt(as_yearquarter(nz))

expect_identical(result, utc)

dat <- "2021-01-01"
pos <- as.POSIXlt("2021-02-04", tz = "UTC")
res1 <- as_yearquarter(pos)

expect_identical(as.Date(res1), as.Date(dat))


# POSIXct -----------------------------------------------------------------
nz <- as.POSIXct(as.POSIXlt("2021-01-04", tz = "NZ"))
dat <- as_yearquarter(nz)
result <- as.POSIXct(dat)
expect_identical(result, as.POSIXct(as.POSIXlt("2021-01-01", tz = "UTC")))

pos <- as.POSIXct(as.POSIXlt("2021-03-04", tz = "UTC"))
res1 <- as_yearquarter(pos)
expect_identical(as.Date(res1), as.Date("2021-01-01"))


# character ---------------------------------------------------------------
dat <- "2021-01-04"
res <- as_yearquarter(dat)
expect_identical(as.Date(res), as.Date("2021-01-01"))

dat <- as.factor("2021-02-04")
res <- as_yearquarter(dat)
expect_identical(as.Date(res), as.Date("2021-01-01"))


# as_yearmon errors ------------------------------------------------------
expect_error(
    as_yearquarter(TRUE),
    "Not implemented for class [logical].",
    fixed = TRUE
)
expect_error(as_yearquarter("bob"))


# as.POSIXct --------------------------------------------------------------
dat <- "2021-01-01"
res <- as.POSIXct(as_yearquarter(dat))
expect_identical(res, as.POSIXct(as.POSIXlt(dat), tz = "UTC"))
expect_error(
    as.POSIXct(as_yearquarter(dat), tz = "GMT"),
    "<grates_yearquarter> objects can only be converted to UTC. If other timezones are required, first convert to <Date> and then proceed as desired.",
    fixed = TRUE
)

# as.POSIXlt --------------------------------------------------------------
dat <- "2021-01-01"
res <- as.POSIXlt(as_yearquarter(dat))
expect_inherits(res, "POSIXlt")
expect_identical(julian(res), julian(as.POSIXlt(dat, tz = "UTC")))
expect_error(
    as.POSIXlt(as_yearquarter(dat), tz = "GMT"),
    "<grates_yearquarter> objects can only be converted to UTC. If other timezones are required, first convert to <Date> and then proceed as desired.",
    fixed = TRUE
)

# as.character ------------------------------------------------------------
dat <- "2020-12-28"
res <- as.character(as_yearquarter(dat))
expect_identical(res, "2020-Q4")


# as.list -----------------------------------------------------------------
dat <- as_yearquarter(c("2020-12-28", "2021-01-04"))
res <- list(as_yearquarter("2020-12-28"), as_yearquarter("2021-01-04"))
expect_identical(res, as.list(dat))

# accessors ---------------------------------------------------------------
dat <- as_yearquarter(as.Date("2020-12-28"))
expect_identical(get_year(dat), 2020L)

# is_yearmon -----------------------------------------------------------------
dat <- as_yearquarter(Sys.Date())
expect_true(is_yearquarter(dat))
expect_false(is_yearquarter("bob"))

# subsetting --------------------------------------------------------------
x <- as.Date("2020-03-01")
dat <- as_yearquarter(x) + 0:1
dat <- setNames(dat, c("a", "b"))
expect_identical(dat[1], c(a=as_yearquarter(x)))
expect_identical(dat[[2]], as_yearquarter(x + 33))
dat[1] <- as_yearquarter(x+33)
expect_identical(dat[1], c(a=as_yearquarter(x + 33)))
dat[[2]] <- dat[[1]]
expect_identical(dat[[2]], dat[[1]])
expect_error(
    dat[1] <- "bob",
    "Can only assign <grates_yearquarter> objects in to an <grates_yearquarter> object.",
    fixed = TRUE
)


# combine -----------------------------------------------------------------
x <- Sys.Date()
dat <- as_yearquarter(x)
expect_identical(c(dat, dat), as_yearquarter(c(x, x)))
expect_error(
    c(dat, "bob"),
    "Unable to combine <grates_yearquarter> objects with other classes.",
    fixed = TRUE
)

# comparison operators ----------------------------------------------------
x <- Sys.Date()
dat1 <- as_yearquarter(x)
dat2 <- as_yearquarter(x)
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
    "Can only compare <grates_yearquarter> objects with <grates_yearquarter> objects.",
    fixed = TRUE
)

# addition ----------------------------------------------------------------
x <- as.Date("2021-01-05")
y <- as.Date("2021-01-04")
dat1 <- as_yearquarter(x)
dat2 <- dat1 + 0:1
expect_identical(as.Date(dat2), as.Date(c("2021-01-01", "2021-04-01")))
expect_identical(dat2, 0:1 + dat1)
expect_identical(+dat1, dat1)
expect_error(
    dat1 + 1.5,
    "Can only add integers to <grates_yearquarter> objects.",
    fixed = TRUE
)
expect_error(
    dat1 + dat1,
    "Cannot add <grates_yearquarter> objects to each other.",
    fixed = TRUE
)

# subtraction -------------------------------------------------------------
x <- as.Date("2021-01-05")
y <- as.Date("2021-01-04")
dat1 <- as_yearquarter(x)
dat2 <- dat1 - 0:1
expect_identical(as.Date(dat2), as.Date(c("2021-01-01", "2020-10-01")))
expect_identical(as.integer(dat2 - dat1), c(0L, -1L))
expect_error(
    1 - dat1,
    "Can only subtract from a <grates_yearquarter> object, not vice-versa.",
    fixed = TRUE
)
expect_error(
    -dat1,
    "Cannot negate a <grates_yearquarter> object.",
    fixed = TRUE
)
expect_error(
    dat1 - 1.5,
    "Can only subtract whole numbers and other <grates_yearquarter> objects from <grates_yearquarter> objects.",
    fixed = TRUE
)

# Other operations error
x <- as_yearquarter(as.Date("2021-01-05"))
expect_error(dat * 3)
expect_error(dat / 3)
expect_error(dat ^ 3)
expect_error(dat %% 3)
expect_error(dat %/% 3)
expect_error(dat & 3)
expect_error(dat | 3)
expect_error(!dat)

# Math
x <- as_yearquarter(as.Date("2021-01-05"))
dat <- c(x + 0:1, new_yearquarter(NA_integer_))
expect_identical(is.nan(dat), c(FALSE, FALSE, FALSE))
expect_identical(is.finite(dat), c(TRUE, TRUE, FALSE))
expect_identical(is.infinite(dat), c(FALSE, FALSE, FALSE))
expect_error(abs(dat))

# test the new constructor implementation
dat <- as.Date("1900-01-01") + seq.int(from = 0L, to = 200L * 365, by = 180L)
expected <- as_yearquarter(dat)
tmp <- as.character(dat)
years <- as.integer(substr(tmp, 1L, 4L))
months <- as.integer(substr(tmp, 6L, 7L))
expect_identical(yearquarter(years, ((months - 1L) %/% 3L) + 1L), expected)
