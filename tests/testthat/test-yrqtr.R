# as_yrqtr -----------------------------------------------------------------

test_that("as_yrqtr errors correctly", {

  expect_error(as_yrqtr(dat, 8))
  expect_error(as_yrqtr(TRUE))
  expect_error(as_yrqtr("bob"))
  expect_error(as_yrqtr("2021-W53"))

})


test_that("as_yrqtr.Date works as expected", {
  yrs <- rep(2021:2022, each = 12)
  months <- rep.int(1:12, times = 2)
  dates <- as.Date(ISOdate(yrs, months, 5L, tz = "UTC"), tz = "UTC")
  expected <- seq(from = as.Date("2021-01-01"), to = as.Date("2022-12-01"), by = "quarter")
  expected <- rep(expected, each = 3)
  expect_equal(as.Date(as_yrqtr(dates)), expected)

  expect_snapshot_output(print(as_yrqtr(dates), format = "%Y-%m"))
  # This should look like:
  #  [1] "2021-Q1" "2021-Q1" "2021-Q1" "2021-Q2" "2021-Q2" "2021-Q2" "2021-Q3"
  #  [8] "2021-Q3" "2021-Q3" "2021-Q4" "2021-Q4" "2021-Q4" "2022-Q1" "2022-Q1"
  # [15] "2022-Q1" "2022-Q2" "2022-Q2" "2022-Q2" "2022-Q3" "2022-Q3" "2022-Q3"
  # [22] "2022-Q4" "2022-Q4" "2022-Q4"

  expect_snapshot_output(as.character(as_yrqtr(dates)))
  # This should look like:
  #  [1] "2021-Q1" "2021-Q1" "2021-Q1" "2021-Q2" "2021-Q2" "2021-Q2" "2021-Q3"
  #  [8] "2021-Q3" "2021-Q3" "2021-Q4" "2021-Q4" "2021-Q4" "2022-Q1" "2022-Q1"
  # [15] "2022-Q1" "2022-Q2" "2022-Q2" "2022-Q2" "2022-Q3" "2022-Q3" "2022-Q3"
  # [22] "2022-Q4" "2022-Q4" "2022-Q4"
})


test_that("as_yrqtr.POSIXlt works as expected", {

  nz <- as.POSIXlt("2021-02-04", tz = "NZ")
  result <- as.POSIXlt(as_yrqtr(nz), tz = "NZ")
  expect_equal(result, as.POSIXlt("2021-01-01", tz = "NZ"))

  dat <- as.POSIXlt("2021-01-04", tz = "UTC")
  res <- as_yrqtr(dat)

  expect_equal(res, as_yrqtr(as.Date("2021-01-01")))
  expect_equal(as.Date(res), as.Date("2021-01-01"))
})


test_that("as_yrqtr.POSIXct works as expected", {
  nz <- as.POSIXct(as.POSIXlt("2021-02-04", tz = "NZ"), tz = "NZ")
  result <- as.POSIXct(as_yrqtr(nz), tz = "NZ")
  expect_equal(result, as.POSIXct(as.POSIXlt("2021-01-01", tz = "NZ"), tz = "NZ"))
  expect_equal(as.Date(result, tz = tzone(result)), as.Date("2021-01-01"))
})


test_that("as_yrqtr.character works as expected", {
  dat <- "2021-01-04"
  res <- as_yrqtr(dat)
  expect_equal(as.Date(res), as.Date("2021-01-01"))
})


test_that("as_yrqtr.factor works as expected", {
  dat <- as.factor("2021-01-04")
  res <- as_yrqtr(dat)
  expect_equal(as.Date(res), as.Date("2021-01-01"))
})


# as.xxx.yrqtr methods -----------------------------------------------------

test_that("as.POSIXct.yrqtr works as expected", {
  dat <- "2021-01-04"
  res <- as.POSIXct(as_yrqtr(dat))
  expect_equal(res, as.POSIXct(as.POSIXlt("2021-01-01", tz = "UTC")))
})


test_that("as.POSIXlt.yrqtr works as expected", {
  dat <- "2021-01-04"
  res <- as.POSIXlt(as_yrqtr(dat))
  expect_equal(res, as.POSIXlt("2021-01-01", tz = "UTC"))
})


test_that("as.character.yrqtr works as expected", {
  dat <- "2020-12-28"
  res <- as.character(as_yrqtr(dat))
  expect_equal(res, "2020-Q4")
})

test_that("as.list.yrqtr works as expected", {
  dat <- as_yrqtr(c(a = "2020-12-28", b = "2021-01-04"))
  res <- list(a = as_yrqtr("2020-12-28"), b = as_yrqtr("2021-01-04"))
  expect_equal(res, as.list(dat))
})
# -------------------------------------------------------------------------


# accessors ---------------------------------------------------------------

test_that("accessors error when they should", {
  expect_error(get_qtr("bob"))
})

test_that("accessors work", {
  dat <- as_yrqtr(as.Date("2020-12-28"))
  expect_equal(get_year(dat), 2020)
  expect_equal(get_quarter(dat), 4)
})
# -------------------------------------------------------------------------



# is_yrqtr -----------------------------------------------------------------

test_that("is_yrqtr/grate works", {
  dat <- as_yrqtr(Sys.Date())
  expect_true(is_yrqtr(dat))
  expect_false(is_yrqtr("bob"))
  expect_true(is_grate(dat))
})
# -------------------------------------------------------------------------



# other methods -----------------------------------------------------------

test_that("is.numeric works", {
  dat <- as_yrqtr(Sys.Date())
  expect_false(is.numeric(dat))
})


test_that("subsetting works", {
  x <- as.Date("2021-01-15")
  dat <- as_yrqtr(x) + 0:1
  expect_equal(dat[1], as_yrqtr(x))
  expect_equal(dat[[2]], as_yrqtr(x + 100))

  dat[1] <- dat[2]
  expect_equal(dat[1], as_yrqtr(x + 100))

  expect_error(dat[1] <- "bob")
  expect_error(dat[1] <- as_yrwk(x))
})


test_that("combine errors correctly", {
  x <- Sys.Date()
  dat1 <- as_yrqtr(x)
  dat2 <- as_yrwk(x)
  expect_error(c(dat1, "bob"))
  expect_error(c(dat1, dat2))
})


test_that("combine works", {
  x <- as.Date("2020-05-26")
  dat <- as_yrqtr(x)
  dat2 <- as_yrmon(x)
  expect_equal(c(dat, dat), as_yrqtr(c(x, x)))
  expect_equal(c(dat, dat2), as_yrqtr(c(x, x)))
})



# ops ---------------------------------------------------------------------
test_that("comparison operators work", {
  x <- Sys.Date()
  dat1 <- as_yrqtr(x)

  expect_true(dat1 == dat1)
  expect_true(dat1 <= dat1 + 1)
  expect_true(dat1 >= dat1 - 1)
  expect_true(dat1 < dat1 + 1)
  expect_true(dat1 > dat1 - 1)
  expect_true(dat1 != dat1 + 1)
})

test_that("addition operation works as expected", {
  x <- as.Date("2021-01-05")
  dat1 <- as_yrqtr(x)
  dat2 <- dat1 + 0:1
  y <- as.Date("2021-01-01")

  expect_equal(as.Date(dat2), c(y, y + 90))
  expect_equal(dat2, 0:1 + dat1)
  expect_equal(+dat1, dat1)
  expect_error(dat1 + 1.5)
  expect_error(dat1 + dat1)
})


test_that("subtraction operation works as expected", {
  x <- as.Date("2021-01-05")
  dat1 <- as_yrqtr(x)
  dat2 <- dat1 - 0:1
  y <- as.Date("2021-01-01")

  expect_equal(as.Date(dat2), c(y, y - 92))
  expect_equal(as.integer(dat2 - dat1), c(0, -1))

  expect_error(1 - dat1)
  expect_error(-dat1)
  expect_error(dat1 - 1.5)
  expect_error(dat1 + dat1)
})


test_that("Other operations error", {
  x <- as_yrqtr(as.Date("2021-01-05"))

  expect_error(dat * 3)
  expect_error(dat / 3)
  expect_error(dat ^ 3)
  expect_error(dat %% 3)
  expect_error(dat %/% 3)
  expect_error(dat & 3)
  expect_error(dat | 3)
  expect_error(!dat)
})


test_that("Maths works", {
  x <- as_yrqtr(as.Date("2021-01-05"))
  dat <- c(x + 0:1, NA)
  expect_equal(is.nan(dat), c(FALSE, FALSE, FALSE))
  expect_equal(is.finite(dat), c(TRUE, TRUE, FALSE))
  expect_equal(is.infinite(dat), c(FALSE, FALSE, FALSE))
  expect_error(abs(dat))
})


