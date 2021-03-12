# as_yrmon -----------------------------------------------------------------

test_that("as_yrmon errors correctly", {

  expect_error(as_yrmon(dat, 8))
  expect_error(as_yrmon(TRUE))
  expect_error(as_yrmon("bob"))
  expect_error(as_yrmon("2021-W53"))

})


test_that("as_yrmon.Date works correctly", {
  yrs <- rep(2021:2022, each = 12)
  months <- rep.int(1:12, times = 2)
  dates <- as.Date(ISOdate(yrs, months, 5L))
  expected <- seq(from = as.Date("2021-01-01"), to = as.Date("2022-12-01"), by = "month")

  expect_equal(as.Date(as_yrmon(dates)), expected)

  expect_snapshot_output(print(as_yrmon(dates), format = "%Y-%m"))
  # This should look like:
  #  [1] "2021-01" "2021-02" "2021-03" "2021-04" "2021-05" "2021-06" "2021-07"
  #  [8] "2021-08" "2021-09" "2021-10" "2021-11" "2021-12" "2022-01" "2022-02"
  # [15] "2022-03" "2022-04" "2022-05" "2022-06" "2022-07" "2022-08" "2022-09"
  # [22] "2022-10" "2022-11" "2022-12"

  expect_snapshot_output(as.character(as_yrmon(dates), format = "%Y-%m"))
  # This should look like:
  #  [1] "2021-01" "2021-02" "2021-03" "2021-04" "2021-05" "2021-06" "2021-07"
  #  [8] "2021-08" "2021-09" "2021-10" "2021-11" "2021-12" "2022-01" "2022-02"
  # [15] "2022-03" "2022-04" "2022-05" "2022-06" "2022-07" "2022-08" "2022-09"
  # [22] "2022-10" "2022-11" "2022-12"


})


test_that("as_yrmon.POSIXlt works as expected", {

  nz <- as.POSIXlt("2021-01-04", tz = "NZ")
  result <- as.POSIXlt(as_yrmon(nz), tz = "NZ")
  expect_equal(result, as.POSIXlt("2021-01-01", tz = "NZ"))

  dat <- as.POSIXlt("2021-01-04", tz = "UTC")
  res <- as_yrmon(dat)

  expect_equal(res, as_yrmon(as.Date("2021-01-01")))
  expect_equal(as.Date(res), as.Date("2021-01-01"))

})


test_that("as_yrmon.POSIXct works as expected", {
  nz <- as.POSIXct(as.POSIXlt("2021-01-04", tz = "NZ"), tz = "NZ")
  result <- as.POSIXct(as_yrmon(nz), tz = "NZ")
  expect_equal(result, as.POSIXct(as.POSIXlt("2021-01-01", tz = "NZ"), tz = "NZ"))
  expect_equal(as.Date(result, tz = tzone(result)), as.Date("2021-01-01"))
})


test_that("as_yrmon.character works as expected", {
  dat <- "2021-01-04"
  res <- as_yrmon(dat)
  expect_equal(as.Date(res), as.Date("2021-01-01"))
})


test_that("as_yrmon.factor works as expected", {
  dat <- as.factor("2021-01-04")
  res <- as_yrmon(dat)
  expect_equal(as.Date(res), as.Date("2021-01-01"))
})




# as.xxx.yrmon methods -----------------------------------------------------

test_that("as.POSIXct.yrmon works as expected", {
  dat <- "2021-01-04"
  res <- as.POSIXct(as_yrmon(dat))
  expect_equal(res, as.POSIXct(as.POSIXlt("2021-01-01", tz = "UTC")))
})


test_that("as.POSIXlt.yrmon works as expected", {
  dat <- "2021-01-04"
  res <- as.POSIXlt(as_yrmon(dat))
  expect_equal(res, as.POSIXlt("2021-01-01", tz = "UTC"))
})


test_that("as.character.yrmon works as expected", {
  dat <- "2020-12-28"
  res <- as.character(as_yrmon(dat))
  expected <- format.Date(as.Date("2020-12-01"), format = "%Y-%b")
  expect_equal(res, expected)
})

test_that("as.list.yrmon works as expected", {
  dat <- as_yrmon(c(a = "2020-12-28", b = "2021-01-04"))
  res <- list(a = as_yrmon("2020-12-28"), b = as_yrmon("2021-01-04"))
  expect_equal(res, as.list(dat))
})
# -------------------------------------------------------------------------



# accessors ---------------------------------------------------------------

test_that("accessors error when they should", {
  expect_error(get_year("bob"))
  expect_error(get_month("bob"))
})

test_that("accessors work", {
  dat <- as_yrmon(as.Date("2020-12-28"))
  expect_equal(get_year(dat), 2020)
  expect_equal(get_month(dat), 12)
})
# -------------------------------------------------------------------------



# is_yrmon -----------------------------------------------------------------

test_that("is_yrmon/grate works", {
  dat <- as_yrmon(Sys.Date())
  expect_true(is_yrmon(dat))
  expect_false(is_yrmon("bob"))
  expect_true(is_grate(dat))
})
# -------------------------------------------------------------------------



# other methods -----------------------------------------------------------

test_that("is.numeric works", {
  dat <- as_yrmon(Sys.Date())
  expect_false(is.numeric(dat))
})


test_that("subsetting works", {
  x <- as.Date("2021-01-15")
  dat <- as_yrmon(x) + 0:1
  expect_equal(dat[1], as_yrmon(x))
  expect_equal(dat[[2]], as_yrmon(x + 30))

  dat[1] <- dat[2]
  expect_equal(dat[1], as_yrmon(x + 30))

  expect_error(dat[1] <- "bob")
  expect_error(dat[1] <- as_yrwk(x))
})


test_that("combine errors correctly", {
  x <- Sys.Date()
  dat1 <- as_yrmon(x)
  dat2 <- as_yrwk(x)
  expect_error(c(dat1, "bob"))
  expect_error(c(dat1, dat2))
})

test_that("combine works", {
  x <- as.Date("2020-05-26")
  dat <- as_yrmon(x)
  expect_equal(c(dat, dat), as_yrmon(c(x, x)))
  dat2 <- as_yrqtr(x)
  expect_equal(c(dat, dat2), as_yrqtr(c(x, x)))
})


# ops ---------------------------------------------------------------------
test_that("comparison operators work", {
  x <- Sys.Date()
  dat1 <- as_yrmon(x)

  expect_true(dat1 == dat1)
  expect_true(dat1 <= dat1 + 1)
  expect_true(dat1 >= dat1 - 1)
  expect_true(dat1 < dat1 + 1)
  expect_true(dat1 > dat1 - 1)
  expect_true(dat1 != dat1 + 1)
})

test_that("addition operation works as expected", {
  x <- as.Date("2021-01-05")
  y <- as.Date("2021-01-01")
  dat1 <- as_yrmon(x)
  dat2 <- dat1 + 0:1

  expect_equal(as.Date(dat2), c(y, y + 31))
  expect_equal(dat2, 0:1 + dat1)
  expect_equal(+dat1, dat1)
  expect_error(dat1 + 1.5)
  expect_error(dat1 + dat1)
})


test_that("subtraction operation works as expected", {
  x <- as.Date("2021-01-05")
  y <- as.Date("2021-01-01")
  dat1 <- as_yrmon(x)
  dat2 <- dat1 - 0:1


  expect_equal(as.Date(dat2), c(y, y - 31))
  expect_equal(as.integer(dat2 - dat1), c(0, -1))

  expect_error(1 - dat1)
  expect_error(-dat1)
  expect_error(dat1 - 1.5)
  expect_error(dat1 + dat1)
})


test_that("Other operations error", {
  x <- as_yrmon(as.Date("2021-01-05"))

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
  x <- as_yrmon(as.Date("2021-01-05"))
  dat <- c(x + 0:1, NA)
  expect_equal(is.nan(dat), c(FALSE, FALSE, FALSE))
  expect_equal(is.finite(dat), c(TRUE, TRUE, FALSE))
  expect_equal(is.infinite(dat), c(FALSE, FALSE, FALSE))
  expect_error(abs(dat))
})



