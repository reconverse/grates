# as_yr -----------------------------------------------------------------

test_that("as_yr errors correctly", {

  expect_error(as_yr(dat, 8))
  expect_error(as_yr(TRUE))
  expect_error(as_yr("bob"))
  expect_error(as_yr("2021-W53"))

})


test_that("as_yr.POSIXlt works as expected", {

  nz <- as.POSIXlt("2021-01-04", tz = "NZ")
  result <- as.POSIXlt(as_yr(nz), tz = "NZ")
  expect_equal(result, as.POSIXlt("2021-01-01", tz = "NZ"))

  dat <- as.POSIXlt("2021-01-04", tz = "UTC")
  res <- as_yr(dat)

  expect_equal(res, as_yr(as.Date("2021-01-01")))
  expect_equal(as.Date(res), as.Date("2021-01-01"))

})


test_that("as_yr.POSIXct works as expected", {
  nz <- as.POSIXct(as.POSIXlt("2021-01-04", tz = "NZ"), tz = "NZ")
  result <- as.POSIXct(as_yr(nz), tz = "NZ")
  expect_equal(result, as.POSIXct(as.POSIXlt("2021-01-01", tz = "NZ"), tz = "NZ"))
  expect_equal(as.Date(result, tz = tzone(result)), as.Date("2021-01-01"))
})


test_that("as_yr.character works as expected", {
  dat <- "2021-01-04"
  res <- as_yr(dat)
  expect_equal(as.Date(res), as.Date("2021-01-01"))
})


test_that("as_yr.factor works as expected", {
  dat <- as.factor("2021-01-04")
  res <- as_yr(dat)
  expect_equal(as.Date(res), as.Date("2021-01-01"))
})




# as.xxx.yr methods -----------------------------------------------------

test_that("as.POSIXct.yr works as expected", {
  dat <- "2021-01-04"
  res <- as.POSIXct(as_yr(dat))
  expect_equal(res, as.POSIXct(as.POSIXlt("2021-01-01", tz = "UTC")))
})


test_that("as.POSIXlt.yr works as expected", {
  dat <- "2021-01-04"
  res <- as.POSIXlt(as_yr(dat))
  expect_equal(res, as.POSIXlt("2021-01-01", tz = "UTC"))
})


test_that("as.character.yr works as expected", {
  dat <- "2020-12-28"
  res <- as.character(as_yr(dat))
  expect_equal(res, "2020")
})

test_that("as.list.yr works as expected", {
  dat <- as_yr(c(a = "2020-12-28", b = "2021-01-04"))
  res <- list(a = as_yr("2020-12-28"), b = as_yr("2021-01-04"))
  expect_equal(res, as.list(dat))
})
# -------------------------------------------------------------------------



# accessors ---------------------------------------------------------------

test_that("accessors error when they should", {
  expect_error(get_year("bob"))
})

test_that("accessors work", {
  dat <- as_yr(as.Date("2020-12-28"))
  expect_equal(get_year(dat), 2020)
})
# -------------------------------------------------------------------------



# is_yr -----------------------------------------------------------------

test_that("is_yr/grate works", {
  dat <- as_yr(Sys.Date())
  expect_true(is_yr(dat))
  expect_false(is_yr("bob"))
  expect_true(is_grate(dat))
})
# -------------------------------------------------------------------------



# other methods -----------------------------------------------------------

test_that("is.numeric works", {
  dat <- as_yr(Sys.Date())
  expect_true(is.numeric(dat))
})


test_that("subsetting works", {
  x <- as.Date("2021-01-15")
  dat <- as_yr(x) + 0:1
  expect_equal(dat[1], as_yr(x))
  expect_equal(dat[[2]], as_yr(x + 365))

  dat[1] <- dat[2]
  expect_equal(dat[1], as_yr(x + 365))

  expect_error(dat[1] <- "bob")
  expect_error(dat[1] <- as_yrwk(x))
})


test_that("combine errors correctly", {
  x <- Sys.Date()
  dat1 <- as_yr(x)
  dat2 <- as_yrwk(x)
  expect_error(c(dat1, "bob"))
  expect_error(c(dat1, dat2))
})

test_that("combine works", {
  x <- as.Date("2020-05-26")
  dat <- as_yr(x)
  expect_equal(c(dat, dat), as_yr(c(x, x)))
  dat2 <- as_yrqtr(x)
  expect_equal(c(dat, dat2), as_yr(c(x, x)))
})


# ops ---------------------------------------------------------------------
test_that("comparison operators work", {
  x <- Sys.Date()
  dat1 <- as_yr(x)

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
  dat1 <- as_yr(x)
  dat2 <- dat1 + 0:1

  expect_equal(as.Date(dat2), c(y, y + 365))
  expect_equal(dat2, 0:1 + dat1)
  expect_equal(+dat1, dat1)
  expect_error(dat1 + 1.5)
  expect_error(dat1 + dat1)
})


test_that("subtraction operation works as expected", {
  x <- as.Date("2022-01-05")
  y <- as.Date("2022-01-01")
  dat1 <- as_yr(x)
  dat2 <- dat1 - 0:1


  expect_equal(as.Date(dat2), c(y, y - 365))
  expect_equal(as.integer(dat2 - dat1), c(0, -1))

  expect_error(1 - dat1)
  expect_error(-dat1)
  expect_error(dat1 - 1.5)
  expect_error(dat1 + dat1)
})


test_that("Other operations error", {
  x <- as_yr(as.Date("2021-01-05"))

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
  x <- as_yr(as.Date("2021-01-05"))
  dat <- c(x + 0:1, NA)
  expect_equal(is.nan(dat), c(FALSE, FALSE, FALSE))
  expect_equal(is.finite(dat), c(TRUE, TRUE, FALSE))
  expect_equal(is.infinite(dat), c(FALSE, FALSE, FALSE))
  expect_error(abs(dat))
})


test_that("yr output looks correct", {
  yrs <- rep(2021:2022, each = 12)
  months <- rep.int(1:12, times = 2)
  dates <- as.Date(ISOdate(yrs, months, 5L))
  expected <- structure(yrs, class = c("yr", "grate"))

  expect_equal(as_yr(dates), expected)

  expect_snapshot_output(print(as_yr(dates)))
  # This should look like:
  #  [1] "2021" "2021" "2021" "2021" "2021" "2021" "2021" "2021" "2021" "2021"
  # [11] "2021" "2021" "2022" "2022" "2022" "2022" "2022" "2022" "2022" "2022"
  # [21] "2022" "2022" "2022" "2022"

  expect_snapshot_output(as.character(as_yr(dates)))
  # This should look like:
  #  [1] "2021" "2021" "2021" "2021" "2021" "2021" "2021" "2021" "2021" "2021"
  # [11] "2021" "2021" "2022" "2022" "2022" "2022" "2022" "2022" "2022" "2022"
  # [21] "2022" "2022" "2022" "2022"

})
