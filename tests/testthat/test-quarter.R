test_that("quarter constructor", {
  dat <- c(0,1,2,3)
  datesq <- seq.Date(from = as.Date("1970-01-01"), length.out = 4, by = "quarter")
  expect_equal(as.Date(quarter(dat)), datesq)
  expect_error(quarter(dat, n = 0))
})


test_that("formatting", {
  dat <- c(0,1,2,3)
  expect_identical(format(quarter(dat)), c("1970-Q1", "1970-Q2", "1970-Q3", "1970-Q4"))
  expect_identical(format(quarter()), character())

  expect_snapshot_output(print(quarter(dat)))
  # this should look like:
  #  <grates_quarter[4]>
  #  [1] 1970-Q1 1970-Q2 1970-Q3 1970-Q4
})

test_that("pre-epoch dates work", {
  dates <- seq.Date(from = as.Date("1900-01-01"), length.out = 4, by = "quarter")
  dates2 <- seq.Date(from = as.Date("1899-01-01"), length.out = 4, by = "quarter")
  expect_equal(as.Date(as_quarter(dates)), dates)
  expect_equal(as.Date(as_quarter(dates) - 4), dates2)

  expect_snapshot(as_quarter(dates))
  # this should look like:
  # <grates_quarter[3]>
  # [1] 1900-Q1 1900-Q2 1900-Q3 1900-Q4
})

test_that("as_quarter errors correctly", {
  expect_error(as_quarter(TRUE))
  expect_error(as_quarter("bob"))
  expect_error(as_quarter("2021-W53"))
})

test_that("as_quarter.Date works correctly", {
  start <- as.Date("2020-01-01")
  dat <- seq.Date(from = start, to = start + 365, by = 1)
  res <- aggregate(dat, list(as_quarter(dat)), length)

  expect_equal(res$x, c(91, 91, 92, 92))
  expect_equal(
    as.Date(res[[1]]),
    seq.Date(from = start, to = start + 365, by = "quarter")
  )

  expect_snapshot_output(print(res))
  # This should look like:
  # Group.1  x
  # 1  2020-Q1 91
  # 2  2020-Q2 91
  # 3  2020-Q3 92
  # 4  2020-Q4 92
})


test_that("as_quarter.POSIXlt works as expected", {
  nz <- as.POSIXlt("2021-01-04", tz = "NZ")
  result <- as.POSIXlt(as_quarter(nz), tz = "NZ")
  nz$gmtoff <- result$gmtoff <- NA_integer_ # HACK but ok for this test
  attr(nz, "tzone") <- attr(result, "tzone") <- "NZ" # HACK but ok for this test
  expect_equal(result, as.POSIXlt("2021-01-01", tz = "NZ"))

  dat <- as.POSIXlt("2021-01-04", tz = "UTC")
  res <- as_quarter(dat)

  expect_equal(res, as_quarter(as.Date("2021-01-01")))
  expect_equal(as.Date(res), as.Date("2021-01-01"))

})


test_that("as_quarter.POSIXct works as expected", {
  nz <- as.POSIXct(as.POSIXlt("2021-01-04", tz = "NZ"), tz = "NZ")
  result <- as.POSIXct(as_quarter(nz), tz = "NZ")
  expect_equal(result, as.POSIXct(as.POSIXlt("2021-01-01", tz = "NZ"), tz = "NZ"))
  expect_equal(as.Date(result, tz = attr(result, "tzone")), as.Date("2021-01-01"))
})


test_that("as_quarter.character works as expected", {
  dat <- "2021-01-04"
  res <- as_quarter(dat)
  expect_equal(as.Date(res), as.Date("2021-01-01"))
})


test_that("as_quarter.factor works as expected", {
  dat <- as.factor("2021-01-04")
  res <- as_quarter(dat)
  expect_equal(as.Date(res), as.Date("2021-01-01"))
})




# as.xxx.grate_quarter methods -----------------------------------------------------

test_that("as.POSIXct.grate_quarter works as expected", {
  dat <- "2021-01-04"
  res <- as.POSIXct(as_quarter(dat))
  expect_equal(res, as.POSIXct(as.POSIXlt("2021-01-01", tz = "")))
})


test_that("as.POSIXlt.grate_quarter works as expected", {
  dat <- "2021-01-04"
  res <- as.POSIXlt(as_quarter(dat))
  expect_s3_class(res, "POSIXlt")
  expect_equal(julian(res), julian(as.POSIXlt("2021-01-01")))
})


test_that("as.character.grate_quarter works as expected", {
  dat <- "2020-12-28"
  res <- as.character(as_quarter(dat))
  expect_equal(res, "2020-Q4")
})

test_that("as.list.grate_quarter works as expected", {
  dat <- as_quarter(c("2020-12-28", "2021-01-04"))
  res <- list(as_quarter("2020-12-28"), as_quarter("2021-01-04"))
  expect_equal(res, as.list(dat))
})
# -------------------------------------------------------------------------


# is_quarter -----------------------------------------------------------------

test_that("is_quarter/grate works", {
  dat <- as_quarter(Sys.Date())
  expect_true(is_quarter(dat))
  expect_false(is_quarter("bob"))
  expect_true(is_grate(dat))
})
# -------------------------------------------------------------------------



# other methods -----------------------------------------------------------
test_that("subsetting works", {
  x <- as.Date("2021-01-15")
  dat <- as_quarter(x) + 0:1
  expect_equal(dat[1], as_quarter(x))
  expect_equal(dat[[2]], as_quarter(x + 90))

  dat[1] <- dat[2]
  expect_equal(dat[1], as_quarter(x + 90))

  expect_error(dat[1] <- "bob")
  expect_error(dat[1] <- as_month(x))
})


test_that("combine errors correctly", {
  x <- Sys.Date()
  dat1 <- as_quarter(x)
  dat2 <- as_month(x)
  expect_error(c(dat1, "bob"))
  expect_error(c(dat1, dat2))
})

test_that("combine works", {
  x <- as.Date("2020-05-26")
  dat <- as_quarter(x)
  expect_equal(c(dat, dat), as_quarter(c(x, x)))
})


# ops ---------------------------------------------------------------------
test_that("comparison operators work", {
  x <- Sys.Date()
  dat1 <- as_quarter(x)

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
  dat1 <- as_quarter(x)
  dat2 <- dat1 + 0:1

  expect_equal(as.Date(dat2), c(y, as.Date("2021-04-01")))
  expect_equal(dat2, 0:1 + dat1)
  expect_equal(+dat1, dat1)
  expect_error(dat1 + 1.5)
  expect_error(dat1 + dat1)
})


test_that("subtraction operation works as expected", {
  x <- as.Date("2021-01-05")
  y <- as.Date("2021-01-01")
  dat1 <- as_quarter(x)
  dat2 <- dat1 - 0:1


  expect_equal(as.Date(dat2), c(y, as.Date("2020-10-01")))
  expect_equal(dat2-dat1, c(0,-1))


  expect_error(1 - dat1)
  expect_error(-dat1)
  expect_error(dat1 - 1.5)
  expect_error(dat1 + dat1)
})


test_that("Other operations error", {
  x <- as_quarter(as.Date("2021-01-05"))

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
  x <- as_quarter(as.Date("2021-01-05"))
  dat <- c(x + 0:1, NA)
  expect_equal(is.nan(dat), c(FALSE, FALSE, FALSE))
  expect_equal(is.finite(dat), c(TRUE, TRUE, FALSE))
  expect_equal(is.infinite(dat), c(FALSE, FALSE, FALSE))
  expect_error(abs(dat))
})




