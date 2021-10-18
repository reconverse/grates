test_that("month constructor", {
  dat <- 0:11
  dates <- seq.Date(
    from = as.Date("1970-01-01"),
    to = as.Date("1970-12-01"),
    by = "month"
  )

  expect_equal(as.Date(month(dat)), dates)
  expect_error(month(dat, n = 0))
  expect_error(month(dat, n = 2))
  expect_error(month(dat[c(TRUE,FALSE)], n = 2, origin = 1))
})


test_that("formatting", {

  dat <- 0:3
  dates <- seq.Date(
    from = as.Date("1970-01-01"),
    to = as.Date("1970-04-01"),
    by = "month"
  )

  expect_identical(format(month(dat)), format.Date(dates, "%Y-%b"))
  expect_identical(format(month(dat), "%Y-%m"), format.Date(dates, "%Y-%m"))
  expect_identical(format(month()), character())

  expect_snapshot_output(print(month(dat)))
  # this should look like:
  #  <grates_month[4]>
  #  [1] 1970-Jan 1970-Feb 1970-Mar 1970-Apr
})


test_that("pre-epoch dates work", {

  dates <- seq.Date(
    from = as.Date("1900-02-01"),
    to = as.Date("1900-12-01"),
    by = "5 months"
  )
  dates5 <- seq.Date(
    from = as.Date("1900-07-01"),
    length.out = 3L,
    by = "5 months"
  )

  expect_equal(as.Date(as_month(dates)), dates)
  expect_equal(as.Date(as_month(dates, n = 5L, origin = 1) + 1), dates5)
  expect_snapshot(as_month(dates))
  # this should look like:
  # <grates_month[3]>
  # [1] 1900-Feb 1900-Jul 1900-Dec
})


test_that("as_month errors correctly", {
  expect_error(as_month(TRUE))
  suppressWarnings(expect_error(as_month("bob")))
  suppressWarnings(expect_error(as_month("2021-W53")))
})


test_that("as_month.Date works correctly - n = 1", {
  start <- as.Date("2020-01-01")
  dat <- seq.Date(from = start, to = start + 365, by = 1)
  res <- aggregate(dat, list(as_month(dat)), length)

  expect_equal(res$x, c(31,29,31,30,31,30,31,31,30,31,30,31))
  expect_equal(
    as.Date(res[[1]]),
    seq.Date(from = start, to = start + 365, by = "month")
  )

  expect_snapshot_output(print(res))
  # This should look like:
  # Group.1  x
  # 1  2020-Jan 31
  # 2  2020-Feb 29
  # 3  2020-Mar 31
  # 4  2020-Apr 30
  # 5  2020-May 31
  # 6  2020-Jun 30
  # 7  2020-Jul 31
  # 8  2020-Aug 31
  # 9  2020-Sep 30
  # 10 2020-Oct 31
  # 11 2020-Nov 30
  # 12 2020-Dec 31

  expect_snapshot_output(print(unique(res$Group.1)))
  # This should look like:
  # <grates_month[12]>
  # [1] 2020-Jan 2020-Feb 2020-Mar 2020-Apr 2020-May 2020-Jun 2020-Jul 2020-Aug
  # [9] 2020-Sep 2020-Oct 2020-Nov 2020-Dec
})


test_that("as_month.Date works correctly - n > 1", {
  yrs <- rep(2021:2022, each = 12)
  months <- rep.int(1:12, times = 2)
  dates <- as.Date(ISOdate(yrs, months, 5L))
  expected <- seq(from = as.Date("2021-01-01"), to = as.Date("2022-12-01"), by = "2 month")
  expected <- rep(expected, each = 2)

  expect_equal(as.Date(as_month(dates, n = 2)), expected)

  expect_snapshot_output(print(as_month(dates, n = 2)))
  # This should look like:
  # <grates_month[24]>
  #   [1] "2021-Jan to 2021-Feb" "2021-Jan to 2021-Feb" "2021-Mar to 2021-Apr"
  #   [4] "2021-Mar to 2021-Apr" "2021-May to 2021-Jun" "2021-May to 2021-Jun"
  #   [7] "2021-Jul to 2021-Aug" "2021-Jul to 2021-Aug" "2021-Sep to 2021-Oct"
  #  [10] "2021-Sep to 2021-Oct" "2021-Nov to 2021-Dec" "2021-Nov to 2021-Dec"
  #  [13] "2022-Jan to 2022-Feb" "2022-Jan to 2022-Feb" "2022-Mar to 2022-Apr"
  #  [16] "2022-Mar to 2022-Apr" "2022-May to 2022-Jun" "2022-May to 2022-Jun"
  #  [19] "2022-Jul to 2022-Aug" "2022-Jul to 2022-Aug" "2022-Sep to 2022-Oct"
  #  [22] "2022-Sep to 2022-Oct" "2022-Nov to 2022-Dec" "2022-Nov to 2022-Dec"
})


test_that("as_month.POSIXlt works as expected", {
  nz <- as.POSIXlt("2021-01-04", tz = "NZ")
  result <- as.POSIXlt(as_month(nz), tz = "NZ")
  nz$gmtoff <- result$gmtoff <- NA_integer_ # HACK but ok for this test
  attr(nz, "tzone") <- attr(result, "tzone") <- "NZ" # HACK but ok for this test
  expect_equal(result, as.POSIXlt("2021-01-01", tz = "NZ"))

  dat <- as.POSIXlt("2021-01-04", tz = "UTC")
  res <- as_month(dat)

  expect_equal(res, as_month(as.Date("2021-01-01")))
  expect_equal(as.Date(res), as.Date("2021-01-01"))

})


test_that("as_month.POSIXct works as expected", {
  nz <- as.POSIXct(as.POSIXlt("2021-01-04", tz = "NZ"), tz = "NZ")
  result <- as.POSIXct(as_month(nz, 2), tz = "NZ")
  expect_equal(result, as.POSIXct(as.POSIXlt("2021-01-01", tz = "NZ"), tz = "NZ"))
  expect_equal(as.Date(result, tz = attr(result, "tzone")), as.Date("2021-01-01"))
})


test_that("as_month.character works as expected", {
  dat <- "2021-01-04"
  res <- as_month(dat)
  expect_equal(as.Date(res), as.Date("2021-01-01"))
})


test_that("as_month.factor works as expected", {
  dat <- as.factor("2021-01-04")
  res <- as_month(dat)
  expect_equal(as.Date(res), as.Date("2021-01-01"))
})




# as.xxx.grate_month methods -----------------------------------------------------

test_that("as.POSIXct.grate_month works as expected", {
  dat <- "2021-01-04"
  res <- as.POSIXct(as_month(dat))
  expect_equal(res, as.POSIXct(as.POSIXlt("2021-01-01"), tz = ""))
})


test_that("as.POSIXlt.grate_month works as expected", {
  dat <- "2021-01-04"
  res <- as.POSIXlt(as_month(dat))
  expect_s3_class(res, "POSIXlt")
  expect_equal(julian(res), julian(as.POSIXlt("2021-01-01")))
})


test_that("as.character.grate_month works as expected", {
  dat <- "2020-12-28"
  res <- as.character(as_month(dat))
  expected <- format.Date(as.Date("2020-12-01"), format = "%Y-%b")
  expect_equal(res, expected)
})

test_that("as.list.grate_month works as expected", {
  dat <- as_month(c("2020-12-28", "2021-01-04"))
  res <- list(as_month("2020-12-28"), as_month("2021-01-04"))
  expect_equal(res, as.list(dat))
})
# -------------------------------------------------------------------------


# is_month -----------------------------------------------------------------

test_that("is_month/grate works", {
  dat <- as_month(Sys.Date())
  expect_true(is_month(dat))
  expect_false(is_month("bob"))
  expect_true(is_grate(dat))
})
# -------------------------------------------------------------------------



# other methods -----------------------------------------------------------
test_that("subsetting works", {
  x <- as.Date("2021-01-15")
  dat <- as_month(x) + 0:1
  expect_equal(dat[1], as_month(x))
  expect_equal(dat[[2]], as_month(x + 30))

  dat[1] <- dat[2]
  expect_equal(dat[1], as_month(x + 30))

  expect_error(dat[1] <- "bob")
  expect_error(dat[1] <- as_month(x, n = 2))
})


test_that("combine errors correctly", {
  x <- Sys.Date()
  dat1 <- as_month(x)
  dat2 <- as_month(x, n = 2)
  expect_error(c(dat1, "bob"))
  expect_error(c(dat1, dat2))
})

test_that("combine works", {
  x <- as.Date("2020-05-26")
  dat <- as_month(x)
  expect_equal(c(dat, dat), as_month(c(x, x)))
})


# ops ---------------------------------------------------------------------
test_that("comparison operators work", {
  x <- Sys.Date()
  dat1 <- as_month(x)

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
  dat1 <- as_month(x)
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
  dat1 <- as_month(x)
  dat2 <- dat1 - 0:1


  expect_equal(as.Date(dat2), c(y, y - 31))
  expect_equal(dat2-dat1, c(0,-1))


  expect_error(1 - dat1)
  expect_error(-dat1)
  expect_error(dat1 - 1.5)
  expect_error(dat1 + dat1)
})


test_that("Other operations error", {
  x <- as_month(as.Date("2021-01-05"))

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
  x <- as_month(as.Date("2021-01-05"))
  dat <- c(x + 0:1, NA)
  expect_equal(is.nan(dat), c(FALSE, FALSE, FALSE))
  expect_equal(is.finite(dat), c(TRUE, TRUE, FALSE))
  expect_equal(is.infinite(dat), c(FALSE, FALSE, FALSE))
  expect_error(abs(dat))
})



