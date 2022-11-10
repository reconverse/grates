test_that("int_period constructor", {
  dat <- c(0, 7, 14, 21)
  datesw <- vctrs::new_date(dat)
  expect_equal(as.Date(period(dat, n = 7)), datesw)
  expect_error(period(dat, n = -1L))
})


test_that("formatting", {
  dat <- c(0, 7, 14, 21)
  expect_identical(
    format(period(dat, n = 7)),
    c('1970-01-01 to 1970-01-07', '1970-01-08 to 1970-01-14', '1970-01-15 to 1970-01-21', '1970-01-22 to 1970-01-28')
  )

  expect_identical(format(period()), character())

  expect_snapshot(period(dat))
  # <grates_period[4]>
  # [1] 1970-01-01 1970-01-08 1970-01-15 1970-01-22
})

test_that("pre-epoch dates work", {
  dates <- seq.Date(from = as.Date("1900-01-01"), length.out = 4, by = 7)
  dates2 <- seq.Date(from = as.Date("1900-01-01") - 28, length.out = 4, by = 7)
  expect_equal(as.Date(as_period(dates, n=7, origin = as.integer(as.Date("1900-01-01")))), dates)
  expect_equal(as.Date(as_period(dates, n=7, origin = as.integer(as.Date("1900-01-01"))) - 4), dates2)

  expect_snapshot(as_period(dates, n = 7, origin = as.integer(as.Date("1900-01-01"))))
  # this should look like:
  # <grates_period[4]>
  # [1] 1900-01-01 to 1900-01-07 1900-01-08 to 1900-01-14 1900-01-15 to 1900-01-21
  # [4] 1900-01-22 to 1900-01-28
})

# as_int_period -----------------------------------------------------------------

test_that("int_period output looks correct", {
  dates <- 1:4
  dat <- as_int_period(dates, n = 2, origin = 1)
  expected <- c(1, 1, 3, 3)
  expect_equal(as.integer(dat), expected)

  expect_snapshot(dat)
  # this should look like:
  # <grates_int_period[4]>
  # [1] 1 to 2 1 to 2 3 to 4 3 to 4

  expect_snapshot(as.character(dat))
  # this should look like:
  # [1] "1 to 2" "1 to 2" "3 to 4" "3 to 4"


})

# as.xxx.period methods -----------------------------------------------------

test_that("as.character.period works as expected", {
  dat <- 1
  res <- as.character(as_int_period(dat, n = 2))
  expect_equal(res, "0 to 1")
})

test_that("as.list.period works as expected", {
  dat <- as_int_period(c(1, 8), n = 2)
  res <- list(
    as_int_period(1, n = 2),
    as_int_period(8, n = 2))
  expect_equal(res, as.list(dat))
})

-------------------------------------------------------------------------



  # accessors ---------------------------------------------------------------

test_that("accessors error when they should", {
  expect_error(get_n("bob"))
})

test_that("accessors work", {
  dat <- as_int_period(
    as.integer(as.Date("2020-12-28")),
    origin = as.integer(as.Date("2020-12-26")),
    n = 55
  )
  expect_equal(get_n(dat), 55)
})
# -------------------------------------------------------------------------



# is_period -----------------------------------------------------------------

test_that("is_period/grate works", {
  dat <- as_int_period(as.integer(Sys.Date()), n = 2)
  expect_true(is_int_period(dat))
  expect_false(is_int_period("bob"))
  expect_true(is_grate(dat))
})
# -------------------------------------------------------------------------



# other methods -----------------------------------------------------------

test_that("subsetting works", {
  x <- 3:102
  dat <- as_int_period(x, n = 10, origin = 3)

  expect_equal(dat[1], as_int_period(x[1], n = 10, origin = 3))
  expect_equal(dat[12], dat[1] + 1)
  dat[1] <- dat[2]
  expect_equal(dat[1], as_int_period(x[2], n = 10, origin = 3))
  expect_error(dat[1] <- "bob")
  expect_error(dat[1] <- as_yearweek(x[1]))
})


test_that("combine errors correctly", {
  x <- as.integer(Sys.Date())
  dat1 <- as_int_period(x, n = 2)
  dat2 <- as_yearweek(Sys.Date())
  expect_error(c(dat1, "bob"))
  expect_error(c(dat1, dat2))
})


test_that("combine works", {
  x <- 1
  dat <- as_int_period(x, n = 2)
  expect_equal(c(dat, dat), as_int_period(c(x, x), n = 2))
})



# ops ---------------------------------------------------------------------
test_that("comparison operators work", {
  x <- 1
  dat1 <- as_int_period(x, n = 2)

  expect_true(dat1 == dat1)
  expect_true(dat1 <= dat1 + 1)
  expect_true(dat1 >= dat1 - 1)
  expect_true(dat1 < dat1 + 1)
  expect_true(dat1 > dat1 - 1)
  expect_true(dat1 != dat1 + 1)
})

test_that("addition operation works as expected", {
  x <- 3
  dat1 <- as_int_period(x, n = 2)
  dat2 <- dat1 + 0:1
  y <- 2
  expect_equal(as.integer(dat2), c(y, y + 2))
  expect_equal(dat2, 0:1 + dat1)
  expect_equal(+dat1, dat1)
  expect_error(dat1 + 1.5)
  expect_error(dat1 + dat1)
})


test_that("subtraction operation works as expected", {
  x <- as.integer(as.Date("2021-01-05"))
  dat1 <- as_int_period(x, n = 2)
  dat2 <- dat1 - 0:1
  y <- as.integer(as.Date("2021-01-05"))

  expect_equal(as.integer(dat2), c(y, y - 2))
  expect_error(1 - dat1)
  expect_error(-dat1)
  expect_error(dat1 - 1.5)
  expect_error(dat1 + dat1)
})


test_that("Other operations error", {
  x <- as_int_period(as.integer(as.Date("2021-01-05")), n = 2)

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
  x <- as_int_period(as.integer(as.Date("2021-01-05"), n = 2))
  dat <- c(x + 0:1, NA)
  expect_equal(is.nan(dat), c(FALSE, FALSE, FALSE))
  expect_equal(is.finite(dat), c(TRUE, TRUE, FALSE))
  expect_equal(is.infinite(dat), c(FALSE, FALSE, FALSE))
  expect_error(abs(dat))
})






