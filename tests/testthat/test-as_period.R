# as_period -----------------------------------------------------------------

test_that("as_period errors correctly", {

  expect_error(as_period(dat))
  expect_error(as_period(TRUE))
  expect_error(as_period("bob"))
  expect_error(as_period("2021-W53"))

})


test_that("period output looks correct", {
  dates <- as.Date("2020-01-01") + 0:61
  dat <- as_period(dates, interval = 2)
  expected <- rep(dates[seq(1, 61, 2)], each = 2)
  expect_equal(as.Date(dat), expected)

  expect_snapshot_output(print(dat))
  # this should look like:
  # <grate_period: interval = 2>
  #  [1] "2020-01-01 to 2020-01-02" "2020-01-01 to 2020-01-02"
  #  [3] "2020-01-03 to 2020-01-04" "2020-01-03 to 2020-01-04"
  #  [5] "2020-01-05 to 2020-01-06" "2020-01-05 to 2020-01-06"
  #  [7] "2020-01-07 to 2020-01-08" "2020-01-07 to 2020-01-08"
  #  [9] "2020-01-09 to 2020-01-10" "2020-01-09 to 2020-01-10"
  # [11] "2020-01-11 to 2020-01-12" "2020-01-11 to 2020-01-12"
  # [13] "2020-01-13 to 2020-01-14" "2020-01-13 to 2020-01-14"
  # [15] "2020-01-15 to 2020-01-16" "2020-01-15 to 2020-01-16"
  # [17] "2020-01-17 to 2020-01-18" "2020-01-17 to 2020-01-18"
  # [19] "2020-01-19 to 2020-01-20" "2020-01-19 to 2020-01-20"
  # [21] "2020-01-21 to 2020-01-22" "2020-01-21 to 2020-01-22"
  # [23] "2020-01-23 to 2020-01-24" "2020-01-23 to 2020-01-24"
  # [25] "2020-01-25 to 2020-01-26" "2020-01-25 to 2020-01-26"
  # [27] "2020-01-27 to 2020-01-28" "2020-01-27 to 2020-01-28"
  # [29] "2020-01-29 to 2020-01-30" "2020-01-29 to 2020-01-30"
  # [31] "2020-01-31 to 2020-02-01" "2020-01-31 to 2020-02-01"
  # [33] "2020-02-02 to 2020-02-03" "2020-02-02 to 2020-02-03"
  # [35] "2020-02-04 to 2020-02-05" "2020-02-04 to 2020-02-05"
  # [37] "2020-02-06 to 2020-02-07" "2020-02-06 to 2020-02-07"
  # [39] "2020-02-08 to 2020-02-09" "2020-02-08 to 2020-02-09"
  # [41] "2020-02-10 to 2020-02-11" "2020-02-10 to 2020-02-11"
  # [43] "2020-02-12 to 2020-02-13" "2020-02-12 to 2020-02-13"
  # [45] "2020-02-14 to 2020-02-15" "2020-02-14 to 2020-02-15"
  # [47] "2020-02-16 to 2020-02-17" "2020-02-16 to 2020-02-17"
  # [49] "2020-02-18 to 2020-02-19" "2020-02-18 to 2020-02-19"
  # [51] "2020-02-20 to 2020-02-21" "2020-02-20 to 2020-02-21"
  # [53] "2020-02-22 to 2020-02-23" "2020-02-22 to 2020-02-23"
  # [55] "2020-02-24 to 2020-02-25" "2020-02-24 to 2020-02-25"
  # [57] "2020-02-26 to 2020-02-27" "2020-02-26 to 2020-02-27"
  # [59] "2020-02-28 to 2020-02-29" "2020-02-28 to 2020-02-29"
  # [61] "2020-03-01 to 2020-03-02" "2020-03-01 to 2020-03-02"

  expect_snapshot_output(as.character(dat))
  # this should look like:
  #  [1] "2020-01-01 to 2020-01-02" "2020-01-01 to 2020-01-02"
  #  [3] "2020-01-03 to 2020-01-04" "2020-01-03 to 2020-01-04"
  #  [5] "2020-01-05 to 2020-01-06" "2020-01-05 to 2020-01-06"
  #  [7] "2020-01-07 to 2020-01-08" "2020-01-07 to 2020-01-08"
  #  [9] "2020-01-09 to 2020-01-10" "2020-01-09 to 2020-01-10"
  # [11] "2020-01-11 to 2020-01-12" "2020-01-11 to 2020-01-12"
  # [13] "2020-01-13 to 2020-01-14" "2020-01-13 to 2020-01-14"
  # [15] "2020-01-15 to 2020-01-16" "2020-01-15 to 2020-01-16"
  # [17] "2020-01-17 to 2020-01-18" "2020-01-17 to 2020-01-18"
  # [19] "2020-01-19 to 2020-01-20" "2020-01-19 to 2020-01-20"
  # [21] "2020-01-21 to 2020-01-22" "2020-01-21 to 2020-01-22"
  # [23] "2020-01-23 to 2020-01-24" "2020-01-23 to 2020-01-24"
  # [25] "2020-01-25 to 2020-01-26" "2020-01-25 to 2020-01-26"
  # [27] "2020-01-27 to 2020-01-28" "2020-01-27 to 2020-01-28"
  # [29] "2020-01-29 to 2020-01-30" "2020-01-29 to 2020-01-30"
  # [31] "2020-01-31 to 2020-02-01" "2020-01-31 to 2020-02-01"
  # [33] "2020-02-02 to 2020-02-03" "2020-02-02 to 2020-02-03"
  # [35] "2020-02-04 to 2020-02-05" "2020-02-04 to 2020-02-05"
  # [37] "2020-02-06 to 2020-02-07" "2020-02-06 to 2020-02-07"
  # [39] "2020-02-08 to 2020-02-09" "2020-02-08 to 2020-02-09"
  # [41] "2020-02-10 to 2020-02-11" "2020-02-10 to 2020-02-11"
  # [43] "2020-02-12 to 2020-02-13" "2020-02-12 to 2020-02-13"
  # [45] "2020-02-14 to 2020-02-15" "2020-02-14 to 2020-02-15"
  # [47] "2020-02-16 to 2020-02-17" "2020-02-16 to 2020-02-17"
  # [49] "2020-02-18 to 2020-02-19" "2020-02-18 to 2020-02-19"
  # [51] "2020-02-20 to 2020-02-21" "2020-02-20 to 2020-02-21"
  # [53] "2020-02-22 to 2020-02-23" "2020-02-22 to 2020-02-23"
  # [55] "2020-02-24 to 2020-02-25" "2020-02-24 to 2020-02-25"
  # [57] "2020-02-26 to 2020-02-27" "2020-02-26 to 2020-02-27"
  # [59] "2020-02-28 to 2020-02-29" "2020-02-28 to 2020-02-29"
  # [61] "2020-03-01 to 2020-03-02" "2020-03-01 to 2020-03-02"


})


test_that("as_period.POSIXlt works as expected", {

  nz <- as.POSIXlt("2021-02-04", tz = "NZ")
  result <- as.POSIXlt(
    as_period(
      nz,
      origin = as.POSIXlt("2021-02-03", tz = "NZ"),
      interval = 2
    ),
    tz = "NZ")
  expected <- as.POSIXlt("2021-02-03", tz = "NZ")
  expected$gmtoff <- result$gmtoff <- NA_integer_ # HACK but ok for this test
  attr(expected, "tzone") <- attr(result, "tzone") <- "NZ" # HACK but ok for this test

  expect_equal(result, expected)

  dat <- as.POSIXlt("2021-01-04", tz = "UTC")
  res <- as_period(dat, interval = 2)

  expect_equal(res, as_period(as.Date("2021-01-04"), interval = 2))
  expect_equal(as.Date(res), as.Date("2021-01-04"))

})


test_that("as_period.POSIXct works as expected", {
  nz <- as.POSIXct(as.POSIXlt("2021-02-04", tz = "NZ"), tz = "NZ")
  result <- as.POSIXct(
    as_period(nz, origin = as.POSIXct(as.POSIXlt("2021-02-03")), interval = 2),
    tz = "NZ"
  )
  expected <- as.POSIXct(as.POSIXlt("2021-02-03", tz = "NZ"), tz = "NZ")
  expect_equal(result, expected)
  expect_equal(as.Date(result, tz = tzone(result)), as.Date("2021-02-03"))
})


test_that("as_period.character works as expected", {
  dat <- "2021-01-04"
  res <- as_period(dat, interval = 2)
  expect_equal(as.Date(res), as.Date("2021-01-04"))
})


test_that("as_period.factor works as expected", {
  dat <- as.factor("2021-01-04")
  res <- as_period(dat, interval = 3)
  expect_equal(as.Date(res), as.Date("2021-01-04"))
})




# as.xxx.period methods -----------------------------------------------------

test_that("as.POSIXct.period works as expected", {
  dat <- "2021-01-04"
  res <- as.POSIXct(as_period(dat, origin = "2021-01-03", interval = 2))
  expect_equal(res, as.POSIXct(as.POSIXlt("2021-01-03", tz = "UTC")))
})


test_that("as.POSIXlt.period works as expected", {
  dat <- "2021-01-04"
  res <- as.POSIXlt(as_period(dat, origin = "2021-01-03", interval = 2))
  expect_equal(res, as.POSIXlt("2021-01-03", tz = "UTC"))
})


test_that("as.character.period works as expected", {
  dat <- "2020-12-28"
  res <- as.character(as_period(dat, interval = 3))
  expect_equal(res, "2020-12-28 to 2020-12-30")
})

test_that("as.list.period works as expected", {
  dat <- as_period(c(a = "2020-12-28", b = "2021-01-04"), interval = 2)
  res <- list(
    a = as_period("2020-12-28", interval = 2),
    b = as_period("2021-01-04", origin = "2020-12-28", interval = 2))
  expect_equal(res, as.list(dat))
})
-------------------------------------------------------------------------



# accessors ---------------------------------------------------------------

test_that("accessors error when they should", {
  expect_error(get_interval("bob"))
})

test_that("accessors work", {
  dat <- as_period(
    as.Date("2020-12-28"),
    firstdate = as.Date("2020-12-26"),
    interval = 55
  )
  expect_equal(get_interval(dat), 55)
})
# -------------------------------------------------------------------------



# is_period -----------------------------------------------------------------

test_that("is_period/grate works", {
  dat <- as_period(Sys.Date(), interval = 2)
  expect_true(is_period(dat))
  expect_false(is_period("bob"))
  expect_true(is_grate(dat))
})
# -------------------------------------------------------------------------



# other methods -----------------------------------------------------------

test_that("is.numeric works", {
  dat <- as_period(Sys.Date(), interval = 2)
  expect_false(is.numeric(dat))
})


test_that("subsetting works", {
  x <- as.Date("2021-01-15")
  dat <- as_period(x, interval = 31) + 0:1
  expect_equal(dat[1], as_period(x, interval = 31))
  expect_equal(dat[[2]], as_period(x + 31, interval = 31, firstdate = as.Date("2021-01-15")))

  dat[1] <- dat[2]
  expect_equal(dat[1], as_period(x + 31, interval = 31, firstdate = as.Date("2021-01-15")))

  expect_error(dat[1] <- "bob")
  expect_error(dat[1] <- as_yrwk(x))
})


test_that("combine errors correctly", {
  x <- Sys.Date()
  dat1 <- as_period(x, interval = 2)
  dat2 <- as_yearweek(x)
  expect_error(c(dat1, "bob"))
  expect_error(c(dat1, dat2))
})


test_that("combine works", {
  x <- as.Date("2020-05-26")
  dat <- as_period(x, interval = 2)
  expect_equal(c(dat, dat), as_period(c(x, x), interval = 2))
})



# ops ---------------------------------------------------------------------
test_that("comparison operators work", {
  x <- Sys.Date()
  dat1 <- as_period(x, interval = 2)

  expect_true(dat1 == dat1)
  expect_true(dat1 <= dat1 + 1)
  expect_true(dat1 >= dat1 - 1)
  expect_true(dat1 < dat1 + 1)
  expect_true(dat1 > dat1 - 1)
  expect_true(dat1 != dat1 + 1)
})

test_that("addition operation works as expected", {
  x <- as.Date("2021-01-05")
  dat1 <- as_period(x, interval = 2)
  dat2 <- dat1 + 0:1
  y <- as.Date("2021-01-05")

  expect_equal(as.Date(dat2), c(y, y + 2))
  expect_equal(dat2, 0:1 + dat1)
  expect_equal(+dat1, dat1)
  expect_error(dat1 + 1.5)
  expect_error(dat1 + dat1)
})


test_that("subtraction operation works as expected", {
  x <- as.Date("2021-01-05")
  dat1 <- as_period(x, interval = 2)
  dat2 <- dat1 - 0:1
  y <- as.Date("2021-01-05")

  expect_equal(as.Date(dat2), c(y, y - 2))
  expect_error(1 - dat1)
  expect_error(dat2 - dat1)
  expect_error(-dat1)
  expect_error(dat1 - 1.5)
  expect_error(dat1 + dat1)
})


test_that("Other operations error", {
  x <- as_period(as.Date("2021-01-05"), interval = 2)

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
  x <- as_period(as.Date("2021-01-05", interval = 2))
  dat <- c(x + 0:1, NA)
  expect_equal(is.nan(dat), c(FALSE, FALSE, FALSE))
  expect_equal(is.finite(dat), c(TRUE, TRUE, FALSE))
  expect_equal(is.infinite(dat), c(FALSE, FALSE, FALSE))
  expect_error(abs(dat))
})






