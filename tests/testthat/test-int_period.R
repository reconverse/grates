# as_period -----------------------------------------------------------------

test_that("as_period errors correctly", {

  expect_error(as_int_period(dat))
  expect_error(as_int_period(TRUE))
  expect_error(as_int_period("bob"))
  expect_error(as_int_period("2021-W53"))

})


test_that("int_period output looks correct", {
  dates <- 1:4
  dat <- as_int_period(dates, interval = 2)
  expected <- c(1, 1, 3, 3)
  expect_equal(as.integer(dat), expected)

  expect_snapshot_output(print(dat))
  # this should look like:
  # <int_period> interval = 2
  # [1] "1 - 2" "1 - 2" "3 - 4" "3 - 4"

  expect_snapshot_output(as.character(dat))
  # this should look like:
  # [1] "1 - 2" "1 - 2" "3 - 4" "3 - 4"


})

# as.xxx.period methods -----------------------------------------------------

test_that("as.character.period works as expected", {
  dat <- 1
  res <- as.character(as_int_period(dat, interval = 2))
  expect_equal(res, "1 - 2")
})

test_that("as.list.period works as expected", {
  dat <- as_int_period(c(a = 1, b = 8), interval = 2)
  res <- list(
    a = as_int_period(1, interval = 2),
    b = as_int_period(8, firstdate = 1, interval = 2))
  expect_equal(res, as.list(dat))
})

-------------------------------------------------------------------------



  # accessors ---------------------------------------------------------------

test_that("accessors error when they should", {
  expect_error(get_interval("bob"))
})

test_that("accessors work", {
  dat <- as_int_period(
    as.integer(as.Date("2020-12-28")),
    firstdate = as.integer(as.Date("2020-12-26")),
    interval = 55
  )
  expect_equal(get_interval(dat), 55)
})
# -------------------------------------------------------------------------



# is_period -----------------------------------------------------------------

test_that("is_period/grate works", {
  dat <- as_int_period(as.integer(Sys.Date()), interval = 2)
  expect_true(is_int_period(dat))
  expect_false(is_int_period("bob"))
  expect_true(is_grate(dat))
})
# -------------------------------------------------------------------------



# other methods -----------------------------------------------------------

test_that("subsetting works", {
  x <- as.integer(as.Date("2021-01-15"))
  dat <- as_int_period(x, interval = 31) + 0:1
  expect_equal(dat[1], as_int_period(x, interval = 31))
  expect_equal(
    dat[[2]],
    as_int_period(x + 31, interval = 31, firstdate = as.integer(as.Date("2021-01-15")))
  )

  dat[1] <- dat[2]
  expect_equal(
    dat[1],
    as_int_period(x + 31, interval = 31, firstdate = as.integer(as.Date("2021-01-15")))
  )

  expect_error(dat[1] <- "bob")
  expect_error(dat[1] <- as_yrwk(x))
})


test_that("combine errors correctly", {
  x <- as.integer(Sys.Date())
  dat1 <- as_int_period(x, interval = 2)
  dat2 <- as_yrwk(Sys.Date())
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
  expect_error(dat2 - dat1)
  expect_error(1 - dat1)
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






