
test_that("year constructor", {
  dat <- c(2001:2004)
  datesy <- seq.Date(from = as.Date("2001-01-01"), length.out = 4, by = "year")
  expect_equal(as.Date(year(dat)), datesy)
})


test_that("formatting", {
  dat <- c(2001:2004)
  expect_identical(format(year(dat)), c("2001", "2002", "2003", "2004"))
  expect_identical(format(quarter()), character())

  expect_snapshot_output(print(year(dat)))
  # this should look like:
  #  <grates_year[4]>
  #  [1] 2001 2002 2003 2004
})


test_that("as_year errors correctly", {

  expect_error(as_year(dat, 8))
  expect_error(as_year(TRUE))
  expect_error(as_year("bob"))
  expect_error(as_year("2021-W53"))

})


test_that("as_year.POSIXlt works as expected", {

  nz <- as.POSIXlt("2021-01-04", tz = "NZ")
  result <- as.POSIXlt(as_year(nz), tz = "NZ")
  expected <- as.POSIXlt("2021-01-01", tz = "NZ")
  expected$gmtoff <- result$gmtoff <- NA_integer_ # HACK but ok for this test
  attr(expected, "tzone") <- attr(result, "tzone") <- "NZ" # HACK but ok for this test
  expect_equal(result, expected)
  dat <- as.POSIXlt("2021-01-04", tz = "UTC")
  res <- as_year(dat)
  expect_equal(res, as_year(as.Date("2021-01-01")))
  expect_equal(as.Date(res), as.Date("2021-01-01"))

})


test_that("as_year.POSIXct works as expected", {
  nz <- as.POSIXct(as.POSIXlt("2021-01-04", tz = "NZ"), tz = "NZ")
  result <- as.POSIXct(as_year(nz), tz = "NZ")
  expect_equal(result, as.POSIXct(as.POSIXlt("2021-01-01", tz = "NZ"), tz = "NZ"))
  expect_equal(as.Date(result, tz = attr(result, "tzone")), as.Date("2021-01-01"))
})


test_that("as_year.character works as expected", {
  dat <- "2021-01-04"
  res <- as_year(dat)
  expect_equal(as.Date(res), as.Date("2021-01-01"))
})


test_that("as_year.factor works as expected", {
  dat <- as.factor("2021-01-04")
  res <- as_year(dat)
  expect_equal(as.Date(res), as.Date("2021-01-01"))
})




# as.xxx.year methods -----------------------------------------------------

test_that("as.POSIXct.year works as expected", {
  dat <- "2021-01-04"
  res <- as.POSIXct(as_year(dat))
  expect_equal(res, as.POSIXct(as.POSIXlt("2021-01-01", tz = "")))
})


test_that("as.POSIXlt.year works as expected", {
  dat <- "2021-01-04"
  res <- as.POSIXlt(as_year(dat))
  expect_s3_class(res, "POSIXlt")
  expect_equal(julian(res), julian(as.POSIXlt("2021-01-01")))

})


test_that("as.character.year works as expected", {
  dat <- "2020-12-28"
  res <- as.character(as_year(dat))
  expect_equal(res, "2020")
})

test_that("as.list.year works as expected", {
  dat <- as_year(c("2020-12-28", "2021-01-04"))
  res <- list(as_year("2020-12-28"), as_year("2021-01-04"))
  expect_equal(res, as.list(dat))
})
# -------------------------------------------------------------------------



# accessors ---------------------------------------------------------------

test_that("accessors error when they should", {
  expect_error(get_year("bob"))
})

test_that("accessors work", {
  dat <- as_year(as.Date("2020-12-28"))
  expect_equal(get_year(dat), 2020)
})
# -------------------------------------------------------------------------



# is_year -----------------------------------------------------------------

test_that("is_year/grate works", {
  dat <- as_year(Sys.Date())
  expect_true(is_year(dat))
  expect_false(is_year("bob"))
  expect_true(is_grate(dat))
})
# -------------------------------------------------------------------------



# other methods -----------------------------------------------------------

test_that("is.numeric works", {
  dat <- as_year(Sys.Date())
  expect_true(is.numeric(dat))
})


test_that("subsetting works", {
  x <- as.Date("2021-01-15")
  dat <- as_year(x) + 0:1
  expect_equal(dat[1], as_year(x))
  expect_equal(dat[[2]], as_year(x + 365))

  dat[1] <- dat[2]
  expect_equal(dat[1], as_year(x + 365))

  expect_error(dat[1] <- "bob")
  expect_error(dat[1] <- as_yearwk(x))
})


test_that("combine errors correctly", {
  x <- Sys.Date()
  dat1 <- as_year(x)
  dat2 <- as_month(x)
  expect_error(c(dat1, "bob"))
  expect_error(c(dat1, dat2))
})

test_that("combine works", {
  x <- as.Date("2020-05-26")
  dat <- as_year(x)
  expect_equal(c(dat, dat), as_year(c(x, x)))
  dat2 <- as_year(x)
  expect_equal(c(dat, dat2), as_year(c(x, x)))
})


# ops ---------------------------------------------------------------------
test_that("comparison operators work", {
  x <- Sys.Date()
  dat1 <- as_year(x)

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
  dat1 <- as_year(x)
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
  dat1 <- as_year(x)
  dat2 <- dat1 - 0:1


  expect_equal(as.Date(dat2), c(y, y - 365))
  expect_equal(as.integer(dat2 - dat1), c(0, -1))

  expect_error(1 - dat1)
  expect_error(-dat1)
  expect_error(dat1 - 1.5)
  expect_error(dat1 + dat1)
})


test_that("Other operations error", {
  x <- as_year(as.Date("2021-01-05"))

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
  x <- as_year(as.Date("2021-01-05"))
  dat <- c(x + 0:1, NA)
  expect_equal(is.nan(dat), c(FALSE, FALSE, FALSE))
  expect_equal(is.finite(dat), c(TRUE, TRUE, FALSE))
  expect_equal(is.infinite(dat), c(FALSE, FALSE, FALSE))
  expect_error(abs(dat))
})


test_that("year output looks correct", {
  years <- rep(2021:2022, each = 12)
  months <- rep.int(1:12, times = 2)
  dates <- as.Date(ISOdate(years, months, 5L))
  expected <- structure(years, class = c("grates_year", "vctrs_vctr"))

  expect_equal(as_year(dates), expected)

  expect_snapshot_output(print(as_year(dates)))
  # This should look like:
  # <grates_year[24]>
  #   [1] 2021 2021 2021 2021 2021 2021 2021 2021 2021 2021 2021 2021 2022 2022 2022
  #  [16] 2022 2022 2022 2022 2022 2022 2022 2022 2022

  expect_snapshot_output(as.character(as_year(dates)))
  # This should look like:
  #  [1] "2021" "2021" "2021" "2021" "2021" "2021" "2021" "2021" "2021" "2021"
  # [11] "2021" "2021" "2022" "2022" "2022" "2022" "2022" "2022" "2022" "2022"
  # [21] "2022" "2022" "2022" "2022"

})
