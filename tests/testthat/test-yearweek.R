test_that("yearweek constructor", {
  dat <- c(0, 1, 2, 3)
  datesw <- seq.Date(from = as.Date("1970-01-01"), length.out = 4, by = "week")
  expect_equal(as.Date(yearweek(dat, firstday = 4L)), datesw)
  expect_error(quarter(dat, firstday = 0L))
})


test_that("formatting", {
  dat <- c(0, 1, 2, 3)
  expect_identical(
    format(yearweek(dat, firstday = 4)),
    c("1970-W01", "1970-W02", "1970-W03", "1970-W04"))
  expect_identical(format(yearweek()), character())

  expect_snapshot_output(print(yearweek(dat)))
  # this should look like:
  #  <grates_yearweek[fd: 4]>
  #  [1] 1970-W01 1970-W02 1970-W03 1970-W04
})

test_that("pre-epoch dates work", {
  dates <- seq.Date(from = as.Date("1900-01-01"), length.out = 4, by = "week")
  dates2 <- seq.Date(from = as.Date("1900-01-01") - 28, length.out = 4, by = "week")
  expect_equal(as.Date(as_yearweek(dates)), dates)
  expect_equal(as.Date(as_yearweek(dates) - 4), dates2)

  expect_snapshot(as_yearweek(dates))
  # this should look like:
  # <grates_quarter[3]>
  # [1] 1900-W01 1900-W02 1900-W03 1900-W04
})

# as_yearweek -----------------------------------------------------------------

test_that("January first dates can be properly converted", {

  # Slightly modified version of tests from Zhian Kamvar's aweek package.
  # This checks that the tricky behaviour at the end/beginning of the year is
  # handled correctly.

  yrs <- 2001:2019
  dates <- sprintf("%d-01-01", yrs)
  dates <- as.Date(c(dates, NA_character_))

  # ISO week (firstday monday)
  dates_iso    <- as_yearweek(dates)
  isoweeks <- c(1, 1, 1, 1, 53, 52, 1, 1, 1, 53, 52, 52, 1, 1, 1, 53, 52, 1, 1)
  expected <- c(
    "2001-W01", "2002-W01", "2003-W01", "2004-W01", "2004-W53",
    "2005-W52", "2007-W01", "2008-W01", "2009-W01", "2009-W53",
    "2010-W52", "2011-W52", "2013-W01", "2014-W01", "2015-W01",
    "2015-W53", "2016-W52", "2018-W01", "2019-W01", NA
  )
  expect_equal(as.character(dates_iso), expected)

  # Epi weeks (firstday sunday)
  dates_epi   <- as_yearweek(dates, 7)
  epiweeks <- c(1, 1, 1, 53, 52, 1, 1, 1, 53, 52, 52, 1, 1, 1, 53, 52, 1, 1, 1, NA)
  epiyears <- c(2001, 2002, 2003, 2003, 2004, 2006, 2007, 2008, 2008, 2009,
                2010, 2012, 2013, 2014, 2014, 2015, 2017, 2018, 2019, NA)
  expect_equal(get_week(dates_epi), epiweeks)
  expect_equal(get_year(dates_epi), epiyears)
})


test_that("as_yearweek.POSIXlt works as expected", {

  nz <- as.POSIXlt("2021-01-04 00:00:00", tz = "NZ")
  result <- as.POSIXlt(as_yearweek(nz), tz = "NZ")
  nz$gmtoff <- result$gmtoff <- NA # HACK but ok for this test
  attr(nz, "tzone") <- attr(result, "tzone") <- "NZ" # HACK but ok for this test
  expect_equal(result, nz)


  dat <- "2021-01-04"
  pos <- as.POSIXlt("2021-01-04", tz = "UTC")
  res1 <- as_yearweek(pos, 1)
  res2 <- as_yearweek(pos, 2)
  res3 <- as_yearweek(pos, 3)
  res4 <- as_yearweek(pos, 4)
  res5 <- as_yearweek(pos, 5)
  res6 <- as_yearweek(pos, 6)
  res7 <- as_yearweek(pos, 7)

  expect_equal(as.Date(res1), as.Date(dat))
  expect_equal(as.Date(res2), as.Date("2020-12-29"))
  expect_equal(as.Date(res3), as.Date("2020-12-30"))
  expect_equal(as.Date(res4), as.Date("2020-12-31"))
  expect_equal(as.Date(res5), as.Date("2021-01-01"))
  expect_equal(as.Date(res6), as.Date("2021-01-02"))
  expect_equal(as.Date(res7), as.Date("2021-01-03"))
})

test_that("as_yearweek.POSIXct works as expected", {
  nz <- as.POSIXct(as.POSIXlt("2021-01-04", tz = "NZ"))
  dat <- as_yearweek(nz)
  result <- as.POSIXct(dat, tz = "NZ")
  expect_equal(result, nz)

  dat <- "2021-01-04"
  pos <- as.POSIXct(as.POSIXlt("2021-01-04", tz = "UTC"))
  res1 <- as_yearweek(pos, 1)
  res2 <- as_yearweek(pos, 2)
  res3 <- as_yearweek(pos, 3)
  res4 <- as_yearweek(pos, 4)
  res5 <- as_yearweek(pos, 5)
  res6 <- as_yearweek(pos, 6)
  res7 <- as_yearweek(pos, 7)
  res <- list(res1, res2, res3, res4, res5, res6, res7)
  expect_snapshot_output(res)

  expect_equal(as.Date(res1), as.Date(dat))
  expect_equal(as.Date(res2), as.Date("2020-12-29"))
  expect_equal(as.Date(res3), as.Date("2020-12-30"))
  expect_equal(as.Date(res4), as.Date("2020-12-31"))
  expect_equal(as.Date(res5), as.Date("2021-01-01"))
  expect_equal(as.Date(res6), as.Date("2021-01-02"))
  expect_equal(as.Date(res7), as.Date("2021-01-03"))
  res2 <- lapply(res, as.Date)
  expect_snapshot_output(res2)

})


test_that("as_yearweek.character works as expected", {
  dat <- "2021-01-04"
  res <- as_yearweek(dat, 1)
  expect_equal(as.Date(res), as.Date(dat))

  dat <- "2020-W53"
  res <- as_yearweek(dat, firstday = 1, format = NULL)
  expect_equal(as.Date(res), as.Date("2020-12-28"))
})


test_that("as_yearweek.factor works as expected", {
  dat <- as.factor("2021-01-04")
  res <- as_yearweek(dat, 1)
  expect_equal(as.Date(res), as.Date(dat))
})


test_that("as_yearweek errors correctly", {
  dat <- Sys.Date()
  expect_error(as_yearweek(dat, "bob"))
  expect_error(as_yearweek(dat, 6.5))
  expect_error(as_yearweek(dat, 8))
  expect_error(as_yearweek(TRUE))
  suppressWarnings(expect_error(as_yearweek("bob")))
  suppressWarnings(expect_error(as_yearweek("2021-W53")))
  suppressWarnings(expect_error(as_yearweek("2021-W54")))
})
# -------------------------------------------------------------------------



# as.xxx.yearweek methods -----------------------------------------------------

test_that("as.POSIXct.yearweek works as expected", {
  dat <- "2021-01-04"
  res <- as.POSIXct(as_yearweek(dat, 1))
  expect_equal(res, as.POSIXct(as.POSIXlt(dat), tz = ""))
})


test_that("as.POSIXlt.yearweek works as expected", {
  dat <- "2021-01-04"
  res <- as.POSIXlt(as_yearweek(dat, 1))
  expect_s3_class(res, "POSIXlt")
  expect_equal(julian(res), julian(as.POSIXlt("2021-01-04")))
})


test_that("as.character.yearweek works as expected", {
  dat <- "2020-12-28"
  res <- as.character(as_yearweek(dat, 1))
  expect_equal(res, "2020-W53")
})

test_that("as.list.yearweek works as expected", {
  dat <- as_yearweek(c("2020-12-28", "2021-01-04"))
  res <- list(as_yearweek("2020-12-28"), as_yearweek("2021-01-04"))
  expect_equal(res, as.list(dat))
})
# -------------------------------------------------------------------------



# accessors ---------------------------------------------------------------

test_that("accessors work", {
  dat <- as_yearweek(as.Date("2020-12-28"))

  expect_equal(get_year(dat), 2020)
  expect_equal(get_week(dat), 53)
  expect_equal(get_firstday(dat), 1)
})

test_that("accessors error when they should", {
  expect_error(get_year("bob"))
  expect_error(get_week("bob"))
  expect_error(get_firstday("bob"))
})
# -------------------------------------------------------------------------



# is_yearweek -----------------------------------------------------------------

test_that("is_yearweek/grate works", {
  dat <- as_yearweek(Sys.Date())
  expect_true(is_yearweek(dat))
  expect_false(is_yearweek("bob"))
  expect_true(is_grate(dat))
  expect_false(is_grate("bob"))
})
# -------------------------------------------------------------------------



# other methods -----------------------------------------------------------
test_that("subsetting works", {
  x <- Sys.Date()
  dat <- as_yearweek(x) + 0:1
  expect_equal(dat[1], as_yearweek(x))
  expect_equal(dat[[2]], as_yearweek(x + 7))

  dat[1] <- dat[2]
  expect_equal(dat[1], as_yearweek(x + 7))

  expect_error(dat[1] <- "bob")
  expect_error(dat[1] <- as_yearweek(x,2))
})

test_that("combine works", {
  x <- Sys.Date()
  dat <- as_yearweek(x)
  expect_equal(c(dat, dat), as_yearweek(c(x, x)))
})

test_that("combine errors correctly", {
  x <- Sys.Date()
  dat1 <- as_yearweek(x,1)
  dat2 <- as_yearweek(x,2)
  expect_error(c(dat1, "bob"))
  expect_error(c(dat1, dat2))
})


# ops ---------------------------------------------------------------------
test_that("comparison operators work", {
  x <- Sys.Date()
  dat1 <- as_yearweek(x, 1)
  dat2 <- as_yearweek(x, 2)

  expect_error(dat1 == dat2)
  expect_error(dat1 != dat2)
  expect_error(dat1 < dat2)
  expect_error(dat1 > dat2)
  expect_error(dat1 <= dat2)
  expect_error(dat1 >= dat2)
  expect_error(dat1 == TRUE)

  expect_true(dat1 == dat1)
  expect_true(dat1 <= dat1 + 1)
  expect_true(dat1 >= dat1 - 1)
  expect_true(dat1 < dat1 + 1)
  expect_true(dat1 > dat1 - 1)
  expect_true(dat1 != dat1 + 1)
})

test_that("addition operation works as expected", {
  x <- as.Date("2021-01-05")
  y <- as.Date("2021-01-04")
  dat1 <- as_yearweek(x)
  dat2 <- dat1 + 0:1

  expect_equal(as.Date(dat2), c(y, y + 7))
  expect_equal(dat2, 0:1 + dat1)
  expect_equal(+dat1, dat1)
  expect_error(dat1 + 1.5)
  expect_error(dat1 + dat1)
})


test_that("subtraction operation works as expected", {
  x <- as.Date("2021-01-05")
  y <- as.Date("2021-01-04")
  dat1 <- as_yearweek(x)
  dat2 <- dat1 - 0:1

  expect_equal(as.Date(dat2), c(y, y - 7))
  expect_equal(as.integer(dat2 - dat1), c(0, -1))
  expect_error(1 - dat1)
  expect_error(-dat1)
  expect_error(dat1 - 1.5)
  expect_error(dat1 - as_yearweek(x, 2))
  expect_error(dat1 + dat1)
})


test_that("Other operations error", {
  x <- as_yearweek(as.Date("2021-01-05"))

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
  x <- as_yearweek(as.Date("2021-01-05"))
  dat <- c(x + 0:1, NA)
  expect_equal(is.nan(dat), c(FALSE, FALSE, FALSE))
  expect_equal(is.finite(dat), c(TRUE, TRUE, FALSE))
  expect_equal(is.infinite(dat), c(FALSE, FALSE, FALSE))
  expect_error(abs(dat))
})
