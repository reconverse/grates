test_that("Refactoring hasn't broke get_year for yearweek objects", {

    # The two functions below were removed during refactoring so we test for
    # compatible results

    .as_utc_posixlt_from_int <- function(x) {
        x <- as.double(x)
        x <- x * 86400 # multiply by seconds in day (24 * 60 * 60)
        as.POSIXlt(x, tz = "UTC", origin = .POSIXct(xx = 0, tz = "UTC"))
    }

    .get_year_grates_yearweek <- function(x, ...) {
        week <- get_week.grates_yearweek(x)
        dat <- .as_utc_posixlt_from_int(as.Date(x))
        december <- dat$mon == 11L
        january <- dat$mon == 0L
        boundary_adjustment <- integer(length(x)) # h/t Zhian Kamvar for boundary adjustment idea in aweek)
        boundary_adjustment[january  & week >= 52L] <- -1L
        boundary_adjustment[december & week == 1L]  <- 1L
        yr <- dat$year + 1900L
        yr + boundary_adjustment
    }

    dat <- fastymd::fymd(1, 1, 1) + 0:999999
    dat <- as_yearweek(dat)
    expect_identical(get_year(dat), .get_year_grates_yearweek(dat))
})


test_that("Refactoring hasn't broke get_year for yearmonth objects", {

    # The function below was removed during refactoring so we test for
    # compatible results

    .get_year_grates_yearmonth <- function(x, ...) {
        x <- as.POSIXlt(x)
        x$year + 1900L
    }

    dat <- fastymd::fymd(1, 1, 1) + 0:999999
    dat <- as_yearmonth(dat)
    expect_identical(get_year(dat), .get_year_grates_yearmonth(dat))
})


test_that("Refactoring hasn't broke get_year for yearquarter objects", {

    # The function below was removed during refactoring so we test for
    # compatible results

    .get_year_grates_yearquarter <- function(x, ...) {
        x <- as.POSIXlt(x)
        x$year + 1900L
    }

    dat <- fastymd::fymd(1, 1, 1) + 0:999999
    dat <- as_yearquarter(dat)
    expect_identical(get_year(dat), .get_year_grates_yearquarter(dat))
})


test_that("utc posixlt conversion still works", {

    # The 'old' function below was removed during refactoring so we test for
    # compatible results

    old <- function(x) as.POSIXlt(x * 86400, tz = "UTC", origin = .POSIXct(0, tz = "UTC"))
    new <- function(x) as.POSIXlt(.POSIXct(x * 86400, tz = "UTC"), tz = "UTC")
    dat <- (-99999):99999
    expect_identical(old(dat), new(dat))
})


test_that("Refactoring hasn't broken as_yearmonth for date objects", {

    # The function below was removed during refactoring so we test for
    # compatible results

    .as_yearmonth_Date <- function(x, ...) {

        # convert to posixlt (this will always be UTC when called on a date)
        x <- as.POSIXlt(x)

        # calculate the year
        yr <- x$year + 1900L

        # calculate the month relative to unix epoch
        mon <- (yr - 1970L) * 12L + x$mon

        # TODO - could mon ever be double here? Maybe call as_yearmonth again?
        .new_yearmonth(mon)
    }


    dat <- fastymd::fymd(1, 1, 1) + 0:999999
    expect_identical(as_yearmonth(dat), .as_yearmonth_Date(dat))
})


test_that("Refactoring hasn't broken as_yearmonth for date objects", {

    # The function below was removed during refactoring so we test for
    # compatible results

    .as_month_Date <- function(x, n, ...) {

        # trigger warning for missing n at top level
        n <- n

        if (!.is_scalar_whole(n))
            stop("`n` must be an integer of length 1.")
        n <- as.integer(n)
        if (n == 1L)
            stop("`n` must be greater than 1. If single month groupings are required please use `as_yearmonth()`.")

        # convert to posixlt (this will always be UTC when called on a date)
        x <- as.POSIXlt(x)

        # calculate the year
        yr <- x$year + 1900L

        # calculate the month relative to unix epoch
        mon <- (yr - 1970L) * 12L + x$mon

        # scale month by n
        mon <- (mon %/% n)

        # TODO - could mon ever be double here? Is as.integer needed or superfluous?
        .new_month(x = as.integer(mon), n = n)
    }

    dat <- fastymd::fymd(1, 1, 1) + 0:999999
    expect_identical(as_month(dat, n = 7), .as_month_Date(dat, n = 7))
    expect_identical(as_month(dat, n = 2), .as_month_Date(dat, n = 2))
})
