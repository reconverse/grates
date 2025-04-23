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

    dat <- fastymd::fymd(1,1,1) + 0:999999
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

    dat <- fastymd::fymd(1,1,1) + 0:999999
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

    dat <- fastymd::fymd(1,1,1) + 0:999999
    dat <- as_yearquarter(dat)
    expect_identical(get_year(dat), .get_year_grates_yearquarter(dat))
})


test_that("utc posixlt conversion still works", {

    # The 'old' function below was removed during refactoring so we test for
    # compatible results

    old <- function(x) as.POSIXlt(x * 86400, tz = "UTC", origin = .POSIXct(0, tz = "UTC"))
    new <- function(x) as.POSIXlt(.POSIXct(x * 86400, tz = "UTC"), tz = "UTC")
    dat <- (-99999):99999
    expect_identical(old(dat),new(dat))
})

