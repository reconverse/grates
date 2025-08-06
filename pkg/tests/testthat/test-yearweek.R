test_that("yearweek constructor and coercion to date works", {
    dates <- seq.Date(from = as.Date("1970-01-01"), length.out = 4, by = "week")
    storage.mode(dates) <- "double" # needed for R <= 4.1.3
    yw4 <- new_yearweek(0:3, firstday = 4L)

    expect_identical(as.Date(yw4), dates)

    expect_identical(format(yw4), c("1970-W01", "1970-W02", "1970-W03", "1970-W04"))
    expect_identical(format(new_yearweek()), character())

    dat <- as.Date("1900-01-01") + seq.int(from = 0L, to = 200L * 365, by = 180L)
    expected <- as_yearweek(dat, firstday = 3L)
    tmp <- as.character(expected)
    years <- as.integer(substr(tmp, 1L, 4L))
    weeks <- as.integer(substr(tmp, 7L, 8L))
    expect_identical(yearweek(years, weeks, 3L), expected)
})

test_that("yearweek, pre-epoch dates work", {
    dates <- seq.Date(from = as.Date("1900-01-01"), length.out = 4, by = "week")
    storage.mode(dates) <- "double" # needed for R <= 4.1.3
    dates2 <- seq.Date(from = as.Date("1900-01-01") - 28, length.out = 4, by = "week")
    storage.mode(dates2) <- "double" # needed for R <= 4.1.3
    expect_identical(as.Date(as_yearweek(dates)), dates)
    expect_identical(as.Date(as_yearweek(dates) - 4), dates2)
})

test_that("yearweek, january 1 dates", {
    # january 1 dates ---------------------------------------------------------
    # Slightly modified version of tests from Zhian Kamvar's aweek package.
    # This checks that the tricky behaviour at the end/beginning of the year is
    # handled correctly.

    yrs <- 2001:2019
    dates <- sprintf("%d-01-01", yrs)
    dates <- as.Date(c(dates, NA_character_))

    # ISO week (firstday monday)
    dates_iso    <- as_yearweek(dates)
    expected <- c(
        "2001-W01", "2002-W01", "2003-W01", "2004-W01", "2004-W53",
        "2005-W52", "2007-W01", "2008-W01", "2009-W01", "2009-W53",
        "2010-W52", "2011-W52", "2013-W01", "2014-W01", "2015-W01",
        "2015-W53", "2016-W52", "2018-W01", "2019-W01", NA
    )
    expect_identical(as.character(dates_iso), expected)
    expect_identical(
        suppressWarnings(as_yearweek(as.character(dates_iso), format = "yearweek")),
        dates_iso
    )

    years <- as.integer(substr(expected, 1L, 4L))
    weeks <- as.integer(substr(expected, 7L, 8L))
    expect_identical(dates_iso, suppressWarnings(yearweek(years, weeks)))

    # Epi weeks (firstday sunday)
    epiweeks <- c(1, 1, 1, 53, 52, 1, 1, 1, 53, 52, 52, 1, 1, 1, 53, 52, 1, 1, 1, NA)
    epiyears <- c(2001, 2002, 2003, 2003, 2004, 2006, 2007, 2008, 2008, 2009,
                  2010, 2012, 2013, 2014, 2014, 2015, 2017, 2018, 2019, NA)

    dates_epi   <- as_yearweek(dates, 7)
    expect_identical(dates_epi, suppressWarnings(yearweek(epiyears, epiweeks, 7L)))

    expected <- sprintf("%d-W%02d", epiyears, epiweeks)
    expected[length(expected)] <- NA_character_
    expect_identical(as.character(dates_epi), expected)
})

test_that("yearweek, POSIXlt coercion works", {
    nz <- as.POSIXlt("2021-01-04 02:00:00", tz = "NZ")
    utc <- as.POSIXlt("2021-01-04 00:00:00", tz = "UTC")
    result <- as.POSIXlt(as_yearweek(nz))

    expect_identical(result, utc)

    dat <- "2021-01-04"
    pos <- as.POSIXlt("2021-01-04", tz = "UTC")
    res1 <- as_yearweek(pos, 1)
    res2 <- as_yearweek(pos, 2)
    res3 <- as_yearweek(pos, 3)
    res4 <- as_yearweek(pos, 4)
    res5 <- as_yearweek(pos, 5)
    res6 <- as_yearweek(pos, 6)
    res7 <- as_yearweek(pos, 7)

    expect_identical(as.Date(res1), as.Date(dat))
    expect_identical(as.Date(res2), as.Date("2020-12-29"))
    expect_identical(as.Date(res3), as.Date("2020-12-30"))
    expect_identical(as.Date(res4), as.Date("2020-12-31"))
    expect_identical(as.Date(res5), as.Date("2021-01-01"))
    expect_identical(as.Date(res6), as.Date("2021-01-02"))
    expect_identical(as.Date(res7), as.Date("2021-01-03"))

    dat <- "2021-01-04"
    res <- as.POSIXlt(as_yearweek(dat, 1))
    expect_s3_class(res, "POSIXlt")
    expect_identical(julian(res), julian(as.POSIXlt("2021-01-04", tz = "UTC")))
    expect_error(
        as.POSIXlt(as_yearweek(dat), tz = "GMT"),
        "<grates_yearweek> objects can only be converted to UTC. If other timezones are required, first convert to <Date> and then proceed as desired.",
        fixed = TRUE
    )
})

test_that("yearweek, POSIXct coercion works", {
    nz <- as.POSIXct(as.POSIXlt("2021-01-04", tz = "NZ"))
    dat <- as_yearweek(nz)
    result <- as.POSIXct(dat)
    expect_identical(result, as.POSIXct(as.POSIXlt("2021-01-04", tz = "UTC")))

    dat <- "2021-01-04"
    pos <- as.POSIXct(as.POSIXlt("2021-01-04", tz = "UTC"))
    res1 <- as_yearweek(pos, 1)
    expect_identical(as.Date(res1), as.Date(dat))
    res2 <- as_yearweek(pos, 2)
    expect_identical(as.Date(res2), as.Date("2020-12-29"))
    res3 <- as_yearweek(pos, 3)
    expect_identical(as.Date(res3), as.Date("2020-12-30"))
    res4 <- as_yearweek(pos, 4)
    expect_identical(as.Date(res4), as.Date("2020-12-31"))
    res5 <- as_yearweek(pos, 5)
    expect_identical(as.Date(res5), as.Date("2021-01-01"))
    res6 <- as_yearweek(pos, 6)
    expect_identical(as.Date(res6), as.Date("2021-01-02"))
    res7 <- as_yearweek(pos, 7)
    expect_identical(as.Date(res7), as.Date("2021-01-03"))

    dat <- "2021-01-04"
    res <- as.POSIXct(as_yearweek(dat, 1))
    expect_identical(res, as.POSIXct(as.POSIXlt(dat), tz = "UTC"))
    expect_error(
        as.POSIXct(as_yearweek(dat), tz = "GMT"),
        "<grates_yearweek> objects can only be converted to UTC. If other timezones are required, first convert to <Date> and then proceed as desired.",
        fixed = TRUE
    )
})

test_that("yearweek, character coercion works", {
    dat <- "2021-01-04"
    res <- as_yearweek(dat, 1)
    expect_identical(as.Date(res), as.Date(dat))

    dat <- as.factor("2021-01-04")
    res <- as_yearweek(dat, 1)
    expect_identical(as.Date(res), as.Date(dat))

    dat <- "2020-12-28"
    res <- as.character(as_yearweek(dat, 1))
    expect_identical(res, "2020-W53")
})

test_that("as_yearweek, misc errors and warnings", {
    dat <- Sys.Date()
    expect_error(as_yearweek(dat, "bob"))

    expect_error(
        as_yearweek(dat, 6.5),
        "`firstday` must be an integer of length 1.",
        fixed = TRUE
    )

    expect_error(
        as_yearweek(dat, 8),
        "`firstday` must be an integer between 1 (Monday) and 7 (Sunday).",
        fixed = TRUE
    )

    expect_error(
        as_yearweek(TRUE),
        "Not implemented for class [logical].",
        fixed = TRUE
    )

    expect_error(as_yearweek("bob"))

    expect_warning(
        as_yearweek("bob", format = "yearweek"),
        "Some entries invalid for given `year` and `week` values. Returning these as NA.",
        fixed = TRUE
    )
})


test_that("yearweek, as.list works", {
    dat <- as_yearweek(c("2020-12-28", "2021-01-04"))
    res <- list(as_yearweek("2020-12-28"), as_yearweek("2021-01-04"))
    expect_identical(res, as.list(dat))
})

test_that("yearweek, accessors", {
    dat <- as_yearweek(as.Date("2020-12-28"))
    expect_identical(get_year(dat), 2020L)
    expect_identical(get_week(dat), 53)
    expect_identical(get_firstday(dat), 1L)
    expect_error(get_year("bob"))
    expect_error(get_week("bob"))
    expect_error(get_firstday("bob"))

    expect_identical(
        get_firstday(as_yearweek(as.Date("2020-12-28"), firstday = 2L)),
        2L
    )

    expect_identical(
        get_firstday(as_yearweek(as.Date("2020-12-28"), firstday = 3L)),
        3L
    )

    expect_identical(
        get_firstday(as_yearweek(as.Date("2020-12-28"), firstday = 4L)),
        4L
    )

    expect_identical(
        get_firstday(as_yearweek(as.Date("2020-12-28"), firstday = 5L)),
        5L
    )

    expect_identical(
        get_firstday(as_yearweek(as.Date("2020-12-28"), firstday = 6L)),
        6L
    )

    expect_identical(
        get_firstday(as_yearweek(as.Date("2020-12-28"), firstday = 7L)),
        7L
    )
})

test_that("yearweek, is_yearweek works", {
    dat <- as_yearweek(Sys.Date())
    expect_true(is_yearweek(dat))
    expect_false(is_yearweek("bob"))
})

test_that("yearweek, subsetting works", {
    x <- Sys.Date()
    dat <- as_yearweek(x) + 0:1
    dat <- setNames(dat, c("a", "b"))
    expect_identical(dat[1], c(a = as_yearweek(x)))
    expect_identical(dat[[2]], as_yearweek(x + 7))
    dat[1] <- as_yearweek(x + 14)
    expect_identical(dat[1], c(a = as_yearweek(x + 14)))
    dat[[2]] <- dat[[1]]
    expect_identical(dat[[2]], dat[[1]])
    expect_error(
        dat[1] <- "bob",
        "Can only assign a <grates_yearweek> object into a <grates_yearweek> object.",
        fixed = TRUE
    )
    expect_error(
        dat[1] <- as_yearweek(x, 2),
        "Incompatible first day of the week.",
        fixed = TRUE
    )
})

test_that("yearweek, combine works", {
    x <- Sys.Date()
    dat <- as_yearweek(x)
    expect_identical(c(dat, dat), as_yearweek(c(x, x)))
    dat2 <- as_yearweek(x, 2)
    expect_error(
        c(dat, "bob"),
        "Unable to combine <grates_yearweek> objects with other classes.",
        fixed = TRUE
    )
    expect_error(
        c(dat, dat2),
        "Unable to combine <grates_yearweek> objects with different first days of the week.",
        fixed = TRUE
    )
})

test_that("yearweek operators and math work", {
    # comparison operators ----------------------------------------------------
    x <- Sys.Date()
    dat1 <- as_yearweek(x, 1)
    dat2 <- as_yearweek(x, 2)
    expect_false(dat1 == dat2)
    expect_true(dat1 != dat2)
    expect_true(dat1 == dat1) # nolint: expect_comparison_linter. False-positive
    expect_lte(dat1, dat1 + 1)
    expect_gte(dat1, dat1 - 1)
    expect_lt(dat1, dat1 + 1)
    expect_gt(dat1, dat1 - 1)
    expect_true(dat1 != dat1 + 1)
    expect_error(
        dat1 < dat2,
        "Can only compare <grates_yearweek> objects with the same first day of the week.",
        fixed = TRUE
    )
    expect_error(
        dat1 > dat2,
        "Can only compare <grates_yearweek> objects with the same first day of the week.",
        fixed = TRUE
    )
    expect_error(
        dat1 <= dat2,
        "Can only compare <grates_yearweek> objects with the same first day of the week.",
        fixed = TRUE
    )
    expect_error(
        dat1 >= dat2,
        "Can only compare <grates_yearweek> objects with the same first day of the week.",
        fixed = TRUE
    )
    expect_error(
        dat1 == TRUE, # nolint: redundant_equals_linter. False-positive.
        "Can only compare <grates_yearweek> objects with <grates_yearweek> objects.",
        fixed = TRUE
    )

    # addition ----------------------------------------------------------------
    x <- as.Date("2021-01-05")
    y <- as.Date("2021-01-04")
    dat1 <- as_yearweek(x)
    dat2 <- dat1 + 0:1
    expect_identical(as.Date(dat2), c(y, y + 7))
    expect_identical(dat2, 0:1 + dat1)
    expect_identical(+dat1, dat1)
    expect_error(
        dat1 + 1.5,
        "Can only add integers to <grates_yearweek> objects.",
        fixed = TRUE
    )
    expect_error(
        dat1 + dat1,
        "Cannot add <grates_yearweek> objects to each other.",
        fixed = TRUE
    )

    # subtraction -------------------------------------------------------------
    x <- as.Date("2021-01-05")
    y <- as.Date("2021-01-04")
    dat1 <- as_yearweek(x)
    dat2 <- dat1 - 0:1
    expect_identical(as.Date(dat2), c(y, y - 7))
    expect_identical(as.integer(dat2 - dat1), c(0L, -1L))
    expect_error(
        1 - dat1,
        "Can only subtract from a <grates_yearweek> object, not vice-versa.",
        fixed = TRUE
    )
    expect_error(
        -dat1,
        "Cannot negate a <grates_yearweek> object.",
        fixed = TRUE
    )
    expect_error(
        dat1 - 1.5,
        "Can only subtract whole numbers and other <grates_yearweek> objects from <grates_yearweek> objects.",
        fixed = TRUE
    )
    expect_error(
        dat1 - as_yearweek(x, 2),
        "<grates_yearweek> objects must have the same first day of the week to perform subtraction.",
        fixed = TRUE
    )

    # Other operations error
    x <- as_yearweek(as.Date("2021-01-05"))
    expect_error(dat * 3)
    expect_error(dat / 3)
    expect_error(dat ^ 3)
    expect_error(dat %% 3)
    expect_error(dat %/% 3)
    expect_error(dat & 3)
    expect_error(dat | 3)
    expect_error(!dat)

    # Math
    x <- as_yearweek(as.Date("2021-01-05"))
    dat <- c(x + 0:1, new_yearweek(NA_integer_))
    expect_identical(is.nan(dat), c(FALSE, FALSE, FALSE))
    expect_identical(is.finite(dat), c(TRUE, TRUE, FALSE))
    expect_identical(is.infinite(dat), c(FALSE, FALSE, FALSE))
    expect_error(abs(dat))
})

test_that("yearweek, miscellaneous work", {
    expect_identical(new_yearweek(-1.5), new_yearweek(-2L))
    expect_error(new_yearweek("bob"), "`x` must be integer.", fixed = TRUE)
    expect_error(
        new_yearweek(0, 1.5),
        "`firstday` must be an integer of length 1.",
        fixed = TRUE
    )
    expect_error(
        new_yearweek(0, 0:1),
        "`firstday` must be an integer of length 1.",
        fixed = TRUE
    )

    expect_error(
        new_yearweek(0, -1),
        "`firstday` must be an integer between 1 (Monday) and 7 (Sunday).",
        fixed = TRUE
    )

    expect_error(
        new_yearweek(0, 8),
        "`firstday` must be an integer between 1 (Monday) and 7 (Sunday).",
        fixed = TRUE
    )

    expect_error(
        as_yearweek(Sys.Date(), 0:1),
        "`firstday` must be an integer of length 1.",
        fixed = TRUE
    )

    dat <- Sys.Date()
    dat <- c(dat, dat - 45L)
    dat <- as_yearweek(dat)
    expect_identical(rep(dat, 2L), c(dat, dat))
    expect_identical(rep(dat, each = 2L), c(dat[[1]], dat[[1]], dat[[2]], dat[[2]]))
    expect_identical(unique(c(dat, dat)), dat)
    dat <- as_yearweek(as.Date("1970-01-01"))
    expect_identical(
        seq(dat, dat + 6L, by = 2L),
        new_yearweek(c(0L, 2L, 4L, 6L))
    )
    expect_error(
        seq(dat, dat + 11, by = 2.5),
        "`by` must be an integer of length 1.",
        fixed = TRUE
    )
    dat2 <- as_yearweek(as.Date(dat + 11), firstday = 3)
    expect_error(
        seq(dat, dat2, by = 2),
        "`to` must have the same first day of the week as `from`",
        fixed = TRUE
    )

    expect_error(
        seq(dat, as.integer(dat + 11), by = 2.5),
        "`to` must be a <grates_yearweek> object of length 1.",
        fixed = TRUE
    )
    expect_identical(as.integer(new_yearweek(100L)), 100L)
    expect_identical(as.double(new_yearweek(100L)), 100)
    expect_identical(min(c(dat, dat + 11)), dat)
    expect_identical(max(c(dat, dat + 11)), dat + 11)
    expect_identical(range(seq(dat, dat + 12, by = 2L)), c(dat, dat + 12))
    expect_error(
        any(dat),
        "`any()` is not supported for <grates_yearweek> objects.",
        fixed = TRUE
    )

    expect_identical(yearweek(1L, 1L), yearweek(1.5, 1.5))
    expect_error(yearweek(1L), "Cannot recycle a vector of length 0:", fixed = TRUE)
    expect_error(yearweek(1:2, 1:3), "Can only recycle vectors of length 1:", fixed = TRUE)
    expect_error(yearweek(year = character()), "`year` must be integer.", fixed = TRUE)
    expect_error(yearweek(week = character()), "`week` must be integer.")
    expect_error(yearweek(firstday = 1:2), "`firstday` must be an integer of length 1.", fixed = TRUE)
    expect_error(yearweek(firstday = ""), "`firstday` must be an integer of length 1.", fixed = TRUE)
    expect_error(yearweek(firstday = 8L), "`firstday` must be an integer between 1 (Monday) and 7 (Sunday).", fixed = TRUE)
    expect_identical(yearweek(firstday = 1.0), yearweek(firstday = 1L))
    expect_false(is.numeric(yearweek(1, 1)))
})

test_that("yearweek boundary functions work", {
    # taking a long-winded manual approach to this double test various things
    # 2020-01-01 is a wednesday
    dates <- as.Date("2020-01-01") + 0:14
    weeks <- as_yearweek(dates, firstday = 4L) # thursday
    starts <- as.Date("2019-12-26") + integer(15L)
    starts[2:8] <- starts[2:8] + 7L
    starts[9:15] <- starts[9:15] + 14L
    ends <- starts + 7 - 1L
    expect_identical(date_start(weeks), starts)
    expect_identical(date_end(weeks), ends)

    d1 <- as.Date("2019-12-31")
    expected <- logical(15L)
    expected[1L] <- TRUE
    expect_identical(d1 %during% weeks, expected)

    d2 <- as.Date("2020-01-02")
    expected <- logical(15L)
    expected[2:8] <- TRUE
    expect_identical(d2 %during% weeks, expected)

    d3 <- as.Date("2020-01-15")
    expected <- logical(15L)
    expected[9:15] <- TRUE
    expect_identical(d3 %during% weeks, expected)
})
