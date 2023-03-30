test_that("month constructor and coercion to date works", {
    dates <- seq.Date(from = as.Date("1970-01-01"), length.out = 6L, by = "2 months")
    storage.mode(dates) <- "double" # needed for R <= 4.1.3
    dat <- new_month(0:5, n=2L)
    dates5 <- seq.Date(from = as.Date("1970-01-01"), length.out = 6L, by = "5 months")
    storage.mode(dates5) <- "double" # needed for R <= 4.1.3
    dat5 <- new_month(0:5, n=5L)

    expect_identical(as.Date(dat), dates)
    expect_identical(as.Date(dat5), dates5)

    # formatting --------------------------------------------------------------
    dates2 <- seq.Date(from = as.Date("1970-01-01"), length.out = 7L, by = "2 months")
    storage.mode(dates2) <- "double" # needed for R <= 4.1.3
    dates2 <- dates2[-1L]
    expect_identical(
        format(dat),
        sprintf("%s to %s", format(dates, "%Y-%b"), format(dates2-1, "%Y-%b"))
    )
    expect_identical(format(new_month(n=2L)), character())
})

test_that("month, pre-epoch dates work", {
    dates <- seq.Date(from = as.Date("1900-01-01"), length.out = 3L, by = "2 months")
    storage.mode(dates) <- "double" # needed for R <= 4.1.3
    expect_identical(as.Date(as_month(dates, n = 2L)), dates)
})

test_that("month, POSIXlt coercion works", {
    nz <- as.POSIXlt("2021-01-04 02:00:00", tz = "NZ")
    utc <- as.POSIXlt("2021-01-01 00:00:00", tz = "UTC")
    result <- as.POSIXlt(as_month(nz, n = 3L))
    expect_identical(result, utc)

    dat <- "2021-01-01"
    pos <- as.POSIXlt("2021-01-04", tz = "UTC")
    res1 <- as_month(pos, n = 3L)

    expect_identical(as.Date(res1), as.Date(dat))

    dat <- "2021-01-01"
    res <- as.POSIXlt(as_month(dat, n = 3L))
    expect_s3_class(res, "POSIXlt")
    expect_identical(julian(res), julian(as.POSIXlt(dat, tz = "UTC")))
    expect_error(
        as.POSIXlt(as_month(dat, n = 3L), tz = "GMT"),
        "<grates_month> objects can only be converted to UTC. If other timezones are required, first convert to <Date> and then proceed as desired.",
        fixed = TRUE
    )
})

test_that("month, accessor works", {
    dat <- as.Date("2021-01-01")
    expect_identical(get_n(as_month(dat, n = 5L)), 5L)
})

test_that("month, POSIXct coercion works", {
    nz <- as.POSIXct(as.POSIXlt("2021-01-04", tz = "NZ"))
    dat <- as_month(nz, n = 2L)
    result <- as.POSIXct(dat)
    expect_identical(result, as.POSIXct(as.POSIXlt("2021-01-01", tz = "UTC")))

    pos <- as.POSIXct(as.POSIXlt("2021-01-04", tz = "UTC"))
    res1 <- as_month(pos, n = 2L)
    expect_identical(as.Date(res1), as.Date("2021-01-01"))

    dat <- "2021-01-01"
    res <- as.POSIXct(as_month(dat, n = 2L))
    expect_identical(res, as.POSIXct(as.POSIXlt(dat), tz = "UTC"))
    expect_error(
        as.POSIXct(as_month(dat, n = 2L), tz = "GMT"),
        "<grates_month> objects can only be converted to UTC. If other timezones are required, first convert to <Date> and then proceed as desired.",
        fixed = TRUE
    )
})

test_that("month, character coercion works", {
    dat <- "2021-01-04"
    res <- as_month(dat, n = 6L)
    expect_identical(as.Date(res), as.Date("2021-01-01"))

    dat <- as.factor("2021-01-04")
    res <- as_month(dat, n = 3L)
    expect_identical(as.Date(res), as.Date("2021-01-01"))

    x <- "2020-12-28"
    dat <- as_month("2020-12-28", n = 2L)
    res <- as.character(dat)
    expect_identical(
        res,
        sprintf("%s to %s", format(as.Date(x)-30L, "%Y-%b"), format(as.Date(x), "%Y-%b"))
    )
})

test_that("as_month, misc errors and warnings", {
    expect_error(
        as_month(TRUE),
        "Not implemented for class [logical].",
        fixed = TRUE
    )

    expect_error(
        as_month(Sys.Date(), n = 1L),
        "`n` must be greater than 1. If single month groupings are required please use `as_yearmonth()`.",
        fixed = TRUE
    )

    expect_error(as_month("bob", n = 2L))
})


test_that("month, as.list works", {
    dat <- as_month(c("2020-12-28", "2021-01-04"), n = 7L)
    res <- list(as_month("2020-12-28", n = 7L), as_month("2021-01-04", n = 7L))
    expect_identical(res, as.list(dat))
})

test_that("month, is_month works", {
    dat <- as_month(Sys.Date(), n = 2L)
    expect_true(is_month(dat))
    expect_false(is_month("bob"))
})

test_that("month, subsetting works", {
    x <- as.Date("2023-02-15")
    dat <- as_month(x, n = 5L) + 0:1
    dat <- setNames(dat, c("a", "b"))
    dat2 <- as.Date(dat)
    expect_identical(dat[1], c(a=as_month(x, n = 5L)))
    expect_identical(dat[[2]], as_month(dat2[[2]], n = 5L))
    dat[1] <- as_month(x+33, n = 5L)
    expect_identical(dat[1], c(a=as_month(x + 33, n = 5L)))
    dat[[2]] <- dat[[1]]
    expect_identical(dat[[2]], dat[[1]])
    expect_error(
        dat[1] <- "bob",
        "Can only assign <grates_month> objects in to an <grates_month> object.",
        fixed = TRUE
    )
})

test_that("month, combine works", {
    x <- Sys.Date()
    dat <- as_month(x, n = 2L)
    expect_identical(c(dat, dat), as_month(c(x, x), n = 2L))
    expect_error(
        c(dat, "bob"),
        "Unable to combine <grates_month> objects with other classes.",
        fixed = TRUE
    )
})

test_that("month operators and math work", {
    # comparison operators ----------------------------------------------------
    x <- Sys.Date()
    dat <- as_month(x, n = 7L)
    expect_true(dat == dat)
    expect_false(dat != dat)
    expect_true(dat == dat)
    expect_true(dat <= dat + 1)
    expect_true(dat >= dat - 1)
    expect_true(dat < dat + 1)
    expect_true(dat > dat - 1)
    expect_true(dat != dat + 1)
    expect_error(
        dat == TRUE,
        "Can only compare <grates_month> objects with <grates_month> objects.",
        fixed = TRUE
    )

    # addition ----------------------------------------------------------------
    x <- as.Date("2021-01-05")
    y <- as.Date("2021-01-04")
    dat1 <- as_month(x, n = 2L)
    dat2 <- dat1 + 0:1
    expect_identical(as.Date(dat2), as.Date(c("2021-01-01", "2021-03-01")))
    expect_identical(dat2, 0:1 + dat1)
    expect_identical(+dat1, dat1)
    expect_error(
        dat1 + 1.5,
        "Can only add integers to <grates_month> objects.",
        fixed = TRUE
    )
    expect_error(
        dat1 + dat1,
        "Cannot add <grates_month> objects to each other.",
        fixed = TRUE
    )

    # subtraction -------------------------------------------------------------
    x <- as.Date("2021-01-05")
    y <- as.Date("2021-01-04")
    dat1 <- as_month(x, n = 2L)
    dat2 <- dat1 - 0:1
    expect_identical(as.Date(dat2), as.Date(c("2021-01-01", "2020-11-01")))
    expect_identical(as.integer(dat2 - dat1), c(0L, -1L))
    expect_error(
        1 - dat1,
        "Can only subtract from a <grates_month> object, not vice-versa.",
        fixed = TRUE
    )
    expect_error(
        -dat1,
        "Cannot negate a <grates_month> object.",
        fixed = TRUE
    )
    expect_error(
        dat1 - 1.5,
        "Can only subtract whole numbers and other <grates_month> objects from <grates_month> objects.",
        fixed = TRUE
    )

    # Other operations error
    x <- as_month(as.Date("2021-01-05"), n = 2L)
    expect_error(dat * 3)
    expect_error(dat / 3)
    expect_error(dat ^ 3)
    expect_error(dat %% 3)
    expect_error(dat %/% 3)
    expect_error(dat & 3)
    expect_error(dat | 3)
    expect_error(!dat)

    # Math
    x <- as_month(as.Date("2021-01-05"), n = 2L)
    dat <- c(x + 0:1, new_month(NA_integer_, n = 2L))
    expect_identical(is.na(dat), c(FALSE, FALSE, TRUE))
    expect_identical(is.nan(dat), c(FALSE, FALSE, FALSE))
    expect_identical(is.finite(dat), c(TRUE, TRUE, FALSE))
    expect_identical(is.infinite(dat), c(FALSE, FALSE, FALSE))
    expect_error(abs(dat))

})

test_that("month, miscellaneous work", {
    expect_identical(new_month(-1.5, n = 2L), new_month(-2L, n = 2L))
    expect_error(new_month("bob", n = 2L), "`x` must be integer.", fixed = TRUE)
    expect_error(
        as_month(NA_character_, n = 2L),
        "Unable to parse any entries of `x` as Dates.",
        fixed = TRUE
    )
    dat <- Sys.Date()
    dat <- c(dat, dat - 90L)
    dat <- as_month(dat, 2L)
    expect_identical(rep(dat, 2L), c(dat, dat))
    expect_identical(rep(dat, each = 2L), c(dat[[1]], dat[[1]], dat[[2]], dat[[2]]))
    expect_identical(unique(c(dat, dat)), dat)
    dat <- as_month(as.Date("1970-01-01"), n = 3L)
    expect_identical(
        seq(dat, dat + 11, by = 2L),
        new_month(c(0L, 2L, 4L, 6L, 8L, 10L), n = 3L)
    )
    expect_error(
        seq(dat, dat + 11, by = 2.5),
        "`by` must be an integer of length 1.",
        fixed = TRUE
    )
    expect_error(
        seq(dat, as.integer(dat + 11), by = 2.5),
        "`to` must be a <grates_month> object of length 1.",
        fixed = TRUE
    )
    expect_identical(as.integer(new_month(100L, n = 5L)), 100L)
    expect_identical(as.double(new_month(100L, n = 2L)), 100)
    expect_identical(min(c(dat, dat+11)), dat)
    expect_identical(max(c(dat, dat+11)), dat+11)
    expect_identical(range(seq(dat, dat + 12, by = 2L)), c(dat, dat+12))
    expect_error(
        any(dat),
        "`any()` is not supported for <grates_month> objects.",
        fixed = TRUE
    )

    expect_error(
        new_month(1L, n = 1.5),
        "`n` must be an integer of length 1.",
        fixed = TRUE
    )

    expect_error(
        new_month(1L, n = 1:2),
        "`n` must be an integer of length 1.",
        fixed = TRUE
    )

    expect_error(
        new_month(1L, n = 1L),
        "`n` must be greater than 1. If single month groupings are required please use `yearmonth()`.",
        fixed = TRUE
    )

    expect_error(
        c(as_month(Sys.Date(), n = 2L), as_month(Sys.Date(), n = 3L)),
        "Unable to combine <grates_month> objects with different groupings.",
        fixed = TRUE
    )

    expect_false(c(as_month(Sys.Date(), n = 2L) == as_month(Sys.Date(), n = 3L)))
    expect_true(c(as_month(Sys.Date(), n = 2L) != as_month(Sys.Date(), n = 3L)))

    dat1 <- as_month(Sys.Date(), n = 2L)
    dat2 <- dat1 + 1L
    expect_identical(dat2 - dat1, 1L)

    expect_error(
        as_month(Sys.Date(), n = 2L) - as_month(Sys.Date(), n = 3L),
        "<grates_month> objects must have the same month grouping to perform subtraction.",
        fixed = TRUE
    )
})

