test_that("year constructor and coercion to date works", {
    dates <- seq.Date(from = as.Date("0000-01-01"), length.out = 4, by = "year")
    storage.mode(dates) <- "double" # needed for R <= 4.1.3
    yr4 <- year(0:3)

    expect_identical(as.Date(yr4), dates)

    # formatting --------------------------------------------------------------
    expect_identical(format(yr4), as.character(yr4))
    expect_identical(format(year()), character())
})

test_that("year, pre-epoch dates work", {
    dates <- seq.Date(from = as.Date("1900-01-01"), length.out = 4, by = "year")
    storage.mode(dates) <- "double" # needed for R <= 4.1.3
    dates2 <- seq.Date(from = as.Date("1896-01-01"), length.out = 4, by = "year")
    storage.mode(dates2) <- "double" # needed for R <= 4.1.3
    expect_identical(as.Date(as_year(dates)), dates)
    expect_identical(as.Date(as_year(dates) - 4), dates2)
})



test_that("year, POSIXlt coercion works", {
    nz <- as.POSIXlt("2021-01-04 02:00:00", tz = "NZ")
    utc <- as.POSIXlt("2021-01-01 00:00:00", tz = "UTC")
    result <- as.POSIXlt(as_year(nz))

    expect_identical(result, utc)

    dat <- "2021-01-01"
    pos <- as.POSIXlt("2021-02-04", tz = "UTC")
    res1 <- as_year(pos)

    expect_identical(as.Date(res1), as.Date(dat))

    dat <- "2021-01-01"
    res <- as.POSIXlt(as_year(dat))
    expect_s3_class(res, "POSIXlt")
    expect_identical(julian(res), julian(as.POSIXlt(dat, tz = "UTC")))
    expect_error(
        as.POSIXlt(as_year(dat), tz = "GMT"),
        "<grates_year> objects can only be converted to UTC. If other timezones are required, first convert to <Date> and then proceed as desired.",
        fixed = TRUE
    )
})

test_that("year, POSIXct coercion works", {
    nz <- as.POSIXct(as.POSIXlt("2021-01-04", tz = "NZ"))
    dat <- as_year(nz)
    result <- as.POSIXct(dat)
    expect_identical(result, as.POSIXct(as.POSIXlt("2021-01-01", tz = "UTC")))

    pos <- as.POSIXct(as.POSIXlt("2021-03-04", tz = "UTC"))
    res1 <- as_year(pos)
    expect_identical(as.Date(res1), as.Date("2021-01-01"))

    dat <- "2021-01-01"
    res <- as.POSIXct(as_year(dat))
    expect_identical(res, as.POSIXct(as.POSIXlt(dat), tz = "UTC"))
    expect_error(
        as.POSIXct(as_year(dat), tz = "GMT"),
        "<grates_year> objects can only be converted to UTC. If other timezones are required, first convert to <Date> and then proceed as desired.",
        fixed = TRUE
    )
})

test_that("year, character coercion works", {
    dat <- "2021-01-04"
    res <- as_year(dat)
    expect_identical(as.Date(res), as.Date("2021-01-01"))

    dat <- as.factor("2021-02-04")
    res <- as_year(dat)
    expect_identical(as.Date(res), as.Date("2021-01-01"))

    dat <- "2020-12-28"
    res <- as.character(as_year(dat))
    expect_identical(res, "2020")
})

test_that("as_year, misc errors and warnings", {
    expect_error(
        as_year(TRUE),
        "Not implemented for class [logical].",
        fixed = TRUE
    )
    expect_error(as_year("bob"))

})


test_that("year, as.list works", {
    dat <- as_year(c("2020-12-28", "2021-01-04"))
    res <- list(as_year("2020-12-28"), as_year("2021-01-04"))
    expect_identical(res, as.list(dat))
})

test_that("year, accessors", {
    dat <- as_year(as.Date("2020-12-28"))
    expect_identical(get_year(dat), 2020L)
})

test_that("year, is_year works", {
    dat <- as_year(Sys.Date())
    expect_true(is_year(dat))
    expect_false(is_year("bob"))
})

test_that("year, subsetting works", {
    x <- Sys.Date()
    x2 <- seq.Date(x, by = "year", length.out = 2)[2]
    storage.mode(x2) <- "double" # needed for R <= 4.1.3
    dat <- as_year(x) + 0:1
    dat <- setNames(dat, c("a", "b"))
    expect_identical(dat[1], c(a = as_year(x)))
    expect_identical(dat[[2]], as_year(x2))
    dat[1] <- as_year(x2)
    expect_identical(dat[1], c(a = as_year(x2)))
    dat[[2]] <- dat[[1]]
    expect_identical(dat[[2]], dat[[1]])
    expect_error(
        dat[1] <- "bob",
        "Can only assign a <grates_year> object into a <grates_year> object.",
        fixed = TRUE
    )
})

test_that("year, combine works", {
    x <- Sys.Date()
    dat <- as_year(x)
    expect_identical(c(dat, dat), as_year(c(x, x)))
    expect_error(
        c(dat, "bob"),
        "Unable to combine <grates_year> objects with other classes.",
        fixed = TRUE
    )
})

test_that("year operators and math work", {
    # comparison operators ----------------------------------------------------
    x <- Sys.Date()
    dat1 <- as_year(x)
    dat2 <- as_year(x)
    expect_true(dat1 == dat2) # nolint: expect_comparison_linter. False-positive
    expect_false(dat1 != dat2)
    expect_true(dat1 == dat1) # nolint: expect_comparison_linter. False-positive
    expect_lte(dat1, dat1 + 1)
    expect_gte(dat1, dat1 - 1)
    expect_lt(dat1, dat1 + 1)
    expect_gt(dat1, dat1 - 1)
    expect_true(dat1 != dat1 + 1)
    expect_error(
        dat1 == TRUE, # nolint: redundant_equals_linter. False-positive.
        "Can only compare <grates_year> objects with <grates_year> objects.",
        fixed = TRUE
    )

    # addition ----------------------------------------------------------------
    x <- as.Date("2021-01-05")
    y <- as.Date("2021-01-04")
    dat1 <- as_year(x)
    dat2 <- dat1 + 0:1
    expect_identical(as.Date(dat2), as.Date(c("2021-01-01", "2022-01-01")))
    expect_identical(dat2, 0:1 + dat1)
    expect_identical(+dat1, dat1)
    expect_error(
        dat1 + 1.5,
        "Can only add integers to <grates_year> objects.",
        fixed = TRUE
    )
    expect_error(
        dat1 + dat1,
        "Cannot add <grates_year> objects to each other.",
        fixed = TRUE
    )

    # subtraction -------------------------------------------------------------
    x <- as.Date("2021-01-05")
    y <- as.Date("2021-01-04")
    dat1 <- as_year(x)
    dat2 <- dat1 - 0:1
    expect_identical(as.Date(dat2), as.Date(c("2021-01-01", "2020-01-01")))
    expect_identical(as.integer(dat2 - dat1), c(0L, -1L))
    expect_error(
        1 - dat1,
        "Can only subtract from a <grates_year> object, not vice-versa.",
        fixed = TRUE
    )
    expect_error(
        -dat1,
        "Cannot negate a <grates_year> object.",
        fixed = TRUE
    )
    expect_error(
        dat1 - 1.5,
        "Can only subtract whole numbers and other <grates_year> objects from <grates_year> objects.",
        fixed = TRUE
    )

    # Other operations error
    x <- as_year(as.Date("2021-01-05"))
    expect_error(dat * 3)
    expect_error(dat / 3)
    expect_error(dat ^ 3)
    expect_error(dat %% 3)
    expect_error(dat %/% 3)
    expect_error(dat & 3)
    expect_error(dat | 3)
    expect_error(!dat)

    # Math
    x <- as_year(as.Date("2021-01-05"))
    dat <- c(x + 0:1, year(NA_integer_))
    expect_identical(is.nan(dat), c(FALSE, FALSE, FALSE))
    expect_identical(is.finite(dat), c(TRUE, TRUE, FALSE))
    expect_identical(is.infinite(dat), c(FALSE, FALSE, FALSE))
    expect_error(abs(dat))
})

test_that("year, miscellaneous work", {
    expect_identical(year(-1.5), year(-2L))
    expect_error(year("bob"), "`x` must be integer.", fixed = TRUE)
    expect_error(
        as_year(NA_character_),
        "Unable to parse any entries of `x` as Dates.",
        fixed = TRUE
    )
    dat <- as.Date("2020-01-20")
    dat <- c(dat, dat - 40L)
    dat <- as_year(dat)
    expect_identical(rep(dat, 2L), c(dat, dat))
    expect_identical(rep(dat, each = 2L), c(dat[[1]], dat[[1]], dat[[2]], dat[[2]]))
    expect_identical(unique(c(dat, dat)), dat)
    dat <- as_year(as.Date("1970-01-01"))
    expect_identical(
        seq(dat, dat + 11, by = 2L),
        year(c(1970L, 1972L, 1974L, 1976L, 1978L, 1980L))
    )
    expect_error(
        seq(dat, dat + 11, by = 2.5),
        "`by` must be an integer of length 1.",
        fixed = TRUE
    )
    expect_error(
        seq(dat, as.integer(dat + 11), by = 2.5),
        "`to` must be a <grates_year> object of length 1.",
        fixed = TRUE
    )
    expect_identical(as.integer(year(100L)), 100L)
    expect_identical(as.double(year(100L)), 100)
    expect_identical(min(c(dat, dat + 11)), dat)
    expect_identical(max(c(dat, dat + 11)), dat + 11)
    expect_identical(range(seq(dat, dat + 12, by = 2L)), c(dat, dat + 12))
    expect_error(
        any(dat),
        "`any()` is not supported for <grates_year> objects.",
        fixed = TRUE
    )

    expect_false(is.numeric(year(1)))


})

test_that("year boundary functions work", {
    dates <- as.Date("2020-01-01") + seq.int(50, 5000, 50)
    years <- as_year(dates)
    starts <- as.Date(years)
    ends <- lapply(starts, function(x) seq(x, by = "1 year", length.out = 2L)[2L])
    ends <- do.call(c, ends) - 1L
    expect_identical(date_start(years), starts)
    expect_identical(date_end(years), ends)
})
