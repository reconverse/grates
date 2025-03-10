# TODO - this was hacked together from test-period.R in a rush so you will see
#        a lot of random as.integer(as.Date()) conversions which will make you
#        wonder I've been up do! Such is life.

test_that("int_int_period constructor and coercion to integer works", {
    dat <- 0:3
    datesw <- seq.int(from = 0, length.out = 4, by = 7)
    expect_identical(as.integer(new_int_period(dat, n = 7)), datesw)
    expect_error(new_int_period(dat, n = -1L))

    # formatting --------------------------------------------------------------
    dat <- 0:3
    expect_identical(
        format(new_int_period(dat, n = 7)),
        c(
            "[0, 6]",
            "[7, 13]",
            "[14, 20]",
            "[21, 27]"
        )
    )

    expect_identical(format(new_int_period()), character())
})

test_that("int_period, negative values work", {
    dates <- seq.int(from = -7*30, length.out = 4, by = 7)
    dates2 <- seq.int(from = -7*30 - 28, length.out = 4, by = 7)
    expect_identical(as.integer(as_int_period(dates, n=7)), dates)
    expect_identical(as.integer(as_int_period(dates, n=7) - 4), dates2)
})

test_that("as_int_period, misc errors and warnings", {
    expect_error(as_int_period(TRUE))
    expect_error(as_int_period("bob"))
})

test_that("int_period, as.list works", {
    dat <- as_int_period(c(0,3), n = 2)
    res <- list(as_int_period(0, n = 2), as_int_period(3, n = 2))
    expect_identical(res, as.list(dat))
})

test_that("int_period, accessors", {
    dat <- as_int_period(67, n = 55)
    expect_identical(get_n(dat), 55L)

})

test_that("period, is_int_period works", {
    dat <- as_int_period(10, n = 2)
    expect_true(is_int_period(dat))
    expect_false(is_int_period("bob"))
})

test_that("period, subsetting works", {
    x <- 11
    dat <- as_int_period(x, n = 31) + 0:1
    expect_identical(dat[1], as_int_period(x, n = 31))
    expect_identical(dat[[2]], as_int_period(x + 31, n = 31))

    dat[1] <- dat[2]
    expect_identical(dat[1], as_int_period(x + 31, n = 31))

    expect_error(dat[1] <- "bob")
    expect_error(dat[1] <- as_yrwk(x))
})

test_that("period, combine works", {
    x <- as.integer(Sys.Date())
    dat1 <- as_int_period(x, n = 2)
    dat2 <- as_yearweek(.Date(x))
    expect_error(c(dat1, "bob"))
    expect_error(c(dat1, dat2))
    dat <- as_int_period(x, n = 2)
    expect_identical(c(dat, dat), as_int_period(c(x, x), n = 2))
})

test_that("period operators and math work", {
    # comparison operators ----------------------------------------------------
    x <- as.integer(Sys.Date())
    dat <- as_int_period(x, n = 2L)
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
        "Can only compare <grates_int_period> objects with <grates_int_period> objects.",
        fixed = TRUE
    )

    # addition ----------------------------------------------------------------
    x <- 9
    dat1 <- as_int_period(x, n = 2L)
    dat2 <- dat1 + 0:1
    expected <- c(8L, 10L)
    expect_identical(as.integer(dat2), expected)
    expect_identical(dat2, 0:1 + dat1)
    expect_identical(+dat1, dat1)
    expect_error(
        dat1 + 1.5,
        "Can only add integers to <grates_int_period> objects.",
        fixed = TRUE
    )
    expect_error(
        dat1 + dat1,
        "Cannot add <grates_int_period> objects to each other.",
        fixed = TRUE
    )

    # subtraction -------------------------------------------------------------
    x <- 9
    dat1 <- as_int_period(x, n = 2)
    dat2 <- dat1 - 0:1
    expected <- c(8L, 6L)
    expect_identical(as.integer(dat2), expected)

    expect_error(
        1 - dat1,
        "Can only subtract from a <grates_int_period> object, not vice-versa.",
        fixed = TRUE
    )
    expect_error(
        -dat1,
        "Cannot negate a <grates_int_period> object.",
        fixed = TRUE
    )
    expect_error(
        dat1 - 1.5,
        "Can only subtract whole numbers and other <grates_int_period> objects from <grates_int_period> objects.",
        fixed = TRUE
    )
    expect_error(
        dat1 + dat1,
        "Cannot add <grates_int_period> objects to each other.",
        fixed = TRUE
    )

    # Other operations error
    x <- as_int_period(55, n = 2)
    expect_error(dat * 3)
    expect_error(dat / 3)
    expect_error(dat ^ 3)
    expect_error(dat %% 3)
    expect_error(dat %/% 3)
    expect_error(dat & 3)
    expect_error(dat | 3)
    expect_error(!dat)

    # Math
    x <- as_int_period(as.integer(as.Date("2021-01-05")), n = 2)
    dat <- c(x + 0:1, new_int_period(NA_integer_, n = 2L))
    expect_identical(is.nan(dat), c(FALSE, FALSE, FALSE))
    expect_identical(is.finite(dat), c(TRUE, TRUE, FALSE))
    expect_identical(is.infinite(dat), c(FALSE, FALSE, FALSE))
    expect_error(abs(dat))
})

test_that("int_period, miscellaneous work", {
    expect_identical(new_int_period(-1.5), new_int_period(-2L))
    expect_error(new_int_period("bob"), "`x` must be integer.", fixed = TRUE)
    expect_error(
        new_int_period(0, 1.5),
        "`n` must be an integer of length 1.",
        fixed = TRUE
    )
    expect_error(
        new_int_period(0, 0:1),
        "`n` must be an integer of length 1.",
        fixed = TRUE
    )

    expect_error(
        new_int_period(0, 0),
        "`n` must be greater than 0.",
        fixed = TRUE
    )

    expect_error(
        as_int_period(9, 0:1),
        "`n` must be an integer of length 1.",
        fixed = TRUE
    )

    expect_error(
        as_int_period(9, 1.5),
        "`n` must be an integer of length 1.",
        fixed = TRUE
    )

    dat <- as.integer(Sys.Date())
    dat <- c(dat, dat - 45L)
    dat <- as_int_period(dat)
    expect_identical(rep(dat, 2L), c(dat, dat))
    expect_identical(rep(dat, each = 2L), c(dat[[1]], dat[[1]], dat[[2]], dat[[2]]))
    expect_identical(unique(c(dat, dat)), dat)
    dat <- as_int_period(as.integer(as.Date("1970-01-01")))
    expect_identical(
        seq(dat, dat + 6L, by = 2L),
        new_int_period(c(0L, 2L, 4L, 6L))
    )
    expect_error(
        seq(dat, dat + 11, by = 2.5),
        "`by` must be an integer of length 1.",
        fixed = TRUE
    )
    dat2 <- as_int_period(as.integer(dat+11), n = 3)
    expect_error(
        seq(dat, dat2, by = 2),
        "`to` must have the same integer grouping as `from`",
        fixed = TRUE
    )

    expect_error(
        seq(dat, as.integer(dat + 11), by = 2.5),
        "`to` must be a <grates_int_period> object of length 1.",
        fixed = TRUE
    )
    expect_identical(as.integer(new_int_period(100L)), 100L)
    expect_identical(as.double(new_int_period(100L)), 100)
    expect_identical(min(c(dat, dat+11)), dat)
    expect_identical(max(c(dat, dat+11)), dat+11)
    expect_identical(range(seq(dat, dat + 12, by = 2L)), c(dat, dat+12))
    expect_error(
        any(dat),
        "`any()` is not supported for <grates_int_period> objects.",
        fixed = TRUE
    )
    expect_error(
        c(as_int_period(as.integer(Sys.Date()), n = 2L), as_int_period(as.integer(Sys.Date()), n = 3L)),
        "Unable to combine <grates_int_period> objects with different groupings.",
        fixed = TRUE
    )
    x <- as.integer(Sys.Date())
    expect_false(c(as_int_period(x, n = 2L) == as_int_period(x, n = 3L)))
    expect_true(c(as_int_period(x, n = 2L) != as_int_period(x, n = 3L)))

    dat1 <- as_int_period(x, n = 2L)
    dat2 <- dat1 + 1L
    expect_identical(dat2 - dat1, 1L)

    expect_error(
        as_int_period(x, n = 2L) - as_int_period(x, n = 3L),
        "<grates_int_period> objects must have the same integer grouping to perform subtraction.",
        fixed = TRUE
    )
    expect_false(is.numeric(new_int_period(1)))
})
