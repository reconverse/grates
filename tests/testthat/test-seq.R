test_that("seq works for grates_month", {
  dat <- as_month("2020-02-29", n = 3)
  dat2 <- as_month("2024-01-05", n = 3)
  res <- seq(dat, dat2)
  expect_equal(dat + 0:(dat2-dat), seq(dat,dat2))

  dat <- as_month("2020-02-29")
  dat2 <- as_month("2024-01-05")
  res <- seq(dat, dat2)
  expect_equal(dat + 0:(dat2-dat), seq(dat,dat2))
})


test_that("seq works for grates_yearweek", {
  dat <- as_yearweek("2020-02-29", firstday = 3)
  dat2 <- as_yearweek("2024-01-05", firstday = 3)
  res <- seq(dat, dat2)
  expect_equal(dat + 0:(dat2-dat), seq(dat,dat2))

  dat <- as_yearweek("2020-02-29")
  dat2 <- as_yearweek("2024-01-05")
  res <- seq(dat, dat2)
  expect_equal(dat + 0:(dat2-dat), seq(dat,dat2))
})

test_that("seq works for grates_year", {
  dat <- as_year("2020-02-29")
  dat2 <- as_year("2024-01-05")
  res <- seq(dat, dat2)
  expect_equal(dat + 0:(dat2-dat), seq(dat,dat2))
})

test_that("seq works for grates_quarter", {
  dat <- as_quarter("2020-02-29")
  dat2 <- as_quarter("2024-01-05")
  res <- seq(dat, dat2)
  expect_equal(dat + 0:(dat2-dat), seq(dat,dat2))
})


test_that("seq works for grates_period", {
  dat <- as_period("2020-02-29", n = 2, origin = 1)
  dat2 <- as_period("2024-01-05", n = 2, origin = 1)
  res <- seq(dat, dat2)
  expect_equal(dat + 0:(dat2-dat), seq(dat,dat2))

  dat <- as_period("2020-02-29")
  dat2 <- as_period("2024-01-05")
  res <- seq(dat, dat2)
  expect_equal(dat + 0:(dat2-dat), seq(dat,dat2))
})
