test_that("quantile for months works", {
  start <- as.Date("2021-01-01")
  dat <- seq.Date(from = start, by = "month", length.out = 1000)
  months <- as_month(dat)
  quantiles <- quantile(months, type = 1, probs = c(0.025, 0.5, 0.975), names = FALSE)
  expect_equal(quantiles, months[c(25,500,975)])
})

test_that("quantile for periods works", {
  start <- as.Date("2021-01-01")
  dat <- seq.Date(from = start, by = "month", length.out = 1000)
  periods <- as_period(dat, n = 60, origin = 2)
  quantiles <- quantile(periods, type = 1, probs = c(0.025, 0.5, 0.975), names = FALSE)
  expect_equal(quantiles, periods[c(25,500,975)])
})

test_that("quantile for int_periods works", {
  dat <- 1:1000
  periods <- as_int_period(dat)
  quantiles <- quantile(periods, type = 1, probs = c(0.025, 0.5, 0.975), names = FALSE)
  expect_equal(quantiles, periods[c(25,500,975)])
})

test_that("quantile for yearweeks works", {
  start <- as.Date("2021-01-01")
  dat <- seq.Date(from = start, by = "week", length.out = 1000)
  weeks <- as_yearweek(dat)
  quantiles <- quantile(weeks, type = 1, probs = c(0.025, 0.5, 0.975), names = FALSE)
  expect_equal(quantiles, weeks[c(25,500,975)])
})

test_that("quantile for quarters works", {
  start <- as.Date("2021-01-01")
  dat <- seq.Date(from = start, by = "quarter", length.out = 1000)
  q <- as_quarter(dat)
  quantiles <- quantile(q, type = 1, probs = c(0.025, 0.5, 0.975), names = FALSE)
  expect_equal(quantiles, q[c(25,500,975)])
})

test_that("quantile for years work", {
  start <- as.Date("2021-01-01")
  dat <- seq.Date(from = start, by = "year", length.out = 1000)
  y <- as_year(dat)
  quantiles <- quantile(y, type = 1, probs = c(0.025, 0.5, 0.975), names = FALSE)
  expect_equal(quantiles, y[c(25,500,975)])
})
