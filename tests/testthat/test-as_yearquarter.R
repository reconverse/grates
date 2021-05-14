test_that("as_yearmonth.Date works correctly", {
  start <- as.Date("2020-01-01")
  dat <- seq.Date(from = start, to = start + 365, by = 1)
  res <- aggregate(dat, list(as_yearquarter(dat)), length)

  expect_equal(res$x, c(91, 91, 92, 92))
  expect_equal(
    as.Date(res[[1]]),
    seq.Date(from = start, to = start + 365, by = "quarter")
  )

  expect_snapshot_output(print(res))
  # This should look like:
  # Group.1  x
  # 1  2020-Q1 91
  # 2  2020-Q2 91
  # 3  2020-Q3 92
  # 4  2020-Q4 92
})
