test_that("as_yearmonth.Date works correctly", {
  start <- as.Date("2020-01-01")
  dat <- seq.Date(from = start, to = start + 365, by = 1)
  res <- aggregate(dat, list(as_yearmonth(dat)), length)

  expect_equal(res$x, c(31,29,31,30,31,30,31,31,30,31,30,31))
  expect_equal(
    as.Date(res[[1]]),
    seq.Date(from = start, to = start + 365, by = "month")
  )

  expect_snapshot_output(print(res))
  # This should look like:
  # Group.1  x
  # 1  2020-Jan 31
  # 2  2020-Feb 29
  # 3  2020-Mar 31
  # 4  2020-Apr 30
  # 5  2020-May 31
  # 6  2020-Jun 30
  # 7  2020-Jul 31
  # 8  2020-Aug 31
  # 9  2020-Sep 30
  # 10 2020-Oct 31
  # 11 2020-Nov 30
  # 12 2020-Dec 31
})
