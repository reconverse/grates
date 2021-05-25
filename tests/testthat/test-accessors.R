# # accessors ---------------------------------------------------------------
#
# test_that("accessors error when they should", {
#   expect_error(get_date_bounds("bob"))
# })
#
# test_that("accessors work", {
#   dat <- as_month(as.Date("2020-12-28"), n = 4, origin = 10)
#   expected <- data.frame(
#     grouping = dat,
#     lower_bound = as.Date("2020-11-01"),
#     upper_bound = as.Date("2021-02-28")
#   )
#   expect_equal(get_date_bounds(dat), expected)
# })
# # -------------------------------------------------------------------------
