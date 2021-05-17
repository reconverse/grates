if (require(testthat) && require(dplyr) && require(outbreaks) && require(ggplot2)) {
  library(grates)
  test_check("grates")
} else {
  warning("'grates' requires 'testthat', `dplyr`, `outbreaks` and `ggplot2` for tests")
}
