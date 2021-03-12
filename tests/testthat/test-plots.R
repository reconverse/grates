# setup -------------------------------------------------------------------
library(outbreaks)
library(dplyr)
library(tidyr)
library(ggplot2)

save_png <- function(code, width = 400, height = 400) {
  path <- tempfile(fileext = ".png")
  png(path, width = width, height = height)
  on.exit(dev.off())
  print(code)
  path
}

expect_snapshot_plot <- function(name, code) {
  skip_if_not_installed("ggplot2", "2.0.0")
  skip_on_os(c("windows", "mac"))
  skip_on_ci()
  path <- save_png(code)
  expect_snapshot_file(path, paste0(name, ".png"))
}

load_dat <- function() {
  skip_if_not_installed("outbreaks")
  outbreaks::ebola_sim_clean$linelist
}
# -------------------------------------------------------------------------


# yrwk --------------------------------------------------------------------

test_that("yrwk plotting works", {
  dat <- load_dat()

  yrwk_monday <-
    dat %>%
    mutate(date = as_yrwk(date_of_infection, firstday = 1)) %>%
    count(date, name = "cases") %>%
    drop_na() %>%
    ggplot(aes(date, cases)) + geom_col() + theme_bw() + xlab("")

  yrwk_thursday <-
    dat %>%
    mutate(date = as_yrwk(date_of_infection, firstday = 4)) %>%
    count(date, name = "cases") %>%
    drop_na() %>%
    ggplot(aes(date, cases)) + geom_col() + theme_bw() + xlab("")

  expect_snapshot_plot("yrwk_monday", yrwk_monday)
  expect_snapshot_plot("yrwk_thursday", yrwk_thursday)
})
# -------------------------------------------------------------------------


