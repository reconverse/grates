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


test_that("yrmon plotting works", {
  dat <- load_dat()

  yrmon <-
    dat %>%
    mutate(date = as_yrmon(date_of_infection)) %>%
    count(date, name = "cases") %>%
    drop_na() %>%
    ggplot(aes(date, cases)) + geom_col() + theme_bw() + xlab("")

  expect_snapshot_plot("yrmon", yrmon)
})


test_that("yrqtr plotting works", {
  dat <- load_dat()

  yrqtr <-
    dat %>%
    mutate(date = as_yrqtr(date_of_infection)) %>%
    count(date, name = "cases") %>%
    drop_na() %>%
    ggplot(aes(date, cases)) + geom_col() + theme_bw() + xlab("")

  expect_snapshot_plot("yrqtr", yrqtr)
})


test_that("yr plotting works", {
  dat <- load_dat()

  yr <-
    dat %>%
    mutate(date = as_yr(date_of_infection)) %>%
    count(date, name = "cases") %>%
    drop_na() %>%
    ggplot(aes(date, cases)) + geom_col() + theme_bw() + xlab("")

  expect_snapshot_plot("yr", yr)
})

test_that("period plotting works", {
  dat <- load_dat()

  two_weeks <-
    dat %>%
    mutate(date = as_period(date_of_infection, "2 weeks")) %>%
    count(date, name = "cases") %>%
    drop_na() %>%
    ggplot(aes(date, cases)) + geom_col() + theme_bw() + xlab("")

  expect_snapshot_plot("two_weeks", two_weeks)

  twentyeight_days <-
    dat %>%
    mutate(date = as_period(date_of_infection, "28 days")) %>%
    count(date, name = "cases") %>%
    drop_na() %>%
    ggplot(aes(date, cases)) + geom_col() + theme_bw() + xlab("")

  expect_snapshot_plot("twentyeight_days", twentyeight_days)


})
