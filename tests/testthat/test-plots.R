# setup -------------------------------------------------------------------
save_png <- function(code, width = 400, height = 400) {
  path <- tempfile(fileext = ".png")
  png(path, width = width, height = height)
  on.exit(dev.off())
  print(code)
  path
}

expect_snapshot_plot <- function(name, code) {
  skip_if_not_installed("ggplot2", "2.0.0")
  library(ggplot2)
  skip_on_os(c("windows", "mac"))
  skip_on_ci()
  path <- save_png(code)
  expect_snapshot_file(path, paste0(name, ".png"))
}

load_dat <- function() {
  skip_if_not_installed("outbreaks")
  library(outbreaks)
  skip_if_not_installed("dplyr")
  library(dplyr)
  skip_if_not_installed("ggplot2", "2.0.0")
  library(ggplot2)
  outbreaks::ebola_sim_clean$linelist
}
# -------------------------------------------------------------------------

test_that("yearweek plotting works", {
  dat <- load_dat()

  yearweek_monday <-
    dat %>%
    mutate(date = as_yearweek(date_of_infection, firstday = 1)) %>%
    count(date, name = "cases") %>%
    na.omit() %>%
    ggplot(aes(date, cases)) + geom_col(width = 1, colour = "white") + theme_bw() + xlab("")

  yearweek_thursday <-
    dat %>%
    mutate(date = as_yearweek(date_of_infection, firstday = 4)) %>%
    count(date, name = "cases") %>%
    na.omit() %>%
    ggplot(aes(date, cases)) + geom_col(width = 1, colour = "white") + theme_bw() + xlab("")

  expect_snapshot_plot("yearweek_monday", yearweek_monday)
  expect_snapshot_plot("yearweek_thursday", yearweek_thursday)
})


test_that("month plotting works", {
  dat <- load_dat()

  month_dat <-
    dat %>%
    mutate(date = as_month(date_of_infection)) %>%
    count(date, name = "cases") %>%
    na.omit()

  month <-
    month_dat %>%
    ggplot(aes(date, cases)) +
    geom_col(width = 1, colour = "white") +
    scale_x_grates_month(n.breaks = 4, n = 1, origin = 0) +
    theme_bw() +
    xlab("")

  month2 <-
    month_dat %>%
    ggplot(aes(date, cases)) + geom_col(width = 1, colour = "white") + theme_bw() + xlab("") +
    scale_x_grates_month(n.breaks = 4, format = NULL, n = 1, origin = 0)

  expect_snapshot_plot("month", month)
  expect_snapshot_plot("month2", month2)
})


test_that("quarter plotting works", {
  dat <- load_dat()

  quarter <-
    dat %>%
    mutate(date = as_quarter(date_of_infection)) %>%
    count(date, name = "cases") %>%
    na.omit() %>%
    ggplot(aes(date, cases)) +
    geom_col(width = 1, colour = "white") +
    scale_x_grates_quarter(n.breaks = 8) +
    theme_bw() +
    xlab("")

  expect_snapshot_plot("quarter", quarter)
})


test_that("year plotting works", {
  dat <- load_dat()

  year <-
    dat %>%
    mutate(date = as_year(date_of_infection)) %>%
    count(date, name = "cases") %>%
    na.omit() %>%
    ggplot(aes(date, cases)) +
    geom_col(width = 1, colour = "white") +
    scale_x_grates_year(n.breaks = 2) +
    theme_bw() +
    xlab("")

  expect_snapshot_plot("year", year)
})

test_that("period plotting works", {
  dat <- load_dat()

  two_weeks <-
    dat %>%
    mutate(date = as_period(date_of_infection,n = 14)) %>%
    count(date, name = "cases") %>%
    na.omit() %>%
    ggplot(aes(date, cases)) + geom_col(width = 14, colour = "white") + theme_bw() + xlab("")

  expect_snapshot_plot("two_weeks", two_weeks)

  twentyeight_days <-
    dat %>%
    mutate(date = as_period(date_of_infection, n=28)) %>%
    count(date, name = "cases") %>%
    na.omit() %>%
    ggplot(aes(date, cases)) + geom_col(width = 28, colour = "white") + theme_bw() + xlab("")

  expect_snapshot_plot("twentyeight_days", twentyeight_days)


})
