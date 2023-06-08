save_png <- function(code, width = 400, height = 400) {
    path <- tempfile(fileext = ".png")
    png(path, width = width, height = height)
    on.exit(dev.off())
    print(code)
    path
}

expect_snapshot_plot <- function(name, code) {
    skip_if_not_installed("ggplot2", "2.0.0")
    skip_on_os("windows", "mac")
    skip_on_ci()
    name <- paste0(name, ".png")

    # Announce the file before touching `code`. This way, if `code`
    # unexpectedly fails or skips, testthat will not auto-delete the
    # corresponding snapshot file.
    announce_snapshot_file(name = name)

    path <- save_png(code)
    expect_snapshot_file(path, name)
}

test_that("yearweek plotting works", {
    skip_if_not_installed("outbreaks")
    skip_if_not_installed("dplyr")
    skip_if_not_installed("ggplot2", "2.0.0")
    dat <- outbreaks::ebola_sim_clean$linelist

    yearweek_monday <-
        dat |>
        dplyr::mutate(date = as_yearweek(date_of_infection, firstday = 1L)) |>
        dplyr::count(date, name = "cases") |>
        na.omit() |>
        ggplot2::ggplot(ggplot2::aes(date, cases)) +
            ggplot2::geom_col(width = 1, colour = "white") +
            ggplot2::theme_bw() +
            ggplot2::xlab("")

    yearweek_monday_breaks <- yearweek_monday +
        scale_x_grates_yearweek(breaks = yearweek(2015, c(3, 13), 1), firstday = 1)

    yearweek_thursday <-
        dat |>
        dplyr::mutate(date = as_yearweek(date_of_infection, firstday = 4L)) |>
        dplyr::count(date, name = "cases") |>
        na.omit() |>
        ggplot2::ggplot(ggplot2::aes(date, cases)) +
        ggplot2::geom_col(width = 1, colour = "white") +
        ggplot2::theme_bw() +
        ggplot2::xlab("")

    yearweek_thursday_breaks <- yearweek_thursday +
        scale_x_grates_yearweek_thursday(breaks = yearweek(2014, c(25,35,45), 4))

    expect_snapshot_plot("yearweek_monday", yearweek_monday)
    expect_snapshot_plot("yearweek_monday_breaks", yearweek_monday_breaks)
    expect_snapshot_plot("yearweek_thursday", yearweek_thursday)
    expect_snapshot_plot("yearweek_thursday_breaks", yearweek_thursday_breaks)
})


test_that("isoweek plotting works", {
    skip_if_not_installed("outbreaks")
    skip_if_not_installed("dplyr")
    skip_if_not_installed("ggplot2", "2.0.0")
    dat <- outbreaks::ebola_sim_clean$linelist

    isoweek <-
        dat |>
        dplyr::mutate(date = as_isoweek(date_of_infection)) |>
        dplyr::count(date, name = "cases") |>
        na.omit() |>
        ggplot2::ggplot(ggplot2::aes(date, cases)) +
            ggplot2::geom_col(width = 1, colour = "white") +
            ggplot2::theme_bw() +
            ggplot2::xlab("")

    isoweek_breaks <- isoweek +
        scale_x_grates_isoweek(breaks = isoweek(year = 2014, week = c(35, 45)))

    isoweek_breaks_dates <- isoweek +
        scale_x_grates_isoweek(
            breaks = isoweek(year = 2014:2015, week = c(35, 3)),
            format = "%Y-%m-%d"
        ) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(hjust = 1, angle = 45))


    expect_snapshot_plot("isoweek", isoweek)
    expect_snapshot_plot("isoweek_breaks", isoweek_breaks)
    expect_snapshot_plot("isoweek_breaks_dates", isoweek_breaks_dates)
})


test_that("epiweek plotting works", {
    skip_if_not_installed("outbreaks")
    skip_if_not_installed("dplyr")
    skip_if_not_installed("ggplot2", "2.0.0")
    dat <- outbreaks::ebola_sim_clean$linelist

    epiweek <-
        dat |>
        dplyr::mutate(date = as_epiweek(date_of_infection)) |>
        dplyr::count(date, name = "cases") |>
        na.omit() |>
        ggplot2::ggplot(ggplot2::aes(date, cases)) +
        ggplot2::geom_col(width = 1, colour = "white") +
        ggplot2::theme_bw() +
        ggplot2::xlab("")

    epiweek_breaks <- epiweek +
        scale_x_grates_epiweek(breaks = epiweek(year = 2014, week = c(35, 45)))

    epiweek_breaks_dates <- epiweek +
        scale_x_grates_epiweek(
            breaks = epiweek(year = 2014:2015, week = c(35, 3)),
            format = "%Y-%m-%d"
        ) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(hjust = 1, angle = 45))


    expect_snapshot_plot("epiweek", epiweek)
    expect_snapshot_plot("epiweek_breaks", epiweek_breaks)
    expect_snapshot_plot("epiweek_breaks_dates", epiweek_breaks_dates)
})

test_that("yearmonth plotting works", {
    skip_if_not_installed("outbreaks")
    skip_if_not_installed("dplyr")
    skip_if_not_installed("ggplot2", "2.0.0")
    dat <- outbreaks::ebola_sim_clean$linelist

    month_dat <-
        dat |>
        dplyr::mutate(date = as_yearmonth(date_of_infection)) |>
        dplyr::count(date, name = "cases") |>
        na.omit()

    month <-
        month_dat |>
        ggplot2::ggplot(ggplot2::aes(date, cases)) +
            ggplot2::geom_col(width = 1, colour = "white") +
            scale_x_grates_yearmonth(n.breaks = 4) +
            ggplot2::theme_bw() +
            ggplot2::xlab("")

    month_breaks <- month +
        scale_x_grates_yearmonth(breaks = yearmonth(year = 2014, month = 3:12)) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(hjust = 1, angle = 45))

    month2 <- month +
        scale_x_grates_yearmonth(n.breaks = 4, format = "%Y-%m-%d")

    expect_snapshot_plot("yearmonth", month)
    expect_snapshot_plot("yearmonth_breaks", month_breaks)
    expect_snapshot_plot("yearmonth2", month2)
})

test_that("yearquarter plotting works", {
    skip_if_not_installed("outbreaks")
    skip_if_not_installed("dplyr")
    skip_if_not_installed("ggplot2", "2.0.0")
    dat <- outbreaks::ebola_sim_clean$linelist

    quarter_dat <-
        dat |>
        dplyr::mutate(date = as_yearquarter(date_of_infection)) |>
        dplyr::count(date, name = "cases") |>
        na.omit()

    quarter <-
        ggplot2::ggplot(quarter_dat, ggplot2::aes(date, cases)) +
            ggplot2::geom_col(width = 1, colour = "white") +
            scale_x_grates_yearquarter(n.breaks = 8) +
            ggplot2::theme_bw() +
            ggplot2::xlab("")

    quarter2 <-
        ggplot2::ggplot(quarter_dat, ggplot2::aes(date, cases)) +
        ggplot2::geom_col(width = 1, colour = "white") +
        scale_x_grates_yearquarter(n.breaks = 8, format = "%Y-%m-%d") +
        ggplot2::theme_bw() +
        ggplot2::xlab("")

    expect_snapshot_plot("yearquarter", quarter)
    expect_snapshot_plot("yearquarter2", quarter2)
})


test_that("year plotting works", {
    skip_if_not_installed("outbreaks")
    skip_if_not_installed("dplyr")
    skip_if_not_installed("ggplot2", "2.0.0")
    dat <- outbreaks::ebola_sim_clean$linelist

    year_dat <-
        dat |>
        dplyr::mutate(date = as_year(date_of_infection)) |>
        dplyr::count(date, name = "cases") |>
        na.omit()

    year <-
        ggplot2::ggplot(year_dat, ggplot2::aes(date, cases)) +
        ggplot2::geom_col(width = 1, colour = "white") +
        scale_x_grates_year(n.breaks = 2) +
        ggplot2::theme_bw() +
        ggplot2::xlab("")

    year2 <-
        ggplot2::ggplot(year_dat, ggplot2::aes(date, cases)) +
        ggplot2::geom_col(width = 1, colour = "white") +
        scale_x_grates_year(n.breaks = 2, format = "%Y-%m-%d") +
        ggplot2::theme_bw() +
        ggplot2::xlab("")

    expect_snapshot_plot("year", year)
    expect_snapshot_plot("year2", year2)
})

test_that("month plotting works", {
    skip_if_not_installed("outbreaks")
    skip_if_not_installed("dplyr")
    skip_if_not_installed("ggplot2", "2.0.0")
    dat <- outbreaks::ebola_sim_clean$linelist

    month_dat <-
        dat |>
        dplyr::mutate(date = as_month(date_of_infection, n = 2L)) |>
        dplyr::count(date, name = "cases") |>
        na.omit()

    month <-
        month_dat |>
        ggplot2::ggplot(ggplot2::aes(date, cases)) +
        ggplot2::geom_col(width = 1, colour = "white") +
        scale_x_grates_month(n.breaks = 4, n = 2) +
        ggplot2::theme_bw() +
        ggplot2::xlab("")

    month_breaks <- month +
        scale_x_grates_month(breaks = as_month("2014-05-01", n = 2) + 0:5 , n = 2) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(hjust = 1, angle = 45))

    month2 <-
        month_dat |>
        ggplot2::ggplot(ggplot2::aes(date, cases)) +
        ggplot2::geom_col(width = 1, colour = "white") +
        ggplot2::theme_bw() +
        ggplot2::xlab("") +
        scale_x_grates_month(n.breaks = 4, n = 2, format = NULL) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(hjust = 1, angle = 45))


    expect_snapshot_plot("month", month)
    expect_snapshot_plot("month_breaks", month_breaks)
    expect_snapshot_plot("month2", month2)
})

test_that("period plotting works", {
    skip_if_not_installed("outbreaks")
    skip_if_not_installed("dplyr")
    skip_if_not_installed("ggplot2", "2.0.0")
    dat <- outbreaks::ebola_sim_clean$linelist

    two_weeks <-
        dat |>
        dplyr::mutate(date = as_period(date_of_infection, n = 14)) |>
        dplyr::count(date, name = "cases") |>
        na.omit() |>
        ggplot2::ggplot(ggplot2::aes(date, cases)) +
            ggplot2::geom_col(width = 1L, colour = "white") +
            ggplot2::theme_bw() +
            ggplot2::xlab("")

    expect_snapshot_plot("two_weeks", two_weeks)

    twentyeight_days <-
        dat |>
        dplyr::mutate(date = as_period(date_of_infection, n = 28)) |>
        dplyr::count(date, name = "cases") |>
        na.omit() |>
        ggplot2::ggplot(ggplot2::aes(date, cases)) +
            ggplot2::geom_col(width = 1L, colour = "white") +
            ggplot2::theme_bw() +
            ggplot2::xlab("") +
            scale_x_grates_period(n.breaks = 7L, n = 28, offset = 0L) +
            ggplot2::theme(axis.text.x = ggplot2::element_text(hjust = 1, angle = 45))

    expect_snapshot_plot("twentyeight_days", twentyeight_days)


})
