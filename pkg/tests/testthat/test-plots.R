count_no_na <- function(x) {
    dat <- split(x, factor(x))
    cases <- lengths(dat)
    date <- lapply(dat, `[`, 1L)
    date <- do.call(c, date)
    data.frame(date, cases)
}


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
    skip_if_not_installed("ggplot2", "2.0.0")
    dat <- outbreaks::ebola_sim_clean$linelist

    yearweek_monday <-
        as_yearweek(dat$date_of_infection, firstday = 1L) |>
        count_no_na() |>
        ggplot2::ggplot(ggplot2::aes(date, cases)) +
            ggplot2::geom_col(width = 1, colour = "white") +
            ggplot2::theme_bw() +
            ggplot2::xlab("")

    yearweek_monday_breaks <- yearweek_monday +
        scale_x_grates_yearweek(breaks = yearweek(2015, c(3, 13), 1), firstday = 1)

    yearweek_thursday <-
        as_yearweek(dat$date_of_infection, firstday = 4L) |>
        count_no_na() |>
        ggplot2::ggplot(ggplot2::aes(date, cases)) +
        ggplot2::geom_col(width = 1, colour = "white") +
        ggplot2::theme_bw() +
        ggplot2::xlab("")

    yearweek_thursday_breaks <- yearweek_thursday +
        scale_x_grates_yearweek_thursday(breaks = yearweek(2014, c(25, 35, 45), 4))

    yearweek_thursday_weeks_only <- yearweek_thursday +
        scale_x_grates_yearweek_thursday(format = "week")

    expect_snapshot_plot("yearweek_monday", yearweek_monday)
    expect_snapshot_plot("yearweek_monday_breaks", yearweek_monday_breaks)
    expect_snapshot_plot("yearweek_thursday", yearweek_thursday)
    expect_snapshot_plot("yearweek_thursday_breaks", yearweek_thursday_breaks)
    expect_snapshot_plot("yearweek_thursday_weeks_only", yearweek_thursday_weeks_only)
})


test_that("isoweek plotting works", {
    skip_if_not_installed("outbreaks")
    skip_if_not_installed("ggplot2", "2.0.0")
    dat <- outbreaks::ebola_sim_clean$linelist

    isoweek <-
        as_isoweek(dat$date_of_infection) |>
        count_no_na() |>
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

    isoweek_weeks <- isoweek + scale_x_grates_isoweek(format = "week")

    expect_snapshot_plot("isoweek", isoweek)
    expect_snapshot_plot("isoweek_breaks", isoweek_breaks)
    expect_snapshot_plot("isoweek_breaks_dates", isoweek_breaks_dates)
    expect_snapshot_plot("isoweek_weeks", isoweek_weeks)
})


test_that("epiweek plotting works", {
    skip_if_not_installed("outbreaks")
    skip_if_not_installed("ggplot2", "2.0.0")
    dat <- outbreaks::ebola_sim_clean$linelist

    epiweek <-
        as_epiweek(dat$date_of_infection) |>
        count_no_na() |>
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

    epiweek_weeks <- epiweek + scale_x_grates_epiweek(format = "week")


    expect_snapshot_plot("epiweek", epiweek)
    expect_snapshot_plot("epiweek_breaks", epiweek_breaks)
    expect_snapshot_plot("epiweek_breaks_dates", epiweek_breaks_dates)
    expect_snapshot_plot("epiweek_weeks", epiweek_weeks)
})

test_that("yearmonth plotting works", {
    skip_if_not_installed("outbreaks")
    skip_if_not_installed("ggplot2", "2.0.0")
    dat <- outbreaks::ebola_sim_clean$linelist

    month_dat <-
        as_yearmonth(dat$date_of_infection) |>
        count_no_na()

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
    skip_if_not_installed("ggplot2", "2.0.0")
    dat <- outbreaks::ebola_sim_clean$linelist

    quarter_dat <-
        as_yearquarter(dat$date_of_infection) |>
        count_no_na()

    quarter <-
        ggplot2::ggplot(quarter_dat, ggplot2::aes(date, cases)) +
            ggplot2::geom_col(width = 1, colour = "white") +
            scale_x_grates_yearquarter(n.breaks = 8) +
            ggplot2::theme_bw() +
            ggplot2::xlab("")

    quarter_breaks <- quarter +
        scale_x_grates_yearquarter(breaks = yearquarter(2014, 1) + 0:4)

    quarter2 <-
        ggplot2::ggplot(quarter_dat, ggplot2::aes(date, cases)) +
        ggplot2::geom_col(width = 1, colour = "white") +
        scale_x_grates_yearquarter(n.breaks = 8, format = "%Y-%m-%d") +
        ggplot2::theme_bw() +
        ggplot2::xlab("")

    quarter2_breaks <- quarter2 +
        scale_x_grates_yearquarter(breaks = yearquarter(2014, 1) + 0:5, format = "%Y-%m-%d")

    expect_snapshot_plot("yearquarter", quarter)
    expect_snapshot_plot("yearquarter_breaks", quarter_breaks)
    expect_snapshot_plot("yearquarter2", quarter2)
    expect_snapshot_plot("yearquarter2_breaks", quarter2_breaks)
})


test_that("year plotting works", {
    skip_if_not_installed("outbreaks")
    skip_if_not_installed("ggplot2", "2.0.0")
    dat <- outbreaks::ebola_sim_clean$linelist

    year_dat <-
        as_year(dat$date_of_infection) |>
        count_no_na()

    year <-
        ggplot2::ggplot(year_dat, ggplot2::aes(date, cases)) +
        ggplot2::geom_col(width = 1, colour = "white") +
        scale_x_grates_year(n.breaks = 2) +
        ggplot2::theme_bw() +
        ggplot2::xlab("")

    year_breaks <- year +
        scale_x_grates_year(breaks = year(2014))

    year2 <-
        ggplot2::ggplot(year_dat, ggplot2::aes(date, cases)) +
        ggplot2::geom_col(width = 1, colour = "white") +
        scale_x_grates_year(n.breaks = 2, format = "%Y-%m-%d") +
        ggplot2::theme_bw() +
        ggplot2::xlab("")

    year2_breaks <- year +
        scale_x_grates_year(breaks = year(2014:2016), format = "%Y-%m-%d")

    expect_snapshot_plot("year", year)
    expect_snapshot_plot("year_breaks", year_breaks)
    expect_snapshot_plot("year2", year2)
    expect_snapshot_plot("year2_breaks", year2_breaks)
})

test_that("month plotting works", {
    skip_if_not_installed("outbreaks")
    skip_if_not_installed("ggplot2", "2.0.0")
    dat <- outbreaks::ebola_sim_clean$linelist

    month_dat <-
        as_month(dat$date_of_infection, n = 2L) |>
        count_no_na()

    month <-
        month_dat |>
        ggplot2::ggplot(ggplot2::aes(date, cases)) +
        ggplot2::geom_col(width = 1, colour = "white") +
        scale_x_grates_month(n.breaks = 4, n = 2) +
        ggplot2::theme_bw() +
        ggplot2::xlab("")

    month_breaks <- month +
        scale_x_grates_month(breaks = as_month("2014-05-01", n = 2) + 0:5, n = 2) +
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
    skip_if_not_installed("ggplot2", "2.0.0")
    dat <- outbreaks::ebola_sim_clean$linelist

    two_weeks <-
        as_period(dat$date_of_infection, n = 14) |>
        count_no_na() |>
        ggplot2::ggplot(ggplot2::aes(date, cases)) +
        ggplot2::geom_col(width = 1L, colour = "white") +
        ggplot2::theme_bw() +
        ggplot2::xlab("")

    br <- as_period(c("2014-08-28", "2015-01-15"), n = 14)
    two_weeks_breaks <- two_weeks +
        scale_x_grates_period(breaks = br, n = 14, offset = 0)

    expect_snapshot_plot("two_weeks", two_weeks)
    expect_snapshot_plot("two_weeks_breaks", two_weeks_breaks)

    twentyeight_days <-
        as_period(dat$date_of_infection, n = 28) |>
        count_no_na() |>
        ggplot2::ggplot(ggplot2::aes(date, cases)) +
        ggplot2::geom_col(width = 1L, colour = "white") +
        ggplot2::theme_bw() +
        ggplot2::xlab("") +
        scale_x_grates_period(n.breaks = 7L, n = 28, offset = 0L) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(hjust = 1, angle = 45))

    br <- as_period("2014-06-19", n = 28) + c(0, 2, 4)
    twentyeight_days_breaks <- twentyeight_days +
        scale_x_grates_period(breaks = br, n = 28, offset = 0) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(hjust = 1, angle = 45))

    expect_snapshot_plot("twentyeight_days", twentyeight_days)
    expect_snapshot_plot("twentyeight_days_breaks", twentyeight_days_breaks)
})

test_that("int_period plotting works", {

    skip_if_not_installed("ggplot2", "2.0.0")

    x <- c(1, 3, 4, 4, 3, 4, 10)
    one_dat <- count_no_na(as_int_period(x))
    two_dat <- count_no_na(as_int_period(x, n = 2L))

    p_one <-
        one_dat |>
        ggplot2::ggplot(ggplot2::aes(date, cases)) +
        ggplot2::geom_col(width = 1, colour = "white") +
        scale_x_grates_int_period(n.breaks = 10, n = 1) +
        ggplot2::theme_bw() +
        ggplot2::xlab("")


    p_two <-
        two_dat |>
        ggplot2::ggplot(ggplot2::aes(date, cases)) +
        ggplot2::geom_col(width = 1, colour = "white") +
        scale_x_grates_int_period(n.breaks = 4, n = 2) +
        ggplot2::theme_bw() +
        ggplot2::xlab("")

    p_breaks <-
        p_two +
        scale_x_grates_int_period(
            breaks = as_int_period(seq(0, 12, by = 2), n = 2),
            n = 2
        )

    p_breaks_centred <-
        p_two +
        scale_x_grates_int_period(
            breaks = as_int_period(seq(0, 12, by = 2), n = 2),
            n = 2,
            centre = TRUE
        )

    expect_snapshot_plot("p_one", p_one)
    expect_snapshot_plot("p_two", p_two)
    expect_snapshot_plot("p_breaks", p_breaks)
    expect_snapshot_plot("p_breaks_centred", p_breaks_centred)
})
