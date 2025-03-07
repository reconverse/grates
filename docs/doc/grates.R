litedown::reactor(error = TRUE, message = TRUE, print = NA, fig.height = 5)

library(grates)
library(outbreaks)
library(ggplot2)

# Pull out the date of infection
x <- ebola_sim_clean$linelist$date_of_infection

# Calculate the daily incidence totals (ignoring missing values)
daily <- aggregate(list(cases = x), by = list(date = x), FUN = length)

# Add explicit zeros for days which aren't present
range <- seq.Date(min(daily$date), max(daily$date), by  = "day")
daily <- merge(data.frame(date = range), daily, by = "date", all.x = TRUE)
daily <- within(daily, cases[is.na(cases)] <- 0)

# plot the resulting output
ggplot(daily, aes(date, cases)) + geom_col(width = 1) + theme_bw()

# calculate the total number for across each week
week_dat <- with(daily,
    aggregate(
        list(cases = cases),
        by = list(week = as_isoweek(date)),
        FUN = sum
    )
)

head(week_dat)

# plot the output
(week_plot <-
    ggplot(week_dat, aes(week, cases)) +
    geom_col(width = 1, colour = "white") +
    theme_bw())

week_plot + scale_x_grates_epiweek(format = "%Y-%m-%d")

period_dat <- with(daily,
    aggregate(
        list(cases = cases),
        by = list(period = as_period(date, n = 14, offset = min(date))),
        FUN = sum
    )
)

head(period_dat)

ggplot(period_dat, aes(period, cases)) +
    geom_col(width = 1, colour = "white") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab("")

(month_dat <- with(daily,
    aggregate(
        list(cases = cases),
        by = list(month = as_yearmonth(date)),
        FUN = sum
    )
))

(month_plot <-
    ggplot(month_dat, aes(month, cases)) +
    geom_col(width = 1, colour = "white") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab(""))

month_plot + scale_x_grates_yearmonth(format = "%Y-%m-%d")

(quarter_dat <- with(daily,
    aggregate(
        list(cases = cases),
        by = list(quarter = as_yearquarter(date)),
        FUN = sum
    )
))

ggplot(quarter_dat, aes(quarter, cases)) +
    geom_col(width = 1, colour = "white") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab("")

(year_dat <- with(daily,
    aggregate(
        list(cases = cases),
        by = list(year = as_year(date)),
        FUN = sum
    )
))

ggplot(year_dat, aes(year, cases)) +
    geom_col(width = 1, colour = "white") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab("")

# calculate the bimonthly number of cases
(bimonth_dat <- with(daily,
    aggregate(
        list(cases = cases),
        by = list(group = as_month(date, n = 2)),
        FUN = sum
    )
))

# by default lower date bounds are used for the x axis
(bimonth_plot <-
    ggplot(bimonth_dat, aes(group, cases)) +
    geom_col(width = 1, colour = "white") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab(""))

bimonth_plot + scale_x_grates_month(format = NULL, n = 2L)

# Choose some dates spread across a few weeks
first <- as.Date("2024-12-18")
dates <- seq.Date(from = first, by = "5 days", length.out = 7)

# add the corresponding ISO week (see later)
dat <- data.frame(date = dates, isoweek = as_isoweek(dates))

with(dat, {
    weeks <- unique(isoweek)
    data.frame(
        isoweek = weeks,
        start = date_start(weeks),
        end = date_end(weeks)
    )
})

with(dat, identical(as.Date(isoweek), date_start(isoweek)))

with(dat, {
    data.frame(
        original_date = date,
        isoweek,
        contains.2025.01.10 = as.Date("2025-01-10") %during% isoweek
    )
})

weeks <- dat$isoweek

(minw <- min(weeks))
(maxw <- max(weeks))
(rangew <- range(weeks))

# seq method works if both `from` and `to` are epiweeks
seq(from = minw, to = maxw, by = 6L)

# but will error informatively if `to` is a different class
seq(from = minw, to = 999, by = 6L)

(dat <- transform(dat, plus4 = isoweek + 4L, minus4 = isoweek - 4L))

transform(dat, willerror = isoweek + isoweek)

transform(dat, difference = plus4 - minus4)

c(minw, maxw)
identical(c(minw, maxw), rangew)

c(minw, 1L)

