opts = litedown::reactor()
opts$error = TRUE
opts$message = TRUE
opts$print = function(x, ...) capture.output(if (isS4(x)) methods::show(x, ...) else print(x, ...))
opts$fig.align = "center"
opts$fig.width = 7
opts$fig.height = 5

library(grates)

# Choose some consecutive dates that begin on a Friday
first <- as.Date("2021-01-01")
weekdays(first)
dates <- first + 0:9

# Below we use a Friday-week grouping
weeks <- as_yearweek(dates, firstday = 5L)
(dat <- data.frame(dates, weeks))

# we can also use the constructor function if we already have weeks and years
yearweek(year = c(2020L, 2021L), week = c(1L, 10L), firstday = 5L)

# epiweeks always start on a Sunday
(epiwk <- as_epiweek(Sys.Date()))

weekdays(as.Date(epiwk))

# isoweeks always start on a Sunday
(isowk <- as_isoweek(Sys.Date()))

weekdays(as.Date(isowk))

library(ggplot2)

# use simulated linelist data from the outbreaks package
dat <- outbreaks::ebola_sim_clean
dat <- dat$linelist$date_of_infection

# calculate the total number for across each week
week_dat <- aggregate(
    list(cases = dat),
    by = list(week = as_epiweek(dat)),
    FUN = length
)

head(week_dat)

# plot the output
(week_plot <-
    ggplot(week_dat, aes(week, cases)) +
    geom_col(width = 1, colour = "white") +
    theme_bw())

week_plot + scale_x_grates_epiweek(format = "%Y-%m-%d")

# calculate the total number for across 14 day periods with no offset.
# note - 0L is the default value for the offset but we specify it explicitly
# here for added clarity
period_dat <- aggregate(
    list(cases = dat),
    by = list(period = as_period(dat, n = 14L, offset = 0L)),
    FUN = length
)

head(period_dat)

ggplot(period_dat, aes(period, cases)) +
    geom_col(width = 1, colour = "white") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab("")

dates <- as.Date("2020-01-03") + 0:9
offset <- as.Date("2020-01-01")
data.frame(dates, period = as_period(dates, n = 7L, offset = offset))

(month_dat <- aggregate(
    list(cases = dat),
    by = list(month = as_yearmonth(dat)),
    FUN = length
))

(month_plot <-
    ggplot(month_dat, aes(month, cases)) +
    geom_col(width = 1, colour = "white") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab(""))

month_plot + scale_x_grates_yearmonth(format = "%Y-%m-%d")

(quarter_dat <- aggregate(
    list(cases = dat),
    by = list(quarter = as_yearquarter(dat)),
    FUN = length
))

ggplot(quarter_dat, aes(quarter, cases)) +
    geom_col(width = 1, colour = "white") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab("")

(year_dat <- aggregate(
    list(cases = dat),
    by = list(year = as_year(dat)),
    length
))

ggplot(year_dat, aes(year, cases)) +
    geom_col(width = 1, colour = "white") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab("")

# Construction functions can also be used
yearmonth(2022L, 11L)
yearquarter(2022L, 4L)
year(2022L)

# calculate the bimonthly number of cases
(bimonth_dat <- aggregate(
    list(cases = dat),
    by = list(group = as_month(dat, n = 2L)),
    FUN = length
))

# by default lower date bounds are used for the x axis
(bimonth_plot <-
    ggplot(bimonth_dat, aes(group, cases)) +
    geom_col(width = 1, colour = "white") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab(""))

bimonth_plot + scale_x_grates_month(format = NULL, n = 2L)

weeks <- week_dat$week

dat <- weeks[1:5]
data.frame(
    week = dat,
    start = date_start(dat),
    end = date_end(dat),
    contains.2014.04.14 = as.Date("2014-04-14") %during% dat
)

identical(as.Date(weeks), date_start(weeks))

# min, max and range
(minw <- min(weeks))
(maxw <- max(weeks))
(rangew <- range(weeks))

# seq method works if both `from` and `to` are epiweeks
seq(from = minw, to = maxw, by = 6L)

# but will error informatively if `to` is a different class
seq(from = minw, to = 999, by = 6L)

dat <- head(week_dat)
(dat <- transform(dat, plus4 = week + 4L, minus4 = week - 4L))

transform(dat, willerror = week + week)

transform(dat, difference = plus4 - minus4)

c(minw, maxw)
identical(c(minw, maxw), rangew)

c(minw, 1L)

