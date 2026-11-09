atime::atime_versions(
    "~/git/grates/newfymd/pkg",
    N <- 2^(0:15),
    sha.vec = c(
        current = "9d5c9e3",
        old = "b06a8f2"
    ),
    setup <- {
        set.seed(1)
        x <- Sys.Date() + (-365*1000):(365*1000)
        x <- grates::get_week(grates::as_isoweek(sample(x, N, replace = TRUE)))
    },
    expr = grates::get_week(x)
)

x <- Sys.Date() + (-365*1000):(365*1000)
xx <- sample(x, 10e6, replace = TRUE)
y <- grates::get_week(grates::as_isoweek(xx))

microbenchmark::microbenchmark(
    fastymd::get_isoweek(xx),
    data.table::isoweek(xx),
    grates::get_week(grates::as_isoweek(xx)),
    times = 20
)
gra
