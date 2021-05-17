# nocov start
.onLoad <- function(...) {
  s3_register("ggplot2::scale_type", "grate_month")
  s3_register("ggplot2::scale_type", "grate_quarter")
  s3_register("ggplot2::scale_type", "grate_yearweek")
  s3_register("ggplot2::scale_type", "grate_year")
  s3_register("ggplot2::scale_type", "grate_period")
  s3_register("ggplot2::scale_type", "grate_int_period")
  invisible()
}
# nocov end
