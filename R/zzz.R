# nocov start
.onLoad <- function(...) {
  s3_register("ggplot2::scale_type", "grates_month")
  s3_register("ggplot2::scale_type", "grates_quarter")
  s3_register("ggplot2::scale_type", "grates_yearweek")
  s3_register("ggplot2::scale_type", "grates_year")
  s3_register("ggplot2::scale_type", "grates_period")
  s3_register("ggplot2::scale_type", "grates_int_period")
  invisible()
}
# nocov end
