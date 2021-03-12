# nocov start
.onLoad <- function(...) {
  s3_register("ggplot2::scale_type", "yrwk")
  s3_register("ggplot2::scale_type", "yrmon")
  # s3_register("ggplot2::scale_type", "yrqtr")
  # s3_register("ggplot2::scale_type", "yr")
  # s3_register("ggplot2::scale_type", "period")
  invisible()
}
# nocov end
