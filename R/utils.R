#' # check if interval valid
#' is_valid_interval <- function(x) {
#'   if (is.character(x)) {
#'     pattern <- "^\\d?\\s?(day|week|month|quarter|year)s?$"
#'     return(grepl(pattern, x, ignore.case = TRUE))
#'   } else if (is.numeric(x)) {
#'     if (!all(is.wholenumber(x) | is.na(x))) {
#'       return(FALSE)
#'     } else {
#'       return(TRUE)
#'     }
#'   }
#'   FALSE
#' }
#'

#'

#'
#'
#'
#' # The following is based on the approach Davis Vaughan took in
#' # https://github.com/DavisVaughan/datea/blob/master/src/month.c but in R and
#' # extended to other cases.
#'
#' # Constants ---------------------------------------------------------------
#'

#'
#'
#' delayedAssign(
#'   "QUARTER_DAYS_IN_MONTH_BEFORE",
#'   c(0L, 31L, 59L, 0L, 30L, 61L, 0L, 31L, 62L, 0L, 31L, 61L)
#' )
#'

#'
#'

#'
#' # other useful conversions ------------------------------------------------
#'
#'

#'

#'

#'
#' quarter_days_before_month <- function(year, month) {
#'   QUARTER_DAYS_IN_MONTH_BEFORE[month] + ((month == 3) & is_leap_year(year))
#' }
#'

#'
#'
#' date_to_month <- function(x) {
#'   x <- as_utc_posixlt_from_int(x)
#'   yr <- x$year + 1900L
#'   mon <- x$mon
#'   mon <- (yr - 1970L) * 12L + mon
#'   mon
#' }
#'

#'
#'
#'
