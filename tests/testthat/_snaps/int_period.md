# formatting

    Code
      period(dat)
    Output
      <grates_period[4]>
      [1] 1970-01-01 1970-01-08 1970-01-15 1970-01-22

# pre-epoch dates work

    Code
      as_period(dates, n = 7, origin = as.integer(as.Date("1900-01-01")))
    Output
      <grates_period[4]>
      [1] 1900-01-01 to 1900-01-07 1900-01-08 to 1900-01-14 1900-01-15 to 1900-01-21
      [4] 1900-01-22 to 1900-01-28

# int_period output looks correct

    Code
      dat
    Output
      <grates_int_period[4]>
      [1] 1 to 2 1 to 2 3 to 4 3 to 4

---

    Code
      as.character(dat)
    Output
      [1] "1 to 2" "1 to 2" "3 to 4" "3 to 4"

