# formatting

    Code
      print(quarter(dat))
    Output
      <grates_quarter[4]>
      [1] 1970-Q1 1970-Q2 1970-Q3 1970-Q4

# pre-epoch dates work

    Code
      as_quarter(dates)
    Output
      <grates_quarter[4]>
      [1] 1900-Q1 1900-Q2 1900-Q3 1900-Q4

# as_quarter.Date works correctly

    Code
      print(res)
    Output
        Group.1  x
      1 2020-Q1 91
      2 2020-Q2 91
      3 2020-Q3 92
      4 2020-Q4 92

