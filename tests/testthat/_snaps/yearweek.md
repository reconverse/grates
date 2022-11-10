# formatting

    Code
      print(yearweek(dat))
    Output
      <grates_yearweek[4]>
      [1] 1970-W01 1970-W02 1970-W03 1970-W04

# pre-epoch dates work

    Code
      as_yearweek(dates)
    Output
      <grates_yearweek[4]>
      [1] 1900-W01 1900-W02 1900-W03 1900-W04

# as_yearweek.POSIXct works as expected

    Code
      res
    Output
      [[1]]
      <grates_yearweek[1]>
      [1] 2021-W01
      
      [[2]]
      <grates_yearweek[1]>
      [1] 2021-W01
      
      [[3]]
      <grates_yearweek[1]>
      [1] 2021-W01
      
      [[4]]
      <grates_yearweek[1]>
      [1] 2021-W01
      
      [[5]]
      <grates_yearweek[1]>
      [1] 2021-W01
      
      [[6]]
      <grates_yearweek[1]>
      [1] 2021-W01
      
      [[7]]
      <grates_yearweek[1]>
      [1] 2021-W01
      

---

    Code
      res2
    Output
      [[1]]
      [1] "2021-01-04"
      
      [[2]]
      [1] "2020-12-29"
      
      [[3]]
      [1] "2020-12-30"
      
      [[4]]
      [1] "2020-12-31"
      
      [[5]]
      [1] "2021-01-01"
      
      [[6]]
      [1] "2021-01-02"
      
      [[7]]
      [1] "2021-01-03"
      

