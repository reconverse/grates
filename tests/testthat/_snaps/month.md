# formatting

    Code
      print(month(dat))
    Output
      <grates_month[4]>
      [1] 1970-Jan 1970-Feb 1970-Mar 1970-Apr

# pre-epoch dates work

    Code
      as_month(dates)
    Output
      <grates_month[3]>
      [1] 1900-Feb 1900-Jul 1900-Dec

# as_month.Date works correctly - n = 1

    Code
      print(res)
    Output
          Group.1  x
      1  2020-Jan 31
      2  2020-Feb 29
      3  2020-Mar 31
      4  2020-Apr 30
      5  2020-May 31
      6  2020-Jun 30
      7  2020-Jul 31
      8  2020-Aug 31
      9  2020-Sep 30
      10 2020-Oct 31
      11 2020-Nov 30
      12 2020-Dec 31

---

    Code
      print(unique(res$Group.1))
    Output
      <grates_month[12]>
       [1] 2020-Jan 2020-Feb 2020-Mar 2020-Apr 2020-May 2020-Jun 2020-Jul 2020-Aug
       [9] 2020-Sep 2020-Oct 2020-Nov 2020-Dec

# as_month.Date works correctly - n > 1

    Code
      print(as_month(dates, n = 2))
    Output
      <grates_month[24]>
       [1] 2021-Jan to 2021-Feb 2021-Jan to 2021-Feb 2021-Mar to 2021-Apr
       [4] 2021-Mar to 2021-Apr 2021-May to 2021-Jun 2021-May to 2021-Jun
       [7] 2021-Jul to 2021-Aug 2021-Jul to 2021-Aug 2021-Sep to 2021-Oct
      [10] 2021-Sep to 2021-Oct 2021-Nov to 2021-Dec 2021-Nov to 2021-Dec
      [13] 2022-Jan to 2022-Feb 2022-Jan to 2022-Feb 2022-Mar to 2022-Apr
      [16] 2022-Mar to 2022-Apr 2022-May to 2022-Jun 2022-May to 2022-Jun
      [19] 2022-Jul to 2022-Aug 2022-Jul to 2022-Aug 2022-Sep to 2022-Oct
      [22] 2022-Sep to 2022-Oct 2022-Nov to 2022-Dec 2022-Nov to 2022-Dec

