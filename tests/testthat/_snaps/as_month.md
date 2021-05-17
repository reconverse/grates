# as_month.Date works correctly - interval = 1

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

    <grate_month>: interval = 1 month
     [1] "2020-01" "2020-02" "2020-03" "2020-04" "2020-05" "2020-06" "2020-07"
     [8] "2020-08" "2020-09" "2020-10" "2020-11" "2020-12"

# as_month.Date works correctly - interval > 1

    <grate_month>: interval = 2 months
     [1] "2021-Jan to 2021-Feb" "2021-Jan to 2021-Feb" "2021-Mar to 2021-Apr"
     [4] "2021-Mar to 2021-Apr" "2021-May to 2021-Jun" "2021-May to 2021-Jun"
     [7] "2021-Jul to 2021-Aug" "2021-Jul to 2021-Aug" "2021-Sep to 2021-Oct"
    [10] "2021-Sep to 2021-Oct" "2021-Nov to 2021-Dec" "2021-Nov to 2021-Dec"
    [13] "2022-Jan to 2022-Feb" "2022-Jan to 2022-Feb" "2022-Mar to 2022-Apr"
    [16] "2022-Mar to 2022-Apr" "2022-May to 2022-Jun" "2022-May to 2022-Jun"
    [19] "2022-Jul to 2022-Aug" "2022-Jul to 2022-Aug" "2022-Sep to 2022-Oct"
    [22] "2022-Sep to 2022-Oct" "2022-Nov to 2022-Dec" "2022-Nov to 2022-Dec"

