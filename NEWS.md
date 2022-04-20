# grates (development version)

For the first major release of grates a significant refactor of grates has been
undertaken that builds upon lessons learnt over the last two years. Whilst we
have tried to limit breaking changes some functionality has been removed and
some function parameters altered (see details below):

## breaking changes

* `<grates_month>` objects are now always stored relative to the UNIX epoch.
  This is equivalent to setting `origin = 0` in the previous release. Calling
  the function with an `origin` argument will now error.

* Trying to create a `<grates_month>` object with `n` set to 1 will now error.
  Uses are encouraged to use `<grates_yearmonth>` for this case.

* `<grates_int_period>` is now a defunct as it did not fit with the scope of the
  package (i.e grouped dates, not grouped integers). In particular the
  `int_period()`, `as_int_period()` and `is_int_period()` will now error on use.

* The `origin` parameter from `<grates_period>` as been renamed to `offset` to
  better reflect its usage. Users will need to update uses of `period()`,
  `as_period()` and `scale_x_grates_period()` to reflect this.

## new functions and classes

* A new `yearmonth` class (`<grates_yearmonth>`) and associated functions have
  been introduced. This object is similar to what was previously obtained via a
  call of `month(x, n = 1L, origin = 0L)` (now defunct - see above).

* New `isoweek` and `epiweek` classes (`<grates_isoweek>` and
  `<grates_epiweek>` respectively) and associated functions. Internally these
  are similar to the corresponding `<grates_yearweek>` objects but with a
  marginally more efficient implementation.

## bug fixes

* `is.numeric()` methods for grates objects previously returned FALSE. Calls to
  these methods now dispatch to the default implementation based on the
  underlying type and should now return TRUE.

## miscellaneous

* Hard dependencies on [clock](https://CRAN.R-project.org/package=clock) and
  [vctrs](https://CRAN.R-project.org/package=vctrs) have now been removed.

* The underlying test framework used is now
  [tinytest](https://CRAN.R-project.org/package=tinytest) as opposed to
  [testthat](https://CRAN.R-project.org/package=testthat).

# grates 0.3.1

* Fix for changes made to POSIXlt objects in R-devel.

# grates 0.3.0

## New functions

* `seq()` methods now implemented for all grates objects.

## bug fixes

* Conversion functions now preserve names.

* Bug fixes for cast functions operating on objects of the same class but
  with different attributes.

# grates 0.2.0

* This is a breaking release that changes the underlying implementations of the
  different grate constructors and associated scales for ggplot2. There has also
  been some renaming of function arguments to bring greater consistency across
  packages.

* We now make more use of the high level API introduced by the 
  [clock](https://CRAN.R-project.org/package=clock) package for working with
  R's date and date-time types.

# grates 0.1.2

## bug fixes

* Fixed bug affecting `scale_x_period()`

# grates 0.1.1

* Initial release
