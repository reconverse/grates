# grates 0.3.1

* Fix for changes made to POSIXlt objects in R-devel.

# grates 0.3.0

## New functions

* seq methods now implemented for all grates objects.

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

* Fixed bug affecting scale_x_period

# grates 0.1.1

* Initial release
