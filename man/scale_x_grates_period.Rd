% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scale_period.R
\name{scale_x_grates_period}
\alias{scale_x_grates_period}
\title{<grates_period> scale}
\usage{
scale_x_grates_period(n.breaks = 6, format = "\%Y-\%m-\%d", n, origin)
}
\arguments{
\item{n.breaks}{Approximate number of breaks calculated using
\code{scales::breaks_pretty} (default 6).}

\item{format}{Format to use for x scale. Passed to \code{\link[=format.Date]{format.Date()}}.}

\item{n}{Number of months used for the original grouping.}

\item{origin}{Month since the Unix epoch used in the original grouping.}
}
\value{
A scale for use with ggplot2.
}
\description{
ggplot2 scale for <grates_period> vector.
}
