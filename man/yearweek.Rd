% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/yearweek.R
\name{yearweek}
\alias{yearweek}
\title{Constructor for yearweek objects}
\usage{
yearweek(year = integer(), week = integer(), firstday = 1L)
}
\arguments{
\item{year}{\verb{[integer]}

Vector representing the year associated with \code{week}.

\code{double} vectors will be converted via \code{as.integer(floor(x))}.}

\item{week}{\verb{[integer]}

Vector representing the week associated with `year.

\code{double} vectors will be converted via \code{as.integer(floor(x))}.}

\item{firstday}{\verb{[integer]}

The day the week starts on from 1 (Monday) to 7 (Sunday).}
}
\value{
A \verb{<grates_yearweek>} object with subclass corresponding to the first day of
the week they represent (e.g. \verb{<grates_yearweek_monday>}).
}
\description{
\code{yearweek()} is a constructor for \verb{<grates_yearweek>} objects. These are
weeks whose first day can be specified by the user.
}
\details{
For yearweek objects the first week of a "year" is considered to be the first
yearweek containing 4 days of the given calendar year. This means that the
calendar year will sometimes be different to that of the associated yearweek
object.
}
\note{
Internally \verb{<grates_yearweek>} objects are stored as the number of weeks
(starting at 0) from the date of the \code{firstday} nearest the Unix Epoch
(1970-01-01). That is, the number of seven day periods from:

\if{html}{\out{<div class="sourceCode">}}\preformatted{- 1969-12-29 for `firstday` equal to 1 (Monday)
- 1969-12-30 for `firstday` equal to 2 (Tuesday)
- 1969-12-31 for `firstday` equal to 3 (Wednesday)
- 1970-01-01 for `firstday` equal to 4 (Thursday)
- 1970-01-02 for `firstday` equal to 5 (Friday)
- 1970-01-03 for `firstday` equal to 6 (Saturday)
- 1970-01-04 for `firstday` equal to 7 (Sunday)
}\if{html}{\out{</div>}}
}
\examples{
yearweek(year = 2000L, week = 3L)

}
\seealso{
\code{as_yearweek()} and \code{new_yearweek()}.
}
