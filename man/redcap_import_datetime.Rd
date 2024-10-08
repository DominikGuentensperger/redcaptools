% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/redcap_import_dates_times.R
\name{redcap_import_datetime}
\alias{redcap_import_datetime}
\title{REDCap Date-Time Conversion}
\usage{
redcap_import_datetime(var, args_rc_dates = list(), args_rc_times = list())
}
\arguments{
\item{var}{variable to convert}
}
\value{
converted variable
}
\description{
This function is used to prepare date-time-values in a data table for import
in REDCap by parsing date and time values and running \code{redcap_import_dates} and
\code{redcap_import_times}
}
\examples{
var <-c("11:11:00","11:11","11")
redcap_import_times(var)

}
