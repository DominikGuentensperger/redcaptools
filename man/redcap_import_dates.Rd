% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/redcap_import_dates_times.R
\name{redcap_import_dates}
\alias{redcap_import_dates}
\title{REDCap Date Conversion}
\usage{
redcap_import_dates(var, unk_day = "01", unk_month = "01", format = "european")
}
\arguments{
\item{var}{variable to convert}

\item{unk_day}{Day to use if unknown, i.e. if only the year or only the month
+ year is found. The default is 01 (2022 -> 2022-01-01).}

\item{unk_month}{Month to use if unknown, i.e. if only the year is found. The
default is 01 (2022 -> 2022-01-01).}

\item{format}{Date format to be used: "European" (DMY) or "American" (MDY).
Dates that match both formats will be converted accordingly.}
}
\value{
converted variable
}
\description{
This function is used to prepare dates in a data table for import in REDCap.
In an Excel, dates can be entered in various shapes and forms. This function
tries to take most possibilities how dates could have been entered into
account and converts them to a format that can be imported into REDCap.
}
\examples{
var <-c("01.12.2022", "12.2022", "2022", "01/12/2022")
redcap_import_dates(var)
}
