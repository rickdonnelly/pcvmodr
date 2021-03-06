% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_file_attributes.R
\name{get_file_attributes}
\alias{get_file_attributes}
\title{Return a string containing file attributes}
\usage{
get_file_attributes(filename, records)
}
\arguments{
\item{filename}{Fully qualified path and filename}

\item{records}{The number of records in the file can be passed to the
function, which will echo this information}
}
\description{
Return a string containing file attributes
}
\details{
This function returns a string that includes the filename, date last
  modified, size, and number of records.
}
\examples{
annual_flows <- get_file_attributes("./flow_database.csv")
}
