% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_file.R
\name{write_file}
\alias{write_file}
\title{Context-sensitive file writer}
\usage{
write_file(df, filename, overwrite = TRUE, compress_wait = FALSE,
  compress_threads = 0)
}
\arguments{
\item{df}{Data frame or tibble that will be written to the output file}

\item{filename}{Fully qualified path and filename that the data will be
written to. This can simply be a filename if used in working directory
or part of file.path() call.}

\item{overwrite}{Boolean variable specifying whether the target file 
(specified in filename) should be overwritten if it already exists. The
default is TRUE.}

\item{compress_wait}{Boolean variable specifying whether write_file should
wait for external compression to be completed before returning control to
this function. Applicable only if compression is implied by the filename
extension, defaults to FALSE (i.e., the system allow this function to 
proceed while compression continues in the background).}

\item{compress_threads}{Integer number of threads that xz compression will
use. Only applicable to xz compression, and defaults to zero, which 
uses all available cores on the computer.}
}
\description{
Context-sensitive file writer
}
\details{
This function is a general purpose file writer for saving data
  frame to files in feather (binary) or comma-separated value (CSV) formats.
  The file extension is parsed to determine the type of file to be written,
  whether compressed or not, and type of compression desired. In its present
  form the program used maximum compression available, and is limited to
  gzip and xz compression utilities. Some parameters are provided to fine-
  tune control of compression, although the user should consider their 
  effect on runtime performance, especially if xz compression with all 
  cores is used.
}
\examples{
write_file(trip_list, "./daily_trips.feather", overwrite = TRUE)
write_file(trip_list, "./daily_trips.csv.gz", compress_wait = TRUE)
}
