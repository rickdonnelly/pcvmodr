% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qrfm2_truck_generation.R
\name{qrfm2_truck_generation}
\alias{qrfm2_truck_generation}
\title{Quick Response Freight Manual 2nd Edition (QRFM2) truck generation}
\usage{
qrfm2_truck_generation(synthetic_firms, generation_rates)
}
\arguments{
\item{synthetic_firms}{Data frame containing data for every synthetic firm
in the simulation.}

\item{generation_rates}{Data frame containing QRFM2 truck trip generation
rates by truck type and firm sector.}
}
\description{
Quick Response Freight Manual 2nd Edition (QRFM2) truck generation
}
\details{
This function calculates discrete truck trips by truck type and
  sector for every synthetic firm in the simulation. The QRFM2 trip rates are
  used to generate total aggregate trips, which are allocated to discrete
  synthetic firms. A tibble containing trip records is returned by this
  program.
}
\examples{
trip_origins <- qfrm2_truck_generation(synthetic_firms, gen_rates)
}
