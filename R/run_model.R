run_model <- function() {
  # Template for running pcvmodr
  library(pcvmodr); library(tidyverse); library(doParallel)

  # Read the runtime parameters
  RTP <<- pcvmodr::get_runtime_parameters("./prelim_parameters.txt")

  # Start the doParallel cluster
  myCluster <- parallel::makeCluster(parallel::detectCores(),
    outfile = file.path(RTP[["scenario_folder"]], "myCluster.log"))
  doParallel::registerDoParallel(myCluster)
  print(paste("doParallel cluster instance started with", getDoParWorkers(),
    "cores"), quote = FALSE)

  # Read the FAF v4.x database for the given year
  fhwa_database <- pcvmodr::read_file(RTP[["faf_regional_database"]],
    format = "CSV")
  all_faf_flows <- pcvmodr::preprocess_faf4_database(fhwa_database, 2013)

  # Keep only those FAF flows that touch Idaho or one of its bordering states
  # (we can do this because it is close to edge of the continent. We'd never be
  # able to do this with Nebraska, for example)
  idaho <- 160
  nevada <- c(321, 329)
  oregon <- c(411, 419)
  utah <- c(491, 499)
  washington <- c(531, 532, 539)
  wyoming <- 560
  modeled_area <- c(idaho, nevada, oregon, utah, washington, wyoming)
  faf_flows <- dplyr::filter(all_faf_flows, dms_orig %in% modeled_area |
      dms_dest %in% modeled_area)
  print(paste(nrow(faf_flows), "FAF flows records affecting ID retained"),
    quote = FALSE)

  # Create annual and daily FAF truckload equivalents
  truckloads <- pcvmodr::create_faf4_annual_truckloads(faf_flows)
  daily_trucks <- pcvmodr::sample_faf4_daily_trucks(truckloads)
  filename <- file.path(RTP[["scenario_folder"]], "daily_truckloads.feather")
  pcvmodr::write_file(daily_trucks, filename)

  # When we're done shut down the doParallel cluster
  stopCluster(myCluster)
}
