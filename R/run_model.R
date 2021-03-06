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
  faf_db <- pcvmodr::preprocess_faf4_database(fhwa_database, 2013)

  # Group FAF regions into states, which are easier to use
  arizona <- c(41, 42, 49)
  california <- c(61:65, 69)
  idaho <- 160
  las_vegas <- 321
  reno <- 329
  montana <- 300
  nevada <- c(las_vegas, reno)
  oregon <- c(411, 419)
  utah <- c(491, 499)
  washington <- c(531, 532, 539)

  # Mark which FAF interchanges we care about
  faf_db$tag <- ifelse(faf_db$dms_orig %in% idaho & faf_db$dms_dest %in% idaho,
    "internal", "drop")
  faf_db$tag <- ifelse(faf_db$dms_orig %in% idaho & !faf_db$dms_dest %in% idaho,
    "outbound", faf_db$tag)
  faf_db$tag <- ifelse(!faf_db$dms_orig %in% idaho & faf_db$dms_dest %in% idaho,
    "inbound", faf_db$tag)

  # Flows from Oregon or Washington to the rest of the USA should be included,
  # with exception of flows between them and rest of Nevada (Reno) or anywhere
  # in California
  faf_db$tag <- ifelse(faf_db$dms_orig %in% c(oregon, washington) &
      !faf_db$dms_dest %in% c(reno, california), "through", faf_db$tag)
  faf_db$tag <- ifelse(faf_db$dms_orig %in% c(reno, california) &
      !faf_db$dms_dest %in% c(oregon, washington), "through", faf_db$tag)

  # Finally, flows between California, Nevada, Utah, and Arizona and anywhere in
  # Montana probably flow through Idaho, so tag them as well
  faf_db$tag <-
    ifelse(faf_db$dms_orig %in% c(arizona, california, nevada, oregon, utah) &
        faf_db$dms_dest %in% montana, "through", faf_db$tag)
  faf_db$tag <- ifelse(faf_db$dms_orig %in% montana &
      faf_db$dms_dest %in% c(arizona, california, nevada, oregon, utah),
    "through", faf_db$tag)

  # Write those we want to keep
  faf_flows <- dplyr::filter(faf_db, tag != "drop")
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
