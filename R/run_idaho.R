#' Run pcvmodr for Idaho statewide model
#'
#' @param runtime_parameters Fully-qualified path and filename of text file
#'   containing runtime parameters required to run models built using pcvmodr
#'   functions
#'
#' @details This function runs the interregional and local truck models built
#'   using pcvmodr functions for the Idaho statewide model.
#'
#' @export
#' @examples
#' run_idaho("./runtime_parameters.txt")


run_idaho <- function(runtime_parameters) {
  library(pcvmodr); library(tidyverse); library(doParallel)

  # Read the runtime parameters
  RTP <<- pcvmodr::get_runtime_parameters(runtime_parameters)

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
  alaska <- 20
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
  # in California or Alaska
  faf_db$tag <- ifelse(faf_db$dms_orig %in% c(oregon, washington) &
      !faf_db$dms_dest %in% c(alaska, reno, california), "through", faf_db$tag)
  faf_db$tag <- ifelse(faf_db$dms_orig %in% c(reno, california) &
      !faf_db$dms_dest %in% c(alaska, oregon, washington), "through", faf_db$tag)

  # Add Alaska to Las Vegas, Arizona, Utah and vice-versa
  faf_db$tag <- ifelse(faf_db$dms_orig %in% c(las_vegas, arizona, utah) &
      faf_db$dms_dest %in% alaska, "through", faf_db$tag)
  faf_db$tag <- ifelse(faf_db$dms_orig %in% alaska &
      faf_db$dms_dest %in% c(las_vegas, arizona, utah), "through", faf_db$tag)

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
  intermediate_output <- file.path(RTP[["scenario_folder"]], "faf_flows.feather")
  pcvmodr::write_file(faf_flows, intermediate_output)

  # Create annual and daily FAF truckload equivalents
  truckloads <- pcvmodr::create_faf4_annual_truckloads(faf_flows)
  daily_trucks <- pcvmodr::sample_faf4_daily_trucks(truckloads)
  filename <- file.path(RTP[["scenario_folder"]], "daily_truckloads.feather")
  pcvmodr::write_file(daily_trucks, filename)

  # When we're done shut down the doParallel cluster
  stopCluster(myCluster)
}
