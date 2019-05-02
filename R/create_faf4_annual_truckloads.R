#' Create annual truckload equivalents from FAF v4+ regional flow database
#'
#' @param faf_flows Data frame containing the FAF interregional flows for the
#'   analysis year, usually derived by applying preprecessor function(s) on
#'   FHWA database for the desired target year.
#' @param load_consolidation_factor Numeric value representing the potential for
#'   two or more commodity flows to be consolidated into single truck for
#'   delivery (default = 1.0)
#'
#' @details This function converts the commodity flows, measured in annual tons
#'   and value between FAF regions, into truckload equivalents. The five-step
#'   method described in the "FAF4 Freight Traffic Analysis" report is
#'   currently implemented, and the user should consult that documentation to
#'   understand the data requirements and methods used. The translation is
#'   deterministic, for it uses tables of proportions by vehicle type, body
#'   style, commodity, and trade type to arrive at one or more discrete annual
#'   truck trips by each of those indices, between each set of FAF regions that
#'   have commodity flows coded in the data. The result is a new, and much
#'   larger, data frame containing the number of truck trips for each inter-
#'   regional flow pair in the FAF data.
#'
#' @export
#' @examples
#' annual_trucks <- create_faf4_annual_truckloads(faf_database)


create_faf4_annual_truckloads <- function(faf_flows,
  load_consolidation_factor = 1.0) {
  simulation.start <- proc.time()

  # DEFINE TRUCK ALLOCATION FACTORS
  # Read the truck allocation factors (Table 3-3) into memory and define helper
  # function to extract values for the appropriate distance range.
  truck_allocation_factors <- read_file(RTP[["truck_allocation_factors"]])

  # Normalize the allocation factors by vehicle type within the chosen distance
  # on the fly
  getTruckAllocationFactors <- function(distance) {
    distance <- round(distance, 0)
    truck_allocation_factors %>%
      dplyr::filter(distance >= minimum_range & distance <= maximum_range) %>%
      dplyr::mutate(alloc_factor = allocation_factor/sum(allocation_factor)) %>%
      dplyr::select(vehicle_type, alloc_factor)
  }

  # DEFINE TRUCK EQUIVALENCY FACTORS
  # Read the truck equivalency factors found in Appendix A of the FAF3 Freight
  # Traffic Analysis report, which we'll need to convert from wide to tall
  # format. The factors are used for converting kilotons into truckload
  # equivalents. Since we are dealing with tons we need to scale the factors to
  # account for the differences.
  truck_equivalency_factors <- read_file(RTP[["truck_equivalency_factors"]]) %>%
    tidyr::gather(body_type, equiv_factor, auto:other) %>%
    dplyr::filter(equiv_factor > 0.0)

  # We will also define a helper function to pull the appropriate values by
  # commodity and vehicle type.
  getTruckEquivalencyFactors <- function(commodity) {
    dplyr::filter(truck_equivalency_factors, sctg2 == commodity)
  }

  # DEFINE EMPTY TRUCK FACTORS
  # Finally, read empty truck factors from Table 3-5 and define a helper
  # function to grab the appropriate ones by body type and flow direction.
  # Again, convert from the original wide to tall format on the fly.
  empty_truck_factors <- read_file(RTP[["empty_truck_factors"]]) %>%
    tidyr::gather(vehicle_type, empty_factor, SU:TPT)

  # As with the other factors define a helper function to access them
  getEmptyTruckFactors <- function(flow_direction) {
    dplyr::filter(empty_truck_factors, crossing_type == flow_direction)
  }

  # FUNCTION TO CALCULATE TRUCK EQUIVALENCIES
  # Apply this function to each FAF flow record by passing it to the function.
  # The resulting data table will have flows by each vehicle (truck) type and
  # status (loaded, empty).
  calcTruckloadEquivalencies <- function(replicant) {
    # Get the factors appropriate for the current distance between domestic FAF
    # regions, trade type, and commodity
    taf <- getTruckAllocationFactors(replicant$wgt_dist)
    tef <- getTruckEquivalencyFactors(replicant$sctg2)
    all_trade_types <- c("domestic", "export", "import")
    trip_type <- all_trade_types[replicant$trade_type]
    etf <- getEmptyTruckFactors(trip_type)

    # Allocate the tonnage to each vehicle (as in Table 3-7)
    taf$tons <- replicant$exp_tons * taf$alloc_factor * load_consolidation_factor

    # Merge the tonnage by vehicle type with the truck equivalency factors to
    # get tonnage by vehicle type and body type, and then finger the vehicle
    # type-body type combo with largest number of trucks.
    loaded <- merge(tef, taf, by = "vehicle_type", all.x = TRUE) %>%
      dplyr::mutate(annual_trucks = tons * equiv_factor) %>%
      dplyr::select(-equiv_factor, -alloc_factor, -tons)
    max_annual_trucks <- which.max(loaded$annual_trucks)

    # It is possible that we wind up with less than one truck using the FAF
    # factors, which we will always round up to one truck. If that happens pick
    # the vehicle and body type that has the highest number of them.
    zed <- sum(loaded$annual_trucks)
    if (zed < 1.0) {
      loaded <- loaded[max_annual_trucks,]
      loaded$annual_trucks <- 1.0
    }

    # Calculate the number of empty trucks. If we obtain zero trucks we will
    # accept that.
    empty <- merge(loaded, etf, by = c("body_type", "vehicle_type")) %>%
      dplyr::mutate(empty_trucks = annual_trucks * empty_factor) %>%
      dplyr::select(-crossing_type, -empty_factor)

    # We needed to retain the body types to calculate empties, but now we can
    # collapse each group to vehicle types.
    loaded <- loaded %>%
      dplyr::group_by(vehicle_type) %>%
      dplyr::summarise(annual_trucks = round(sum(annual_trucks), 0)) %>%
      dplyr::mutate(status = "Loaded")
    # In rare cases we round to zero trucks in each vehicle type despite zed
    # greater than 1.0, so handle that
    if (sum(loaded$annual_trucks) == 0.0 & zed > 1.0) {
      loaded$annual_trucks[loaded$vehicle_type == "SU"] <- 1
    }
    # Handle empties in same way, except that we'll accept zero trucks
    empty <- empty %>%
      dplyr::group_by(vehicle_type) %>%
      dplyr::summarise(annual_trucks = round(sum(empty_trucks), 0)) %>%
      dplyr::mutate(status = "Empty")

    # dplyr's mutate cannot access variables outside of the table passed to it,
    # so append the original value and tons for it to operate upon
    loaded$tons <- replicant$exp_tons
    loaded$value <- replicant$exp_value

    # Create a separate record for the combined groups and merge with the data
    # from the flow record. Note that we drop the original value and tons, as
    # they are now split among the various loaded truck types.
    loaded <- loaded %>%
      dplyr::mutate(percent = annual_trucks / sum(annual_trucks),
        tons = percent * tons, value = percent * value) %>%
      dplyr::select(-percent)
    empty <- dplyr::mutate(empty, tons = 0.0, value = 0.0)
    together <- dplyr::bind_rows(loaded, empty) %>%
      dplyr::filter(annual_trucks > 0)

    # Add the SCTG code back in, as we will use that to merge the two tables
    together$sctg2 <- replicant$sctg2
    result <- merge(select(replicant, -exp_tons, -exp_value), together,
      by="sctg2")
    result$zed <- zed
    return(result)
  }

  # RUN THE ALLOCATION
  # Read the FAF data and run it through the simulation. We are only interested
  # in truck trips, so drop other modes. The rub is that we have some FAF
  # datasets with string values for mode and others use less helpful integer
  # codes. So we'll have to select based upon either one.
  if (class(faf_flows$dms_mode) == "character") {
    truck_mode <- "truck"
  } else {
    truck_mode <- 1
  }
  truck_flows <- dplyr::filter(faf_flows, dms_mode == truck_mode)

  # Run the simulation using the cluster already registered by the calling
  # program
  if (length(getDoParName()) == 0) {
    stop("doParallel cluster apparently not available")
  }
  print(paste("Creating FAF annual truckloads from", nrow(truck_flows),
    "truck flow records"), quote = FALSE)
  combined <- foreach(i = 1:nrow(truck_flows), .combine="rbind",
    .packages=c("tidyverse"), .errorhandling = "remove") %dopar% {
      calcTruckloadEquivalencies(truck_flows[i,])
    }

  # Wind down and return the FAF truckload equivalents for the requested year
  simulation.stop <- proc.time()
  elapsed_seconds <- round((simulation.stop-simulation.start)[["elapsed"]], 1)
  print(paste("Simulation time=", elapsed_seconds, "seconds"), quote = FALSE)
  return(combined)
}
