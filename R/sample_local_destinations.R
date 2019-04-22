#' Sample destinations for trips in local truck tours
#'
#' @param truck_origins Data frame containing discrete trip records with origin
#'   zones annotated
#' @param skim_distances Data frame containing distances between each IDSTM_TAZ
#'   combination
#' @param trip_length_targets Data frame containing observed or asserted trip
#'   length frequencies or probabilities by distance for each truck type in the
#'   simulation
#' @param max_distance The maximum distance that a local trip will travel to
#'   reach a discrete destination (default = 120 miles)
#' @param allow_intrazonals A flag to indicate whether intrazonal trips will be
#'   included in the data (default = TRUE)
#' @param debug_tracing A flag to indicate whether tracing statements for each
#'   origin and truck type combination are printed (default = FALSE)
#'
#' @details This function carries out a singly-constrained destination choice,
#'   where the utility of each destination can be varied between distance and
#'   size term. The resulting tibble mirrors the input trip list with the
#'   addition of the destination.
#'
#' @export
#' @examples
#' add_destinations <- sample_local_destinations(trip_records, skim_matrix,
#'   trip_length_targets)

sample_local_destinations <- function(truck_origins, skim_distances,
  trip_length_targets, max_distance = 120.0, allow_intrazonals = TRUE,
  debug_tracing = FALSE) {
  simulation.start <- proc.time()

  # Check to make sure that we don't have trips originating from zones that
  # are not in the skim matrix. If so, stop the simulation.
  all_truck_origins <- sort(unique(truck_origins$origin))
  all_skim_origins <- sort(unique(skim_distances$origin))
  problem_children <- setdiff(all_truck_origins, all_skim_origins)
  if (length(problem_children) > 0) {
    stop(paste("Local origins in trip list have no corresponding skim:",
      paste(problem_children, collapse = ' ')))
  }

  # Sum the attractors by zone. We will need to have data defined for each of
  # the zones defined in the skim matrix, even if some have zero truck trips
  # associated with it. Thus, we'll substitute any missing values with zeros.
  attractors <- truck_origins %>%
    dplyr::group_by(origin, vehicle_type) %>%
    dplyr::summarise(attractors = n()) %>%
    dplyr::mutate(destination = origin)

  # Transform the trip length frequency targets by truck type from wide to tall
  # format and impute missing values
  x <- dplyr::tibble(distance = 0:max_distance)
  targets <- trip_length_targets %>%
    tidyr::gather(vehicle_type, share, SU:TPT) %>%
    dplyr::left_join(x, ., by = "distance")

  # Run the model: The ideal distributions and alpha parameters differ by truck
  # type, so we will handle each one differently. At the end of handling each
  # truck type we will add those results to a final data table that will have OD
  # flows.
  truck_origins$destination <- NA_integer_
  all_vehicle_types <- sort(unique(truck_origins$vehicle_type))
  for (this_vehicle_type in all_vehicle_types) {
    # Tell us the problem size
    these_trips <- dplyr::filter(truck_origins, vehicle_type == this_vehicle_type)
    print(paste("Sampling destinations for", nrow(these_trips), this_vehicle_type,
      "origins"), quote = FALSE)

    # Associate skim distances with probabilities for this truck type
    these_skims <- skim_distances %>%
      dplyr::filter(value <= max_distance) %>%
      dplyr::mutate(distance = round(value + 0.5, 0)) %>%
      dplyr::left_join(., dplyr::filter(targets, vehicle_type == this_vehicle_type),
        by = "distance")

    # We will have to sample for each origin zone separately
    these_origins <- sort(unique(these_trips$origin))
    for (this_origin in these_origins) {
      # How many trips from this origin and truck type do I have?
      N <- nrow(dplyr::filter(these_trips, origin == this_origin &
          vehicle_type == this_vehicle_type))
      if (N < 1) next

      # Grab all of the eligible destinations for trips originating in this zone
      these_targets <- dplyr::filter(these_skims, origin == this_origin)

      # How we sample the destinations depends upon how many targets we have
      n_targets <- nrow(these_targets)
      if (debug_tracing == TRUE) print(paste0("truck type=", this_vehicle_type,
        " origin=", this_origin, " N=", N, " targets=", n_targets),
        quote = FALSE)
      if (n_targets == 0) {
        # If there are no available targets we will assume that all of the trips
        # are internal. This is still a pathological condition, so write a
        # warning as well.
        truck_origins$destination[truck_origins$vehicle_type == this_vehicle_type &
            truck_origins$origin == this_origin] <- truck_origins$origin
        warning(paste("No eligible destinations found for truck type",
          this_vehicle_type, "in origin", this_origin))
      } else if (n_targets == 1) {
        truck_origins$destination[truck_origins$vehicle_type == this_vehicle_type &
            truck_origins$origin == this_origin] <- these_targets$destination
      } else {
        truck_origins$destination[truck_origins$vehicle_type == this_vehicle_type &
            truck_origins$origin == this_origin] <-
          sample(these_targets$destination, N, replace = TRUE,
            prob = these_targets$share)
      }
    }
  }

  # Append the skim distance to each trip record, but before we do that we need
  # to change the unhelpful distance colname from "value" to something that
  # matches the long-distance truck records
  skim_distances <- dplyr::rename(skim_distances, wgt_dist = value)
  trip_list <- dplyr::left_join(truck_origins, skim_distances,
    by = c("origin", "destination"))

  # Shut down and return
  simulation.stop <- proc.time()
  elapsed_seconds <- round((simulation.stop-simulation.start)[["elapsed"]], 1)
  message(paste("Simulation time=", elapsed_seconds, "seconds"))
  return(trip_list)
}
