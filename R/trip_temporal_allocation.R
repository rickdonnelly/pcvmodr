#' Temporal allocation for individual trip records
#'
#' @param daily_trips Data frame containing the daily trip records
#' @param temporal_distributions Data frame containing the hourly shares for
#'   trip starting times for each vehicle type defined in the simulation
#'
#' @details This function samples starting (departure) hour for each record in
#'   a trip list. Hourly shares must be specified for each vehicle type defined
#'   in the `temporal_distributions` data frame. The function does not ensure
#'   that all 24 hours in a day are defined, or even that values from 0-23 are
#'   required. However, at least one record for each vehicle type must be
#'   present. The resulting data frame adds `start_hour`, `period`, and `matrix`
#'   identifier to each record in the `daily_trips` data frame.
#'
#' @export
#' @examples
#' add_start_hour <- trip_temporal_allocation(daily_truck_trips,
#'   observed_temporal_distributions)


trip_temporal_allocation <- function(daily_trips, temporal_distributions) {
  # Add fields that we'll use sampling to fill
  daily_trips$start_hour <- NA

  # Process each truck type in turn, as each can have a different temporal
  # distribution
  all_vehicle_types <- sort(unique(daily_trips$vehicle_type))
  for (this_vehicle_type in all_vehicle_types) {
    # Grab the temporal distribution (hourly shares) for this truck type
    this_distribution <- dplyr::filter(temporal_distributions,
      vehicle_type == this_vehicle_type)
    if (nrow(this_distribution) < 1) {
      stop(paste("No temporal distribution was found for", this_vehicle_type,
        "vehicle type"))
    } else {
      N <- nrow(dplyr::filter(daily_trips, vehicle_type == this_vehicle_type))
      print(paste("Sampling from", nrow(this_distribution), "intervals for",
        N, this_vehicle_type, "daily truck trips"), quote = FALSE)
    }

    # Sample the starting hour
    daily_trips$start_hour[daily_trips$vehicle_type == this_vehicle_type] <-
      sample(this_distribution$hour, N, replace = TRUE,
        prob = this_distribution$share)
  }

  # We next need to code the period associated with each starting hour
  add_period <- daily_trips %>%
    dplyr::mutate(period = ifelse(start_hour >= 7 & start_hour < 9, "AM",
      ifelse(start_hour >= 9 & start_hour < 16, "MD",
        ifelse(start_hour >= 16 & start_hour < 18, "PM", "NT")))) %>%
    dplyr::mutate(matrix = ifelse(vehicle_type %in% c("SU", "TT"), "SUT",
      "MUT")) %>%
    dplyr::mutate(matrix = paste0(tolower(period), '_', tolower(matrix)))

  # Send the results back to the calling program
  return(add_period)
}
