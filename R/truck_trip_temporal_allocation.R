truck_trip_temporal_allocation <- function(local_daily_trips = NULL,
  regional_daily_trips = NULL, temporal_distributions, period_definitions,
  save_to = NULL) {

  # We set NULL values for most of the function parameters by default, but at
  # least one trip list must be provided. Catch cases where it is not.
  if (is.null(local_daily_trips) & is.null(regional_daily_trips)) {
    stop("No trip lists provided for temporal allocation")
  }

  # Combine the two trip lists into a single data frame. We should assume that
  # each has some fields in it that the other doesn't. Thus, we will assign the
  # fields that are common to each that we want to retain.
  if (!is.null(local_daily_trips)) {
    local_trucks <- local_daily_trips %>%
      dplyr::select(origin, destination, truck_type, purpose, status, value,
        tons, sctg2) %>%
      dplyr::mutate(dataset = "local")
  } else {
    local_trucks <- dplyr::data_frame()  # Return empty data frame, no header
  }
  if (!is.null(regional_daily_trips)) {
    regional_trucks <- regional_daily_trips %>%
      dplyr::select(origin, destination, truck_type, status, value, tons,
        sctg2) %>%
      dplyr::mutate(dataset = "regional")
  }
  combined_trucks <- dplyr::bind_rows(local_trucks, regional_trucks)

  # We will fill in the starting hour on the fly, so create that fields now
  combined_trucks <- dplyr::mutate(combined_trucks, dep_hour = NA)

  # The temporal distribution should have shares or probabilities coded for
  # each hour and truck type in the data. So we will process each truck type in
  # turn, randomly choosing a start hour for each trip.
  all_truck_types <- sort(unique(combined_trucks$truck_type))
  for (this_truck_type in all_truck_types) {
    # Grab these truck types
    n_these_trucks <- nrow(dplyr::filter(combined_trucks,
      truck_type == this_truck_type))
    print(paste("Processing", n_these_trucks, this_truck_type, "trips"),
      quote = FALSE)

    # Pull the temporal distribution for this truck type. Stop if we don't find
    # at least two periods defined for the current truck type.
    these_probabilities <- dplyr::filter(temporal_distributions,
      truck_type == this_truck_type)
    if (nrow(these_probabilities)<2) {
      stop(paste("Less than two hourly probabilities found for",
        this_truck_type, "trucks"))
    }

    # Assign the starting hour for each truck trip
    combined_trucks$dep_hour[combined_trucks$truck_type == this_truck_type] <-
      sample(these_probabilities$hour, n_these_trucks, replace = TRUE,
        prob = these_probabilities$probability)
  }


  # TO-DO: Assign the period based upon starting hour

  # TO-DO: Write the results

}
