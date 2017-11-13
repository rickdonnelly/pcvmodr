#' Sample starting hour for daily truck trips
#'
#' @param local_daily_trips Data frame containing local (intra-regional) daily
#'   truck trips (optional, but only if regional_data_trips is defined)
#' @param regional_daily_trips Data frame containing regional (inter-regional)
#'   daily truck trips (optional, but only if local_data_trips is defined)
#' @param temporal_distributions Data frame containing frequencies or
#'   probabilities of trips by hour or period of the day, by truck type
#' @param period_definitions Data frame containing the period code corresponding
#'   to each of the hours defined in temporal_distributions
#' @param save_to File name for saving the trip list with starting hour or
#'   period appended
#'
#' @details This function samples a user-specified distribution of frequencies
#'   or probabilities of trip starting hour or period for each trip in the input
#'   list. The user can pass either the local or regional trip list, or both,
#'   enabling the use of different temporal distributions for each group of
#'   trips if desired. Note that missing hours in the temporal distributions are
#'   not imputed; if there are no data for a given time slice no trips will be
#'   assigned to it. The resulting trip records are written with the starting
#'   hour or period appended to them.
#'
#' @export
#' @examples
#' hourly_trips <- allocate_departure_hour(local_daily_trucks, NULL,
#'   temporal_distributions, period_definitions, save_to = "hourly_trucks.csv")

allocate_departure_hour <- function(local_daily_trips = NULL,
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
      dplyr::select(ozone, dzone, truck_type, purpose, status, value, tons,
        sctg2) %>%
      dplyr::mutate(dataset = "local")
  } else {
    local_trucks <- dplyr::data_frame()  # Return empty data frame, no header
  }
  if (!is.null(regional_daily_trips)) {
    regional_trucks <- regional_daily_trips %>%
      dplyr::rename(truck_type = vehicle_type) %>%
      dplyr::select(ozone, dzone, truck_type, status, value, tons, sctg2) %>%
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

  # Assign the period based upon starting hour
  combined_trucks$period <- NA   # Starting value
  n_periods <- nrow(period_definitions)
  for (i in (1:n_periods)) {
    combined_trucks$period <-
      ifelse(combined_trucks$dep_hour >= period_definitions$begin[i] &
          combined_trucks$dep_hour <= period_definitions$end[i],
        period_definitions$period[i], combined_trucks$period)
  }

  # It's entirely possible that the user did not cover every minute of 24 hours,
  # in which case there would still be some missing values in the period field.
  # Catch and report that before moving on.
  missing_periods <-
    sort(unique(combined_trucks$dep_hour[is.na(combined_trucks$period)]))
  if (length(missing_periods) > 0) {
    unallocated_hours <- paste(unlist(missing_periods), collapse = ' ')
    stop(paste("Hours without corresponding period defined:", unallocated_hours))
  }

  # Write the results and exit stage right
  print("Total trips by period and truck type:", header = TRUE)
  print(addmargins(xtabs(~period+truck_type, data = combined_trucks)))
  if (!is.null(save_to)) { readr::write_csv(combined_trucks, save_to) }
  combined_trucks
}
