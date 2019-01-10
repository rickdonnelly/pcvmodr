#' Export trips into Cube matrix import format
#'
#' @param trip_trips Data frame containing the final trip list that is ready for
#'   network assignment
#'
#' @details This function exports the final trip list to a textfile format that
#'   can be imported into Cube for network assignment. Separate columns are
#'   created for each combination of period of the day and truck group. The
#'   latter is hard-coded to map the FAF truck categories to single-unit truck
#'   (SUT) and multi-unit truck (MUT) groups. A data frame containing the
#'   exported files in matrix import format is returned by the function.
#'
#' @export
#' @examples
#' exported_trips <- export_trip_matrices(trip_list)


export_trip_matrices <- function(trip_list) {
  # Define the truck type equivalencies used in the Idaho STDM. The model uses
  # two truck types for assignment, so group them together.
  truck_groups <-
    dplyr::data_frame(vehicle_type = c("SU", "TT", "CS", "DBL", "TPT"),
      truck_group = c("SUT", "SUT", "MUT", "MUT", "MUT"))

  # Append the truck group to the trip list, as we'll operate on groups instead
  # of individuals
  add_groups <- dplyr::left_join(trip_list, truck_groups, by = "vehicle_type")

  # Tell us the result
  print("Total truck trips exported by truck type and group:", quote = FALSE)
  print(addmargins(xtabs(~truck_group + vehicle_type, data = add_groups)))

  # Summarise total truck by OD pairs, period, and truck type
  trip_matrices <- add_groups %>%
    dplyr::group_by(origin, destination, truck_group, period) %>%
    dplyr::summarise(total_trips = n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(matrix = paste0(tolower(period), "_", tolower(truck_group))) %>%
    dplyr::select(origin, destination, matrix, total_trips) %>%
    tidyr::spread(matrix, total_trips, fill = 0)

  # We're done!
  return(trip_matrices)
}
