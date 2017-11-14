#' Export trips into Cube matrix import format
#'
#' @param trip_trips Data frame containing the final trip list that is ready for
#'   network assignment
#' @param save_to File name for saving the trip list converted into Cube matrix
#'   import format (Optional)
#'
#' @details This function exports the final trip list to a textfile format that
#'   can be imported into Cube for network assignment. Separate columns are
#'   created for each combination of period of the day and truck group. The
#'   latter is hard-coded to map the FAF truck categories to single-unit truck
#'   (SUT) and multi-unit truck (MUT) groups. A data frame containing the
#'   exported files in matrix import format is returned by the function, and can
#'   optionally be written to file using the save_to parameter.
#'
#' @export
#' @examples
#' exported_trips <- export_trip_matrices(trip_list,
#'   "exported_trip_matrices.csv")


export_trip_matrices <- function(trip_list, save_to = NULL) {
  # Define the truck type equivalencies used in the Idaho STDM. The model uses
  # two truck types for assignment, so group them together.
  truck_groups <-
    dplyr::data_frame(truck_type = c("SU", "TT", "CS", "DBL", "TPT"),
      truck_group = c("SUT", "SUT", "MUT", "MUT", "MUT"))

  # Append the truck group to the trip list, as we'll operate on groups instead
  # of individuals
  add_groups <- dplyr::left_join(trip_list, truck_groups, by = "truck_type")

  # Tell us the result
  print("Total truck trips exported by truck type and group:", quote = FALSE)
  print(addmargins(xtabs(~truck_group+truck_type, data = add_groups)))

  # Summarise total truck by OD pairs, period, and truck type
  trip_matrices <- add_groups %>%
    dplyr::group_by(ozone, dzone, truck_group, period) %>%
    dplyr::summarise(total_trips = n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(matrix = paste0(tolower(period), "_", tolower(truck_group))) %>%
    dplyr::select(ozone, dzone, matrix, total_trips) %>%
    dplyr::rename(origin = ozone, destination = dzone) %>%
    tidyr::spread(matrix, total_trips, fill = 0)

  # We will usually write the results to a file, but since save_to is optional
  # we'll write it out only if asked to, and return to calling function
  # otherwise
  if (!is.null(save_to)) { readr::write_csv(trip_matrices, save_to) }
  trip_matrices
}
