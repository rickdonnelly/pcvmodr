#' List the class and range of values in each column of a data frame
#'
#' @param df Data frame containing the variables to be summarized
#' @param threshold The number of discrete values of a certain variable to
#'   enumerate. Defaults to 10, which typically fits within margin of knitr
#'   output
#'
#' @details This function creates a data frame with the name, class, and range
#'   of values included for each column in a data frame, as well as the number
#'   of missing and zero values. It returns a data frame with that information,
#'   with one variable per record.
#'
#' @export
#' @examples
#' df_contents <- data_frame_contents, 9)


data_frame_contents <- function(df, threshold = 10) {
  all_fields <- names(df)
  profiles <- dplyr::data_frame()
  for (this_field in all_fields) {
    n_missing <- sum(is.na(df[[this_field]]))
    field_type <- class(df[[this_field]])
    unique_values <- sort(unique(df[[this_field]]))
    range <- ifelse(length(unique_values) < threshold,
      paste(unlist(unique_values), collapse = ", "),
      paste(unique_values[1], "--", unique_values[length(unique_values)]))
    these <- dplyr::data_frame(Variable = this_field, Class = field_type,
      Unique = length(unique_values), Missing = n_missing, Values = range)
    profiles <- dplyr::bind_rows(profiles, these)
  }
  return(profiles)
}

