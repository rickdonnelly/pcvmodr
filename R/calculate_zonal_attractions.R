#' Zonal attraction calculator
#'
#' @param synthetic_firms Data frame containing data for every synthetic firm
#'   in the simulation.
#' @param attraction_rates Data frame containing truck trip generation rates for
#'   each truck type used in the simulation, for each category of firms.
#'
#' @details This function calculates zonal attractions by truck type for each
#'   synthetic firm using aggregate truck trip generation rates. The rates are
#'   defined by the user, and often come from the Quick Response Freight Manual
#'   (QRFM), ITE trip generation rates, or estimation results from a local or
#'   borrowed establishment survey. A data frame containing the attractions by
#'   these classification variables is returned
#'
#' @export
#' @examples
#' attractions <- calculate_zonal_attractions(synthetic_firms, gen_rates)

#@.start_harness
#@library(tidyverse)
#@synthetic_firms <- readr::read_csv("/Models/pcvmodr-test/inputs/idaho_pseudo_firms.csv")
#@attraction_rates <- readr::read_csv("/Models/pcvmodr-test/inputs/qrfm2_generation_probabilities.csv")
#@.end_harness

calculate_zonal_attractions <- function(synthetic_firms, attraction_rates) {
  # Distill lists of all categories coded in the synthetic firms and those coded
  # in the generation rates. If there are categories in the former that are not
  # included in the latter then crash.
  all_firm_categories <- sort(unique(synthetic_firms$sector))
  all_gen_categories <- sort(unique(attraction_rates$sector))
  missing_categories <- setdiff(all_firm_categories, all_gen_categories)
  if (length(missing_categories) > 0) {
    stop(paste("Missing generation rates for categories:",
      unlist(missing_categories), collapse = ''))
  }

  # Convert the trip generation rates from wide to tall format so that we can
  # apply rates for each truck type individually without knowing their
  # definition in advance.
  tall_rates <- tidyr::gather(attraction_rates, truck_type, rate, SU:TPT)

  # Append the generation rates for each synthetic firm. We should wind up with
  # multiple records for each synthetic firm (n = number of different truck
  # types). Then aggregate the attractions by zone, category, and truck type.
  zonal_attractions <- synthetic_firms %>%
    dplyr::right_join(., tall_rates, by = "sector") %>%
    dplyr::mutate(daily_attractions = round(rate * size, 4)) %>%
    dplyr::group_by(STDM_TAZ, truck_type) %>%
    dplyr::summarise(daily_attractions = sum(daily_attractions)) %>%
    dplyr::arrange(STDM_TAZ) %>%
    tidyr::spread(truck_type, daily_attractions)

  # There isn't really anything exciting to report here, so just return the
  # results
  return(zonal_attractions)
}
