#' Zonal attraction calculator
#'
#' @param zonal_data Data frame containing data for every synthetic firm
#'   in the simulation.
#' @param employment_categories Data frame containing mappings from economic
#'   sectors to firm categories
#' @param generation_rates Data frame containing truck trip generation rates for
#'   each truck type used in the simulation, for each category of firms.
#' @param save_to Fully-specified file name (i.e., includes path) for saving the
#'   zonal attractions.
#'
#' @details This function calculates zonal attractions by truck type and firm
#'   firm category using aggregate trip generation rates. The rates are defined
#'   by the user, and often come from the Quick Response Freight Manual (QRFM),
#'   ITE trip generation rates, or estimation results from a local or borrowed
#'   establishment survey. A data frame containing the attractions by these
#'   classification variables is returned, with the option of also saving the
#'   results to an external file.
#'
#' @export
#' @examples
#' attractions <- calc_alt_zonal_attractions(synthetic_firms, empcat, gen_rates)
#' attractions <- calc_alt_zonal_attractions(synthetic_firms, empcat,
#'   qrfm_rates, save_to = file.path(MY_PATH, "outputs/zonal_attractions.csv"))

#library(tidyverse)
#zonal_data <- readr::read_csv("/Models/pcvmodr-test/inputs/TAZs.csv")
#employment_categories <- readr::read_csv("/Models/pcvmodr-test/inputs/employment_categories.csv")
#generation_rates <- readr::read_csv("/Models/pcvmodr/data-raw/qrfm2_generation_rates.csv")
#save_to <- "/Models/pcvmodr-test/outputs/zonal_attractions.csv"

calc_alt_zonal_attractions <- function(zonal_data, employment_categories,
  generation_rates, save_to = NULL) {
  # We will calculate aggregate trip attractions by category, so start by
  # converting the zonal data from wide to tall format
  zones <- zonal_data %>%
    dplyr::rename(Households = TOTHH_T) %>%
    tidyr::gather(Sector, total, 2:length(names(zonal_data))) %>%
    dplyr::left_join(., employment_categories, by = "Sector") %>%
    dplyr::filter(!is.na(Category)) %>%
    dplyr::group_by(STDM_TAZ, Category) %>%
    dplyr::summarise(employees = sum(as.integer(total))) %>%
    dplyr::filter(employees > 0) %>%
    dplyr::ungroup()

  # Next take the trip generation rates and convert them from wide to tall
  tall_rates <- tidyr::gather(generation_rates, truck_type, rate,
    2:length(names(generation_rates)))

  # Append the generation rates by truck type to each zone record, creating one
  # variant for each
  attractors <- zones %>%
    dplyr::right_join(., tall_rates, by = "Category") %>%
    dplyr::mutate(attractions = round(employees * rate, 3)) %>%
    dplyr::select(-employees, -rate) %>%
    dplyr::arrange(STDM_TAZ, Category, truck_type) %>%
    dplyr::filter(!is.na(STDM_TAZ))
  print(paste(nrow(attractors), "attractors created for",
    length(unique(attractors$STDM_TAZ)), "zones"), quote = FALSE)

  # Finally, write the results
  if (!is.null(save_to)) { readr::write_csv(attractors, save_to) }
  attractors
}
