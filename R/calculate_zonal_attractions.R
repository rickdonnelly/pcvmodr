#' Zonal attraction calculator
#'
#' @param synthetic_firms Data frame containing data for every synthetic firm
#'   in the simulation.
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
#' attractions <- calculate_zonal_attractions(synthetic_firms, gen_rates)
#' attractions <- calculate_zonal_attractions(synthetic_firms, qrfm_rates,
#'   save_to = file.path(MY_PATH, "outputs/zonal_attractions.csv"))

calculate_zonal_attractions <- function(synthetic_firms, generation_rates,
  save_to = NULL) {
  # Distill lists of all categories coded in the synthetic firms and those coded
  # in the generation rates. If there are categories in the former that are not
  # included in the latter then crash.
  all_firm_categories <- sort(unique(synthetic_firms$Category))
  all_gen_categories <- sort(unique(generation_rates$Category))
  missing_categories <- setdiff(all_firm_categories, all_gen_categories)
  if (length(missing_categories)>0) {
    stop(paste("Missing generation rates for categories:",
      unlist(missing_categories), collapse = ''))
  }

  # Convert the trip generation rates from wide to tall format so that we can
  # apply rates for each truck type individually without knowing their
  # definition in advance.
  tall_generation_rates <- tidyr::gather(generation_rates, truck_type, rate,
    2:length(names(generation_rates)))

  # Append the generation rates for each synthetic firm. We should wind up with
  # multiple records for each synthetic firm (n = number of different truck
  # types). Then aggregate the attractions by zone, category, and truck type.
  zonal_attractions <- synthetic_firms %>%
    dplyr::right_join(., tall_generation_rates, by = "Category") %>%
    dplyr::mutate(attractions = round(rate * total, 3)) %>%
    dplyr::group_by(STDM_TAZ, Category, truck_type) %>%
    dplyr::summarise(attractions = sum(attractions)) %>%
    dplyr::ungroup()

  # Show us how many attractions were calculated, although somewhat unhelpful
  # metric since they're relative rather than absolute measures
  eh <- addmargins(xtabs(attractions ~ Category + truck_type,
    data = zonal_attractions))
  print("Total calculated attractions by category and truck type:", quote = FALSE)
  print(eh)

  # The user can collapse these categories further if desired, but for now we
  # will return these indexed attractions.
  if (!is.null(save_to)) { readr::write_csv(zonal_attractions, save_to) }
  zonal_attractions
}
