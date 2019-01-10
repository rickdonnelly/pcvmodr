#' Allocate flows from FAF regions to traffic analysis zones
#'
#' @param faf_truck_flows Data frame containing truck flows between domestic
#'   FAF regions
#' @param internal_regions List of FAF regions included within the modeled area
#' @param zonal_attractions Data frame containing number of attractions by truck
#'   type by traffic analysis zone
#' @param zonal_equivalencies Data frame containing the mapping from FAF regions
#'   to traffic analysis zones
#'
#' @details This function maps the domestic FAF regions coded in their regional
#'   flow database to traffic analysis zone (TAZ) equivalents. This can be done
#'   on a one-to-one basis in `zonal_equivalncies` when there is one and only
#'   one TAZ associated with a given FAF region. However, the FAF regions within
#'   the interior of the modeled area, defined in `internal_regions`, should be
#'   coded as missing values. They should be coded as `zonal_attractions`
#'   instead, where several TAZs can be defined for each internal FAF region.
#'   The resulting data frame includes all of the fields included in the input
#'   `faf_truck_flows` data frame with origin and destination fields added.
#'
#' @export
#' @examples
#' exports <- allocate_faf4_truckloads(daily_trucks, 160, zonal_attractions,
#'   zonal_equivalencies)
#' exports <- allocate_faf4_truckloads(daily_trucks, c(411, 419, 532),
#'   zonal_attractions, zonal_equivalencies)

#@.start_harness
#@library(tidyverse); library(pcvmodr); library(doParallel)
#@faf_truck_flows <- feather::read_feather("~/Models/pcvmodr-test/outputs/daily_truckloads.feather")
#@synthetic_firms <- readr::read_csv("~/Models/pcvmodr-test/inputs/idaho_pseudo_firms.csv")
#@zonal_equivalencies <- "~/Models/pcvmodr-test/inputs/faf44_zonal_equivalencies.csv" %>%
#@  readr::read_csv() %>%
#@  dplyr::select(faf_region, STDM_TAZ)
#@zonal_attractions <- readr::read_csv("~/Models/pcvmodr-test/outputs/zonal_attractions.csv")
#@internal_regions <- c(160)
#@.end_harness


allocate_faf4_truckloads <- function(faf_truck_flows, internal_regions,
  zonal_attractions, zonal_equivalencies) {
  # Recode the origin based on mapping FAF regions to traffic analysis zones
  matched_orig <- zonal_equivalencies %>%
    dplyr::rename(dms_orig = faf_region, origin = STDM_TAZ) %>%
    dplyr::right_join(., faf_truck_flows, by = "dms_orig")

  # Now recode the destination the same way. When we are finished with this all
  # of the FAF domestic origins and destinations outside of internal regions
  # should be coded to traffic analysis zone equivalents
  faf_truck_flows <- zonal_equivalencies %>%
    dplyr::rename(dms_dest = faf_region, destination = STDM_TAZ) %>%
    dplyr::right_join(., matched_orig, by = "dms_dest")

  # Since we are allocating FAF flows we will use only combination truck
  # attractions
  attractions <- zonal_attractions %>%
    dplyr::mutate(combi = CS + DBL + TPT) %>%
    dplyr::select(STDM_TAZ, combi)

  # We can sample the internal database by SCTG, as we might eventually use
  # input-output coefficients that will vary by commodity
  all_commodities <- sort(unique(faf_truck_flows$sctg2))
  for (this_commodity in all_commodities) {
    # First match the origins, which requires recoding the attactions
    N <- nrow(dplyr::filter(faf_truck_flows, sctg2 == this_commodity,
      dms_orig %in% internal_regions))
    if (N > 0) {
      faf_truck_flows$origin[faf_truck_flows$sctg2 == this_commodity &
          faf_truck_flows$dms_orig %in% internal_regions] <- sample(
            attractions$STDM_TAZ, N, replace = TRUE, prob = attractions$combi)
    }

    # Then match the destinations using the same process
    N <- nrow(dplyr::filter(faf_truck_flows, sctg2 == this_commodity,
      dms_dest %in% internal_regions))
    if (N > 0) {
      faf_truck_flows$destination[faf_truck_flows$sctg2 == this_commodity &
          faf_truck_flows$dms_dest %in% internal_regions] <- sample(
            attractions$STDM_TAZ, N, replace = TRUE, prob = attractions$combi)
    }
  }

  # At this point we should have no instances remaining where origin and
  # destination are not both coded. Check to ensure that they are rather than
  # just making that heroic assumption
  problem_children <- dplyr::filter(faf_truck_flows, is.na(origin) |
      is.na(destination))
  n_problems <- nrow(problem_children)
  if (n_problems > 0) {
    # Figure out what FAF regions did not code
    problem_orig <- unique(
      problem_children$dms_orig[is.na(problem_children$origin)])
    problem_dest <- unique(
      problem_children$dms_dest[is.na(problem_children$destination)])
    all_problems <- sort(unique(c(problem_orig, problem_dest)))

    # Since we cannot stop the calling program we can drop things dead in their
    # tracks by returning an empty data frame
    faf_truck_flows <- dplyr::data_frame()  # Underhanded but effective
    stop(paste(length(all_problems),
      "FAF regions could not be coded to traffic zones:",
      paste(unlist(all_problems), collapse = ' ')))
  }

  # Return the results
  return(faf_truck_flows)
}
