#' Quick Response Freight Manual 2nd Edition (QRFM2) truck generation
#'
#' @param synthetic_firms Data frame containing data for every synthetic firm
#'   in the simulation.
#' @param generation_rates Data frame containing QRFM2 truck trip generation
#'   rates by truck type and firm sector.
#'
#' @details This function calculates discrete truck trips by truck type and
#'   sector for every synthetic firm in the simulation. The QRFM2 trip rates are
#'   used to generate total aggregate trips, which are allocated to discrete
#'   synthetic firms. A tibble containing trip records is returned by this
#'   program.
#'
#' @export
#' @examples
#' trip_origins <- qfrm2_truck_generation(synthetic_firms, gen_rates)


qrfm2_truck_generation <- function(synthetic_firms, generation_rates) {
  # Convert generation rates from wide to tall format
  tall_rates <- generation_rates %>%
    tidyr::gather(vehicle_type, rate, SU:TPT) %>%
    dplyr::filter(rate > 0.0)

  # Define a quick bucket rounding function that operates on a numeric vector
  vector_bucket_round <- function(v) {
    last <- length(v)
    residual <- 0
    for (i in (1:(last-1))) {
      # TO-DO: I saw a recipe for doing this in two steps, need to find that
      v[i] <- v[i]+residual
      residual <- v[i] - trunc(v[i])
      v[i] <- trunc(v[i])
    }
    v[last] <- round(v[last]+residual)
    v
  }

  # We will append a scaling factor for each county, based on how urban it is
  urbanized_counties <- c("Ada", "Canyon")
  synthetic_firms$scaling_factor <- ifelse(
    synthetic_firms$County %in% urbanized_counties, 0.4, 1)

  # Calculate daily rates by truck type
  all_vehicle_types <- sort(unique(tall_rates$vehicle_type))
  results <- dplyr::tibble()
  for (this_vehicle_type in all_vehicle_types) {
    # Calculate exact (fractional) number of trips for this truck type
    this <- tall_rates %>%
      dplyr::filter(vehicle_type == this_vehicle_type) %>%
      dplyr::left_join(synthetic_firms, ., by = "sector") %>%
      dplyr::mutate(fp_trucks = rate * size * scaling_factor,
        int_trucks = vector_bucket_round(fp_trucks)) %>%
      dplyr::filter(int_trucks > 0)

    # Report number of trips
    pct_zones_generated <- pcvmodr::percent(nrow(this), nrow(synthetic_firms))
    print(paste0("Truck type ", this_vehicle_type, ": firms=", nrow(this), " (",
      pct_zones_generated,"%) trips=", sum(this$int_trucks)), quote = FALSE)

    # Add the results to previously calculated trip ends
    results <- dplyr::bind_rows(results, this)
  }

  # Write the results in format compatible with truck tour generator, which
  # includes some fields not included in this variant of generation
  keep <- results %>%
    dplyr::mutate(trip_number = NA_integer_, sctg2 = 99, tons = NA_real_,
      value = NA_real_, direction = "local") %>%
    dplyr::rename(origin = STDM_TAZ) %>%
    dplyr::select(pfirmID, trip_number, sector, vehicle_type, sctg2, tons,
      value, origin, direction) %>%
    dplyr::arrange(pfirmID, trip_number)

  # Return the truck trip records and exit stage left
  return(keep)
}
