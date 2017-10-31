# Sample trip ends
library(tidyverse)

daily_trips <- readr::read_csv("/Models/pcvmodr-test/outputs/daily_faf_trucks.csv")
zonal_attractions <- readr::read_csv("/Models/pcvmodr-test/outputs/zonal_attractions.csv")
zonal_equivalencies <- readr::read_csv("/Models/pcvmodr-test/inputs/zonal_equivalencies.csv")
makeuse_coefficients <- readr::read_csv("/Models/pcvmodr-test/inputs/makeuse-coefficients.csv")
save_to <- "/Models/pcvmodr-test/outputs/daily_faf_trip_ends.csv"

sample_faf_tripends <- function(daily_trips, zonal_attractions,
  zonal_equivalencies, makeuse_coefficients = NULL, save_to = NULL) {
  # If the user has specified make and use coefficients we will use them to help
  # guide the zonal allocation. However, if they are provided we need to ensure
  # that every category in the zonal attractors has coefficients defined for it.
  # We will do this by summarizing the sum of the make and coefficients by
  # category and comparing them to categories coded in zonal attractions.
  all_categories <- sort(unique(zonal_attractions$Category))
  if (!is.null(makeuse_coefficients)) {
    makeuse_exists <- makeuse_coefficients %>%
      dplyr::group_by(MorU, Category, sctg2) %>%
      dplyr::summarise(total = sum(coefficient))
    print(paste(nrow(makeuse_exists), "make-use coefficient pairs found"),
      quote = FALSE)
  } else {
    makeuse_exists <- dplyr::data_frame()
  }

  # We will use the heavy truck attractions for the traffic analysis zones
  zonal_attactions <- dplyr::filter(zonal_attractions, truck_type == "Heavy")

  # We will update the origin and destination zones on the fly so create them
  daily_trips$ozone <- NA
  daily_trips$dzone <- NA

  # The coefficients vary by commodity carried in the truck, so loop through
  # each separately. Calculate commodity-specific attractors on the fly.
  all_commodities <- sort(unique(daily_trips$sctg2))
  for (this_commodity in all_commodities) {
    # Grab all of the trip records associated with this commodity
    these_trips <- dplyr::filter(daily_trips, sctg2 == this_commodity)
    this_N <- nrow(these_trips)
    if (this_N == 0) next
    print(paste("Processing", this_N, "trip records for SCTG", this_commodity),
      quote = FALSE)

    # We'll allocate the trip origins first. We will need to loop through each
    # origin CFS area at a time.
    these_origins <- sort(unique(these_trips$dms_orig))
    for (this_origin in these_origins) {
      # How many trip records are we working with?
      replicants <- nrow(dplyr::filter(daily_trips, dms_orig == this_origin,
        sctg2 == this_commodity))

      # Which traffic analysis zone is this origin associated with?
      zed <- dplyr::filter(zonal_equivalencies, faf_region == this_origin)
      these_zones <- sort(unique(zed$STDM_TAZ))
      if (length(these_zones) == 0) {
        #stop(paste("No corresponding TAZs found for FAF region",
        #  this_origin))
        print(paste("No corresponding TAZs found for FAF region",
          this_origin))
        next
      }

      # If there is only one corresponding TAZ then we're done
      if (length(these_zones) == 1) {
        lmp <- these_zones[1]
      } else {
        # Get the attractions for this origin
        these_attractions <- dplyr::filter(zonal_attractions,
          STDM_TAZ %in% these_zones)
        n_these_attractions <- nrow(these_attractions)

        # We should never have the case where one or more corresponding TAZs are
        # found but no attractions are found for them, but just in case...
        if (n_these_attractions == 0) {
          these_attractions <- dplyr::data_frame(STDM_TAZ = these_zones,
            attractions = 1.0)
          n_these_attractions <- nrow(these_attractions)
          #print(paste("No attractions found for zones associated with FAF region",
          #  this_origin), quote = FALSE)
        }

        # Sample the possible destinations
        lmp <- sample(these_attractions$STDM_TAZ, replicants,
          replace = TRUE, prob = these_attractions$attractions)
      }

      # Finally, replace the missing values in the trip records with the chosen
      # origin traffic zones
      daily_trips[daily_trips$sctg2 == this_commodity &
          daily_trips$dms_orig == this_origin,] <- lmp
    }
  }




}
