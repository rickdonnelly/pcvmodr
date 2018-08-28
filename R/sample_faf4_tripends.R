#' Allocate interregional truck trips to origin and destination traffic zone
#'
#' @param daily_trips Data frame containing the daily FAF truck flows with
#'   FAF origin and destination regions coded (dms_orig and dms_dest,
#'   respectively)
#' @param zonal_attractions Data frame containing daily trips for each
#'   combination of zone, firm category, and truck type. At least one truck type
#'   must be included (and specified in the included_attractions parameter). If
#'   make and use coefficients are defined these corresponding firm categories
#'   are required, but otherwise can be combined into single category. If there
#'   are no trips for a given combination of zone, category, and truck type the
#'   record does not need to be coded.
#' @param included_attractions String or list of strings containing the truck
#'   type(s) to be retained in the zonal_attractions data frame. This field is
#'   required, although you can list all of the truck types coded in the file if
#'   you really want to include all truck types. Defaults to "Heavy".
#' @param zonal_equivalencies Data frame containing the FAF region-TAZ
#'   equivalencies. One of more records can be coded for each FAF region.
#' @param phantom_zones Data frame containing the FAF region-TAZ equivalencies
#'   in cases where one or more FAF regions do not have corresponding TAZs coded
#'   in the network. In this case the user can code them to a nearby zone that
#'   is connected to the network. (Optional)
#' @param save_to File name for saving the daily interregional truck trips
#'   created by this program. (Optional)
#'
#' @details This function samples traffic analysis zone (TAZ) origins and
#'   destinations within FAF regions coded on each trip record. The sampling is
#'   weighted by the number of trips estimated for each combination of zone,
#'   firm category, and truck type. Heavy truck trips are typically used as
#'   these attractors (sampling weights), although any truck type(s) can be
#'   defined. Make and use coefficients can be used to scale the attractors, and
#'   if used should be defined for every combination of firm category and
#'   commodity (SCTG) coded in the data. The program assumes that missing values
#'   are zero, so they should be made explicit otherwise. Note that make or use
#'   scaling only takes place if one or more non-zero coefficients corresponding
#'   to a given commodity are found. If make and use coefficients are not
#'   supplied the trips or frequencies coded in the zonal_attractions file are
#'   used as weights in sampling the zones within each FAF region. The resulting
#'   data frame contains the same information passed in, with origin and
#'   destination TAZ (ozone and dzone, respectively) appended to each trip
#'   record.
#'
#' @export
#' @examples
#' trip_ends <- sample_faf_tripends(daily_trucks, zonal_attractions,
#'   "Heavy", zonal_equivalencies)   # Run with minimal parameters
#' trip_ends <- sample_faf_tripends(daily_trucks, zonal_attractions,
#'   "Heavy", zonal_equivalencies, phantom_zones, makeuse_coefficients,
#'   "trips_with_taz_coded.csv")   # Run with all parameters


sample_faf4_tripends <- function(daily_trips, zonal_attractions,
  included_attractions = "Heavy", zonal_equivalencies, phantom_zones = NULL,
  makeuse_coefficients = NULL, save_to = NULL) {
  # We will append the origin and destination traffic analysis zone (TAZ) to
  # each record using slicing, so define those columns before we get started
  daily_trips$ozone <- NA
  daily_trips$dzone <- NA
  print(paste("Allocating zonal origin and destination for", nrow(daily_trips),
    "interregional trips"), quote = FALSE)

  # We tyically only use heavy truck attractions, so cull them from the list of
  # all attractions
  attractors <- zonal_attractions %>%
    dplyr::filter(truck_type %in% included_attractions) %>%
    dplyr::select(-truck_type)

  # Next merge the zonal equivalencies with phantom zones (if defined) and check
  # that we have at least one equivalent zone defined for each FAF region
  if (!is.null(phantom_zones)) {
    zonal_equivalencies <- dplyr::bind_rows(zonal_equivalencies, phantom_zones)
  }
  equiv_faf_regions <- sort(unique(zonal_equivalencies$faf_region))
  modeled_faf_regions <- sort(unique(c(daily_trips$dms_orig, daily_trips$dms_dest)))
  missing_regions <- setdiff(modeled_faf_regions, equiv_faf_regions)
  if (length(missing_regions)>0) {
    eh <- paste(unlist(missing_regions), collapse = ' ')
    stop(paste("Zonal equivalencies not found for FAF regions:", eh))
  }

  # Next append the zonal attractions to them, keeping on the variables we need
  equiv <- zonal_equivalencies %>%
    dplyr::full_join(., attractors, by = "STDM_TAZ") %>%
    dplyr::select(STDM_TAZ, ITD_district, Category, faf_region, attractions) %>%
    dplyr::mutate(ITD_district = ifelse(is.na(ITD_district), 0, ITD_district)) %>%
    dplyr::arrange(STDM_TAZ, Category)

  # We'll loop through the allocation process for each commodity, as the make
  # and use coefficients will be different for each.
  all_commodities <- sort(unique(daily_trips$sctg2))
  for (this_commodity in all_commodities) {
    # We will start with the origins, which would be scaled using make
    # coefficients if they exist
    these_equiv <- equiv
    if (!is.null(makeuse_coefficients)) {
      # Grab the make coefficients corresponding to this SCTG, which might be an
      # empty set.
      make_coefficients <- dplyr::filter(makeuse_coefficients, MorU == "M",
        sctg2 == this_commodity)

      # If it is not empty then scale the attractions by the make coefficients.
      # We are making the dangerous but unavoidable assumption that any
      # coefficients not explicitly specified are zero, effectively dropping
      # those attractions from the mix.
      if (nrow(make_coefficients)>0) {
        these_equiv <- equiv %>%
          dplyr::left_join(., make_coefficients, by = "Category") %>%
          dplyr::mutate(attractions = coefficient*attractions)
        n_scaled <- nrow(dplyr::filter(these_equiv, !is.na(attractions)))
        print(paste0("SCTG ", this_commodity, ": ", nrow(make_coefficients),
          " make coefficients result in ", n_scaled, " viable attractors"),
          quote = FALSE)
      } else {
        print(paste0("SCTG ", this_commodity, ": no make coefficients found"),
          quote = FALSE)
      }
    }

    # Now collapse the equiv/attractors table to total attractions for each
    # TAZ.
    combined_attractors <- these_equiv %>%
      dplyr::group_by(STDM_TAZ, ITD_district, faf_region) %>%
      dplyr::summarise(attractions = sum(attractions, na.rm = TRUE))

    # Process each origin in turn, selecting the TAZ within each origin region
    # by its attractors (scaled by make coefficient, if applicable)
    these_origins <- sort(
      unique(daily_trips$dms_orig[daily_trips$sctg2 == this_commodity])
    )
    for (this_origin in these_origins) {
      # We can combine these two statements later, but for now figure out how
      # many trip records are of this origin and commodity
      these_trips <- dplyr::filter(daily_trips, dms_orig == this_origin,
        sctg2 == this_commodity)
      n_trips <- nrow(these_trips)

      # Grab the relevant attractors for this origin region
      these_attractors <- dplyr::filter(combined_attractors,
        faf_region == this_origin)

      # A lot of things can do wrong here. One is that there may no defined
      # attractors, which shouldn't occur, and another is that only one TAZ is
      # found, which is often. Catch both of those errors, but otherwise sample
      # from among the eligible zones.
      n_attractors <- nrow(these_attractors)
      if (n_attractors == 0) {
        stop(paste("No attractors found for origin region", this_origin,
          "and commodity", this_commodity))
      } else if (n_attractors == 1) {
        # If there is only one corresponding TAZ the choice is simple
        daily_trips$ozone[daily_trips$dms_orig == this_origin &
            daily_trips$sctg2 == this_commodity] <- these_attractors$STDM_TAZ[1]
      } else {
        # The sum of the attractions might be zero, in which case we want every
        # zone to be equally attractive
        if (sum(these_attractors$attractions) == 0) {
          these_attractors$attractions <- 1
        }

        # Sample the TAZ within this origin FAF region and commodity
        daily_trips$ozone[daily_trips$dms_orig == this_origin &
            daily_trips$sctg2 == this_commodity] <-
          sample(these_attractors$STDM_TAZ, n_trips, replace = TRUE,
            prob = these_attractors$attractions)
      }
    }  # end handling allocation of origin FAF region to zones

    # Repeat the same process, but this time to allocate destination FAF region
    # to zones. We could generalise this into a function, but for now we'll
    # stick to this two-stage (mostly duplicative) code.
    # We will start with the origins, which would be scaled using make
    # coefficients if they exist
    these_equiv <- equiv
    if (!is.null(makeuse_coefficients)) {
      # Grab the make coefficients corresponding to this SCTG, which might be an
      # empty set.
      use_coefficients <- dplyr::filter(makeuse_coefficients, MorU == "U",
        sctg2 == this_commodity)

      # Scale the attractions by the use coefficients, again assuming that any
      # coefficients not explicitly specified are zero, effectively dropping
      # those attractions from the mix.
      if (nrow(use_coefficients)>0) {
        these_equiv <- equiv %>%
          dplyr::left_join(., use_coefficients, by = "Category") %>%
          dplyr::mutate(attractions = coefficient*attractions)
        n_scaled <- nrow(dplyr::filter(these_equiv, !is.na(attractions)))
        print(paste0("SCTG ", this_commodity, ": ", nrow(use_coefficients),
          " use coefficients result in ", n_scaled, " viable attractors"),
          quote = FALSE)
      } else {
        print(paste0("SCTG ", this_commodity, ": no use coefficients found"),
          quote = FALSE)
      }
    }

    # Now collapse the equiv/attractors table to total attractions for each
    # TAZ.
    combined_attractors <- these_equiv %>%
      dplyr::group_by(STDM_TAZ, ITD_district, faf_region) %>%
      dplyr::summarise(attractions = sum(attractions, na.rm = TRUE))

    # Now zoom through each destination FAF region, allocating them destination
    # zones
    these_destinations <- sort(
      unique(daily_trips$dms_dest[daily_trips$sctg2 == this_commodity])
    )
    for (this_destination in these_destinations) {
      # How many trip records are of this origin and commodity?
      these_trips <- dplyr::filter(daily_trips, dms_dest == this_destination,
        sctg2 == this_commodity)
      n_trips <- nrow(these_trips)

      # Grab the relevant attractors for this origin region
      these_attractors <- dplyr::filter(combined_attractors,
        faf_region == this_destination)

      # How we handle the allocation depends upon how many choices we have
      n_attractors <- nrow(these_attractors)
      if (n_attractors == 0) {
        stop(paste("No attractors found for destination region",
          this_destination, "and commodity", this_commodity))
      } else if (n_attractors == 1) {
        # If there is only one corresponding TAZ the choice is simple
        daily_trips$dzone[daily_trips$dms_dest == this_destination &
            daily_trips$sctg2 == this_commodity] <- these_attractors$STDM_TAZ[1]
      } else {
        # The sum of the attractions might be zero, in which case we want every
        # zone to be equally attractive
        if (sum(these_attractors$attractions) == 0) {
          these_attractors$attractions <- 1
        }

        # Sample the TAZ within this origin FAF region and commodity
        daily_trips$dzone[daily_trips$dms_dest == this_destination &
            daily_trips$sctg2 == this_commodity] <-
          sample(these_attractors$STDM_TAZ, n_trips, replace = TRUE,
            prob = these_attractors$attractions)
      }
    }  # end handling allocation of destination FAF region to zones

  }  # end handling this_commodity

  # At this point all of the trip records should have origin and destination
  # traffic analysis zone now coded. Check to make sure that is true.
  problem_children <- dplyr::filter(daily_trips, is.na(ozone) | is.na(dzone))
  if (nrow(problem_children)>0) {
    save_to <- file.path(getwd(), "trip_records_missing_taz.csv")
    readr::write_csv(problem_children, save_to)
    stop(paste("Trip records remain with missing TAZ written to", save_to))
  }

  # Otherwise save the intermediate results if so requested and return the
  # trip list with zonal origins and destinations appended
  if (!is.null(save_to)) { readr::write_csv(daily_trips, save_to) }
  daily_trips
}
