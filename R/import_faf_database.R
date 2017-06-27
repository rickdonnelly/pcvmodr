#' Import FHWA Freight Analysis Framework (FHWA) Version 4.0+ regional database
#'
#' @param faf_data Data frame containing the FAF regional full database in the
#'   format originally supplied by FHWA. Alternatively, the user can code the
#'   filename (with path) of a comma-separated value (CSV) file containing the
#'   same information, which the program will import into a data frame.
#' @param distance Data frame containing the inter-regional (FAF region to FAF
#'   region) average distances. Alternatively, the user can code the filename
#'   (with path) of a comma-separated value file containing the data.
#' @param save_to File name for saving the FAF data transformed into the format
#'   used internally by the CV modeling system
#'
#' @details This function reads the FHWA Freight Analysis Framework Verion 4
#'   regional database in comma-separated value (CSV) format, as found on their
#'   website, and translates it into the format used in our analyses. Some
#'   categorical variables are mapped from integer values to more descriptive
#'   strings, value and tons are scaled back to real numbers, and average inter-
#'   regional distances are appended to each record. A single data frame of
#'   processed records is returned. It is important to note that the origin(s)
#'   and destination(s) are still coded in FAF regions at this point, as data
#'   still represent annual flows.
#'
#' @export
#' @examples
#' faf43 <- import_faf_database(faf_flows, faf_distances)
#' faf43 <- import_faf_database("/Models/CT_Future2040/inputs/faf4_3_csv.zip",
#'   "faf_interregional_distances.csv.gz", "faf43-reformmatted.csv")


import_faf_database <- function(faf_data, distances = NULL, save_to = NULL) {
  # If the FAF data are provided as a data frame then we're ready to. But if it
  # is a string assume that it is a fully-qualified path name, and attempt to
  # read it into a data frame.
  if (!is.data.frame(faf_data)) {
    print(paste("Reading FAF data from", faf_data), quote = FALSE)
    faf_data <- readr::read_csv(faf_data)
  }

  # Irrespective of how the data were provided we need to do a fair amount of
  # cleanup before we can use them directly. We will first morph it into tall
  # format to make handling and filtering by year easier.
  tall <- faf_data %>%
    tidyr::gather(variable, value, -fr_orig:-trade_type) %>%

    # Next we will split the variable field we just created at the underscore in
    # order to separate the variable from the year.
    tidyr::separate(variable, c("variable", "year"), sep = '_') %>%

    # Keep only value and tons (jettison ton-miles and current value)
    dplyr::filter(variable %in% c("value", "tons")) %>%

    # And now rotate value and tons into separate columns and transform the
    # variables from implied millions (value) and thousands (tons) to unscaled
    # values.
    tidyr::spread(variable, value) %>%
    dplyr::mutate(value = as.numeric(value)*1e6, tons = as.numeric(tons)*1e3,
      year = as.integer(year), dms_orig = as.integer(dms_orig),
      dms_dest = as.integer(dms_dest), fr_orig = as.integer(fr_orig),
      fr_dest = as.integer(fr_dest), sctg2 = as.integer(sctg2))

  # Recode the trade type from numeric to string description
  trade_type_labels <- c("Domestic", "Import", "Export")
  tall$trade_type <- trade_type_labels[tall$trade_type]

  # Map the numeric mode codes to more descriptive string values. If coded to
  # missing values for foreign modes they should still remain that way. I wanted
  # to use same efficient subscripting like I did above, but R sensed that and
  # slowed the code way down. Maybe sometime next century R will get a switch
  # statement?
  tall$dms_mode <- ifelse(tall$dms_mode == 1, "Truck",
    ifelse(tall$dms_mode == 2, "Rail",
      ifelse(tall$dms_mode == 3, "Water",
        ifelse(tall$dms_mode == 4, "Air",
          ifelse(tall$dms_mode == 5, "Multiple",
            ifelse(tall$dms_mode == 6, "Pipeline",
              ifelse(tall$dms_mode == 7, "Other",
                ifelse(tall$dms_mode == 8, "None", NA))))))))
  # Repeat the same thing, this time for inbound foreign flows
  tall$fr_inmode <- ifelse(tall$fr_inmode == 1, "Truck",
    ifelse(tall$fr_inmode == 2, "Rail",
      ifelse(tall$fr_inmode == 3, "Water",
        ifelse(tall$fr_inmode == 4, "Air",
          ifelse(tall$fr_inmode == 5, "Multiple",
            ifelse(tall$fr_inmode == 6, "Pipeline",
              ifelse(tall$fr_inmode == 7, "Other",
                ifelse(tall$fr_inmode == 8, "None", NA))))))))
  # And again, this time for outbound foreign flows
  tall$fr_outmode <- ifelse(tall$fr_outmode == 1, "Truck",
    ifelse(tall$fr_outmode == 2, "Rail",
      ifelse(tall$fr_outmode == 3, "Water",
        ifelse(tall$fr_outmode == 4, "Air",
          ifelse(tall$fr_outmode == 5, "Multiple",
            ifelse(tall$fr_outmode == 6, "Pipeline",
              ifelse(tall$fr_outmode == 7, "Other",
                ifelse(tall$fr_outmode == 8, "None", NA))))))))

  # Finally, we'll append the region-to-region average distances to the records
  # if the user has supplied that information. However, we will stop if we do
  # not find distances for all region pairs in the database.
  if (!is.null(distances)) {
    # If the parameter is a string assume it is a filename, so grab contents
    if (!is.data.frame(distances)) {
      print(paste("Reading FAF interregional distances from", distances),
        quote = FALSE)
      distances <- readr::read_csv(distances)
    }

    # Append the distances to the data records.
    tall <- dplyr::left_join(tall, distances, by = c("dms_orig", "dms_dest"))

    # Check to make sure that distances were found for all interchanges. If not
    # give us some clues about what went wrong and then quit.
    missing_distances <- dplyr::filter(tall, is.na(distance))
    if (nrow(missing_distances)>0) {
      missing_distances$zone_pairs <- paste(missing_distances$dms_orig, '-',
        missing_distances$dms_dest, sep = '')
      zed <- sort(unique(missing_distances$zone_pairs))
      print(paste("Missing distances for:", zed), quote = FALSE)
      stop(paste("Distances not provided for", length(zed),
        "FAF region interchanges"))
    }
    # But otherwise we should be good to go
  }

  # Save the resulting data frame if the user elects to
  if (!is.null(save_to)) {
    print(paste(nrow(tall), "records saved to", save_to), quote = FALSE)
    readr::write_csv(tall, save_to)
  }

  # Return the data frame
  tall
}
