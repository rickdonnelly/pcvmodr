#' Transform Cube assignment results in ESRI shapefile to data frame
#'
#' @param shapefile Data frame containing the contents of the dBASE (.dbf) file
#'   created by Cube to export loaded network (traffic assignment) results.
#'   Alternatively a string that contains a fully qualified filename (i.e.,
#'   including full path information) can be supplied, which the function will
#'   preprocess before proceeding.
#' @param period_label String denoting the period of the day that the data in
#'   `shapefile` represents. This field will be added to the data frame.
#' @param districts_def Data frame containing a `DistrictID` for each
#'   combination of `A` and `B` in `shapefile`.
#' @param areatypes A list of area type descriptions whose position in the list
#'   correspond to integer values of `AT` in `shapefile`,
#' @param combine.directions Boolean variable denoting whether data for both
#'   directions of two-day directional links will be combined to create a single
#'   bidirectional link record (defaults to TRUE)
#' @param count.direction String denoting whether the daily counts coded in the
#'   `shapefile` are one-way or two-way (bidirectional) counts (defauls to
#'   `two-way`)
#'
#' @details This function converts loaded network data included in a dBASE
#'   (.dbf) file to those required for post-assignment summaries. The format is
#'   specific to the Idaho statewide travel model. A data frame is returned that
#'   includes variables that will be used in subsequent analyses.
#'
#' @export
#' @examples
#' assignment_results <- dump_ISTDM_shapefile(shapefileFN, "AM",
#'   district_definitions, c("CBD", "Urban", "Rural"), TRUE, "two-way")


dump_ISTDM_shapefile <- function(shapefile, period_label, districts_def,
  areatypes = c("CBD", "Urban", "Rural"), combine.directions = TRUE,
  count.direction = "two-way") {
  # Read the raw file, which might either be a filename or data frame already
  # read from the dBASE (.dbf) file inside the shapefile
  content.type <- class(shapefile)
  if (content.type == "character") {
    # Assume it is a fully-qualified pathname, and that we need to read the file
    if (file.exists(shapefile)) {
      shapefileFN <- shapefile
      shapefile <- foreign::read.dbf(shapefileFN, as.is = TRUE)
      print(paste("Read", nrow(shapefile), "records from", shapefileFN),
        quote = FALSE)
    } else {
      stop(paste(shapefile, "does not exist"))
    }
  } else if (content.type %in% c("tibble", "data.frame")) {
    print(paste("Processing", nrow(shapefile), "records from shapefile"),
      quote = FALSE)
  } else {
    stop(paste(content.type, "is not supported for shapefile definition"))
  }

  # Check to see that all of the required fields are present
  sut_ <- paste0("SUT", period_label)
  mut_ <- paste0("MUT", period_label)
  ctime_ <- paste0("CTIME", period_label)
  required_fields <- c(sut_, mut_, ctime_, "A", "B", "PCT_TRUCK", "FINALCOUNT",
    "AT", "MILES", "TYP", "SRC")
  missing_fields <- setdiff(required_fields, names(shapefile))
  if (length(missing_fields) > 0) {
    stop(paste("shapefile missing required fields:",
      paste0(missing_fields, collapse = " ")))
  }

  # We somehow, some way need to rename the columns containing the SUT, MUT, and
  # travel times to consistent value used throughout. We will append the period
  # identifier so that we keep things straight later. Note that the SUT and MUT
  # fields are intentionally numeric rather than integer (i.e., allows for )
  redux <- shapefile %>%
    dplyr::filter(PCT_TRUCK > 0.0, FINALCOUNT > 0.0,
      !TYP %in% c("Centroid", "Ramp")) %>%
    dplyr::select(required_fields) %>%
    dplyr::rename(SUT = sut_, MUT = mut_, travel_time = ctime_) %>%
    dplyr::mutate(total_assigned = SUT + MUT, period = period_label)
  print(paste(nrow(redux), "links with non-zero counts retained"), quote = FALSE)

  ## Now matter what it comes in as convert to tibble
  ##recast <- dplyr::ungroup(tibble::as_tibble(shapefile))
  # Recode the area type from numeric to more descriptive text code
  add_area_type <- redux %>%
    dplyr::mutate(area_type = areatypes[AT]) %>%
    dplyr::select(-AT)

  # We next create a linkID. If the user asks to create bidirectional link data
  # the same identifier is used in both directions. Otherwise simply concatenate
  # the A and B nodes.
  add_identifiers <- add_area_type %>%
    dplyr::mutate(az = pmin(A, B), bz = pmax(A, B),
      linkID = case_when(combine.directions == TRUE ~ paste0(az, '-', bz),
        TRUE ~ paste0(A, "-", B))) %>%
    dplyr::select(-A, -B, -az, -bz)

  # Next calculate the daily count for each link, which can either be one or
  # two-way.
  add_daily_count <- add_identifiers %>%
    dplyr::mutate(xcount = round(PCT_TRUCK * FINALCOUNT, 0),
      daily_count = case_when(count.direction == "two-way" ~ xcount,
        TRUE ~ round(xcount / 2, 0))) %>%
    dplyr::select(-xcount)

  # Finally, add the ITD district coded for each link for summaries by region of
  # the state. That isn't currently coded for each link, so David created a
  # separate file with that info. We will read it in, create linkID that
  # corresponds to what we're using, and append district to the link records.
  add_districts <- districts_def %>%
    dplyr::mutate(DistrictID = ifelse(is.na(DistrictID), 0, DistrictID),
      az = pmin(A, B), bz = pmax(A, B),
      linkID = case_when(combine.directions == TRUE ~ paste0(az, '-', bz),
        TRUE ~ paste0(A, "-", B))) %>%
    dplyr::select(linkID, DistrictID) %>%
    dplyr::filter(!duplicated(linkID)) %>%
    dplyr::left_join(add_daily_count, ., by = "linkID")

  # How many links did not have a districtID appended?
  missing_district <- dplyr::filter(add_districts, is.na(DistrictID))
  n_missing <- nrow(missing_district)
  if (n_missing > 0) {
    missing_def <- sort(unique(missing_district$linkID))
    print(paste("No district defined for", length(missing_def), "links:",
      paste0(missing_def, collapse = " ")), quote = FALSE)
    stop(paste("District definitions missing for", length(missing_def),
      "links"))
  }

  # Tell us how many links by district and functional class and then return the
  # results
  print("Number of links by functional class by ITD district:", quote = FALSE)
  print(addmargins(xtabs(~TYP + DistrictID, data = add_districts)))
  return(dplyr::ungroup(add_districts))
}
