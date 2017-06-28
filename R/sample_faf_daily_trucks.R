#' Sample discrete daily trucks from database of FAF annual truckload equivalents
#'
#' @param annual_truckload_equivalents Data frame containing the FAF regional
#'   database with number of annual truck trips appended. If a string is passed
#'   instead of data frame the program will attempt to read the data in expected
#'   format from the full path and filename in the string.
#' @param scaling_factor is a floating point value that scales the annual trucks
#'   to account for multiple stops and trip durations longer than the simulation
#'   period (see description). The default value is 1.0.
#' @param target_week is an integer that defines which of the 52 weeks in the
#'   year the trips are sampled from. The default value is 19, which roughly
#'   corresponds in the second week in May.
#' @param target_day is an integer defining the day of the week trips are
#'   included for. Note that this denotes the day the trip started, which is not
#'   necessarily the day it ends or when it will be within the modeled area.
#'   However, the latter is assumed to average out, obviating the need to
#'   explicitly represent trip duration in the model. The default value is 4,
#'   corresponding to Wednesday.
#' @param save_to File name for saving the daily interregional truck trips
#'   created by this program.
#'
#' @details This function samples weekly, and then daily, discrete truck trips
#'   based upon the number of annual truckload equivalents calculated for (FAF)
#'   or coded on (Transearch) each commodity flow record. The user can specify
#'   the week of the year and day of the week, although neither vary except
#'   randomly at the present. The user can also scale the annual trips to
#'   account for multiple stops per interregional trip and the fact that some
#'   (many?) FAF truck flows are longer than one day in duration. The resulting
#'   trip records have the FAF values appended, and the value and tons for each
#'   truck are split among all of the annual trucks.
#'
#' @export
#' @examples
#' daily_trucks <- sample_faf_daily_trucks(annual_trucks)  # Accept defaults
#' daily_trucks <- sample_faf_daily_trucks(annual_truckloads,
#'  scaling_factor = 2.41, target_week = 24, save_to = "daily_trucks.csv")


sample_faf_daily_trucks <- function(annual_truckload_equivalents,
  scaling_factor = 1.0, target_week = 19, target_day = 4, save_to = NULL) {

  # If we are scaling the external trips then apply the factor to affected trips
  if (scaling_factor != 1.0) {
    print(paste("Scaling factor to correct FAF external equivalencies =",
      scaling_factor), quote=FALSE)
    # TO-DO: Decide if and how to scale inbound and outbound (but not internal)
    # flows
  }

  # Tell us what our targets are (mostly for debugging and development use)
  annual_trucks <- sum(annual_truckload_equivalents$annual_trucks)
  weekly_target <- round(annual_trucks / 52, 0)
  daily_target <- round(weekly_target/7.0, 0)
  print(paste("Annual trucks = ", annual_trucks, "targets: weekly = ",
    weekly_target, "daily = ", daily_target), quote = FALSE)

  # Build a function to sample the week and day for each annual truck trip.
  # Trips occurring on the sampled week and day are returned as fully-attributed
  # daily truck trip data table. This is pretty time-consuming if monolithic,
  # so using with parallel() or similar is a practical necessity.
  build_daily_trip_records <- function(replicant, replicantID) {
    # Grab the probabilities associated with each trip. We might assume that
    # truck trips are evenly spaced out throughout the year, so if there are
    # fewer annual trips than weeks sample week without replacement.
    n_trucks <- replicant$annual_trucks
    if (n_trucks < 52) {
      replacement_key <- FALSE
    } else {
      replacement_key <- TRUE
    }
    draws <- dplyr::data_frame(
      week = sample(1:52, n_trucks, replace=replacement_key),
      day = sample(1:7, n_trucks, replace=TRUE)
    )

    # Do any of the trips occur within our target week and day? If so append
    # the important details from the replicant record. Note that the reporting
    # will not show up on the screen when running in parallel, but will turn up
    # in the logfile.
    keep <- dplyr::filter(draws, week == target_week, day == target_day)
    d_trucks <- nrow(keep)
    if (d_trucks > 0) {
      keep <- keep %>%
        dplyr::mutate(truckID = seq(1, d_trucks), status = replicant$status,
          value = round(replicant$value/n_trucks, 0),
          tons = round(replicant$tons/n_trucks, 1)) %>%
        dplyr::select(-week, -day) %>%
        dplyr::full_join(replicant, by = "status") %>%
        dplyr::select(-zed, -value, -tons, -ton_miles, -annual_trucks)
      print(paste("replicant", replicantID, ": weekly trucks =", weekly_trucks,
        "daily trucks=", nrow(keep)), quote = FALSE)
    } else {
      keep <- dplyr::data_frame()   # Delete the contents if no trucks generated
      print(paste("replicant", replicantID, ":", n_trucks,
        "annual trucks, but no weekly trips"), quote = FALSE)
    }
    keep
  }

  # Build a database of daily truck trips, how that we know how many truck trips
  # are associated with each annual truckload equivalent record.
  simulation_start <- proc.time()
  results <- foreach(i=1:nrow(annual_truckload_equivalents),
    .packages=c("dplyr")) %dopar%
    build_daily_trip_records(annual_truckload_equivalents[i,], i)
  daily_trips <- dplyr::bind_rows(results)
  simulation_stop <- proc.time()
  elapsed_seconds <- round((simulation_stop - simulation_start)[["elapsed"]], 1)
  print(paste("Simulation time=", elapsed_seconds, " seconds"), quote = FALSE)
  convergence <- round((nrow(daily_trips)/daily_target), 3)
  print(paste("Daily trucks: target =", daily_target, "generated =",
    nrow(daily_trips), "convergence =", convergence), quote = FALSE)

  # Return the result
  daily_trips
}
