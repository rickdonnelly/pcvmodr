#' Sample daily FAF trucks from annual truckload estimates
#'
#' @param annual_truckloads Data frame containing FAF flow records with 
#'   estimates of annaul truckload equivalents appended.
#' @param target.week Integer value specifying the week of the year the
#'   simulation should sample for (defaults to 24, typically occurs in
#'   April)
#' @param target.day Integer value specifying the day of the week trips
#'   should be sampled for (defaults to 4, which corresponds to 
#'   Wednesday in the target.week)
#'
#' @details This function creates discrete truck trip records from annual
#'   truckloads estimated eaelier, randomly assigns a travel week and day, and
#'   then saves those records that correspond to the user-specified simulation
#'   week and day. The user can specify observed flow distributions by week
#'   and day if desired, or sample from uniform distribution. The resulting
#'   trip records are for individual trucks, and include the FAF attributes
#'   associated with them. Note that the value and tonnage estimates have been
#'   split evenly across each of the trucks associatd with a given FAF flow
#'   record.
#'
#' @export
#' @examples
#' daily_trucks <- sample_faf4_daily_trucks(faf_flows)


sample_faf4_daily_trucks <- function(annual_truckloads, target.week = 24,
  target.day = 4) {
  # Use tidyr's uncount function to expand flow records to discrete truck trips,
  # and then add info we need to each trip record.
  discrete_trucks <- annual_truckloads %>%
    # Split the tonnage and value over the number of annual trucks generated, 
    # and then split flow records into discrete annual trucks
    dplyr::mutate(tons = tons / annual_trucks, value = value / annual_trucks) %>%
    tidyr::uncount(annual_trucks)
  
  # Sample the week and day for each discrete truck
  discrete_trucks$week <- sample(1:52, size = nrow(discrete_trucks), 
    replace = TRUE)
  discrete_trucks$day <- sample(1:7, size = nrow(discrete_trucks),
    replace = TRUE)
    
  # Save records that correspond to the specified week and day
  daily_trucks <- dplyr::filter(discrete_trucks, week == target.week,
    day == target.day)
  print(paste(nrow(daily_trucks), "daily truck records sampled from",
    sum(annual_truckloads$annual_trucks, na.rm = TRUE), "annual trucks"), 
    quote = FALSE)
  return(daily_trucks)
}