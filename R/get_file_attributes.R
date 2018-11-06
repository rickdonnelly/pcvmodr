#' Return a string containing file attributes
#'
#' @param filename Fully qualified path and filename
#' @param records The number of records in the file can be passed to the
#'   function, which will echo this information
#'
#' @details This function returns a string that includes the filename, date last
#'   modified, size, and number of records.
#'
#' @export
#' @examples
#' annual_flows <- get_file_attributes("./flow_database.csv")


get_file_attributes <- function(filename, records) {
  # Create the string we'll use even if the user doesn't pass along the number
  # of records
  result <- paste0(filename, ": modified=", file.info(filename)[, "mtime"],
    " size=", file.info(filename)[, "size"])

  # If the user passes along the number of records we can report them as well
  # (we cannot report this ourself becauser the file might not be open)
  if (!is.null(records)) {
    result <- paste0(result, " records=", records)
  }
  return(result)
}
