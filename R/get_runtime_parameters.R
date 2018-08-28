#' Create database of runtime parameters
#'
#' @param filename Fully qualified path and filename of text file containing
#'   the user-defined runtime parameters.
#' @param tokens_sep Character value used in the parameters file to separate
#'   key names and their respective values (defaults to equal sign)
#' @param echo_parameters Boolean flag specifying whether the contents of the
#'   runtime parameters file should be printed to the console (default is TRUE)
#'
#' @details This function reads a text file containing runtime parameters that
#'   will be globally available during the simulation. Each key-value pair must
#'   be specified on a single line. The key names are normally nor quoted, and
#'   string values generally do not need to be enclosed in quotes unless the
#'   string includes whitespace. The key-value pairs are stored in a global 
#'   list named RTP. It is up to the user to define each of the required run-
#'   time parameters, which depend upon the implemented model structure.
#'
#' @export
#' @examples
#' RTP <<- get_runtime_parameters("~/Models/test/prelim_params.txt")


get_runtime_parameters <- function(filename, tokens_sep = "=",
  echo_parameters = TRUE) {
  # The simplest file out there would be a text file with one key-value pair
  # per record. Let's work with that.
  contents <- scan(file = filename, what = "list", sep = tokens_sep)
  pairs_found <- length(contents) / 2
  print(paste(pairs_found, "key-value pairs read from", filename), quote = FALSE)
  
  # Each token is now a list element. That's great, except that we potentially
  # have extra whitespace in the strings if the user included such in the file.
  # Fix that on the fly as we process each key-value pair.
  RTP <<- list()   # Set as global list
  for (i in seq(1, length(contents), 2)) {
    # Extract the key and its associated value
    key <- stringr::str_trim(contents[i])
    value <- stringr::str_trim(contents[i+1])
    
    # If the token appears to be numeric make it so
    suppressWarnings(
      value <- ifelse(!is.na(as.numeric(value)), as.numeric(value), value)
    )
    
    # Add them to the list
    RTP[[key]] <- value
  }
  
  # The last part might be the hardest, where we echo the parameters we've
  # just read from the file
  if (echo_parameters == TRUE) {
    keys <- names(RTP)  # Print them in the order processed
    print(paste0("Run-time parameters read from ", filename, ":"), quote = FALSE)
    for (this_key in keys) {
      print(paste("  ", this_key, tokens_sep, RTP[[this_key]]), quote = FALSE)
    }
  }
  
  return(RTP)
}
