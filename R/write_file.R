#' Context-sensitive file writer
#'
#' @param df Data frame or tibble that will be written to the output file
#' @param filename Fully qualified path and filename that the data will be
#'   written to. This can simply be a filename if used in working directory
#'   or part of file.path() call.
#' @param overwrite Boolean variable specifying whether the target file 
#'   (specified in filename) should be overwritten if it already exists. The
#'   default is TRUE.
#' @param compress_wait Boolean variable specifying whether write_file should
#'   wait for external compression to be completed before returning control to
#'   this function. Applicable only if compression is implied by the filename
#'   extension, defaults to FALSE (i.e., the system allow this function to 
#'   proceed while compression continues in the background).
#' @param compress_threads Integer number of threads that xz compression will
#'   use. Only applicable to xz compression, and defaults to zero, which 
#'   uses all available cores on the computer.
#'
#' @details This function is a general purpose file writer for saving data
#'   frame to files in feather (binary) or comma-separated value (CSV) formats.
#'   The file extension is parsed to determine the type of file to be written,
#'   whether compressed or not, and type of compression desired. In its present
#'   form the program used maximum compression available, and is limited to
#'   gzip and xz compression utilities. Some parameters are provided to fine-
#'   tune control of compression, although the user should consider their 
#'   effect on runtime performance, especially if xz compression with all 
#'   cores is used.
#'
#' @export
#' @examples
#' write_file(trip_list, "./daily_trips.feather", overwrite = TRUE)
#' write_file(trip_list, "./daily_trips.csv.gz", compress_wait = TRUE)


write_file <- function(df, filename, overwrite = TRUE, compress_wait = FALSE,
  compress_threads = 0) {
  # If we have to wait for compression to finish we'll report the time required
  write_start <- proc.time()
  
  # If we cannot overwrite an existing instance stop in our tracks
  if (file.exists(filename) & overwrite == FALSE) {
    stop(paste(filename, "exists but overwrite set to", overwrite))
  }
  
  # We are assuming that a data frame or tibble is passed to the function, so we
  # only need to figure out what format it'll be stored in. But if the user has
  # implied the type of compression required we'll need to strip that from the
  # filename before writing it initially in uncompressed format.
  requested_compression <- NA
  compression_formats <- c("gz", "xz")   # Maybe other later, but these for now
  tokens <- unlist(stringr::str_split(filename, "[.]"))
  if (tokens[length(tokens)] %in% compression_formats) {
    requested_compression <- tokens[length(tokens)]
    filename <- gsub(paste0('.', requested_compression), "", filename)
  }
  
  # Now we're ready to write the uncompressed
  if (endsWith(filename, ".csv")) {
    readr::write_csv(df, filename)
  } else if (endsWith(filename, ".feather")) {
    feather::write_feather(df, filename) 
  } else {
    stop(paste("Unrecognized file format for", filename))
  }
  
  # Now apply the requested compression, if applicable. Use shQuote() to wrap
  # the filenames in case or their path contains spaces
  if (!is.na(requested_compression)) {
    overwrite <- ifelse(overwrite == TRUE, 'f', '')
    if (requested_compression == "gz") {
      system2("gzip", paste0("-9", overwrite, " ", shQuote(filename)),
        wait = compress_wait)
    } else if (requested_compression == "xz") {
      system2("xz", paste0("-9e", overwrite, " -T", compress_threads, " ",
        shQuote(filename)), wait = compress_wait)
    }
  }
  
  
  # If we waited for compression to complete then report how long it took
  if (compress_wait == TRUE) {
    write_end <- proc.time()
    elapsed_seconds <- round((write_end - write_start)[["elapsed"]], 1)
    print(paste(nrow(df), "records written to", basename(filename), "in", 
      elapsed_seconds, "seconds"), quote = FALSE)
  }
  
  # Nothing to return except gratitude
}