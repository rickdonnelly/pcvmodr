#' Context-sensitive file reader
#'
#' @param filename Fully qualified path and filename of data file to be read.
#'   Both compresed and uncompressed data can be read, although it is assumed
#'   that the underlying data are in comma-separated value (CSV) of feather
#'   formats. The type of file is inferred from the file extension.
#' @param format String describing the format of the file, used in cases where
#'   the file encoding cannot be inferred from the file extension(s). 
#'   Defaults to "unknown".
#'
#' @details This function is a general purpose file reader for data in comma-
#'   separated value (CSV) format. The goal is for the user to be able to
#'   specify the filename containing the data without having to worry about 
#'   which reader to use or handle different behaviors of specific readers.
#'   If the file extension is supported "it just works" without casting or
#'   wrapping the reader. In current version it handles feather or uncompressed
#'   CSV files, as well as those with gzip, zip, or xz compression. The data
#'   are returned in a data frame or tibble (depending upon the reader).
#'
#' @export
#' @examples
#' trip_list <- read_file("combined_trips.csv.gz")
#' trip list <- read_file("https:://faf.fhwa.gov/faf44.zip", format = "csv")


read_file <- function(filename, format = "unknown") {
  # Show us key file attributes after determining if it is remote or local, and
  # if it really exists
  read_start <- proc.time()
  if (startsWith(filename, "http:") | startsWith(filename, "https:") |
      startsWith(filename, "ftp:")) {
    print(filename, quote = FALSE)  # No other details available to us
  } else {
    print(paste0(filename, ": modified=", file.info(filename)[, "mtime"],
      " size=", file.info(filename)[, "size"]), quote = FALSE)
  }
  
  # Let's handle each file type we support in turn rather than doing something
  # elegant but hard
  munged <- tolower(filename)  # So we don't get tripped up by case
  if (grepl(".csv", munged) | tolower(format) == "csv") {
    # The nice thing about readr::read_csv() is that it uncompresses supported
    # formats on the fly.
    contents <- tryCatch(
      { 
        readr::read_csv(filename)
      }, 
      error = function(cond) {
        stop(paste("Cannot read", basename(filename), "(broken link?)"))
      },
      warning = function(cond) {
        # The not so nice thing about readr::read_csv() is that bugs sometimes
        # creep into production code. If so catch it and use legacy reader, but
        # only if not a URL
        warning("readr:read_csv() failed, attempting read.table()...",
          immediate. = TRUE)
        read.table(filename, header = TRUE, sep = ',', as.is = TRUE)
      }
    )  # end tryCatch
  } else if (endsWith(filename, ".feather") | tolower(format) == "feather") {
    # We use feather to temporarily store data frames that we need to push to
    # disk
    contents <- feather::read_feather(filename)
  } else if (endsWith(filename, ".feather.xz")) {
    # In rare cases we'll transfer feathers from other systems in compressed 
    # format, so handle that with different reader, as read_feather() doesn't 
    # automagically handle compressed files
    contents <- feather::read_feather(xzfile(filename))
  } else {
    # We're run out of possibilities
    stop(paste("Unrecognized format for", filename))
  }
  
  # Tell us what you found and exit stage right
  read_end <- proc.time()
  elapsed_seconds <- round((read_end - read_start)[["elapsed"]], 1)
  print(paste(nrow(contents), "records read from", basename(filename), "in", 
    elapsed_seconds, "seconds"), quote = FALSE)
  return(contents)
}
