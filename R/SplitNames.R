#' Split vector of names into two-column data frame
#'
#' @param x Vector of species names, in the format "Common Name - Scientific
#' name".
#' @param delim Character separator that delimits common from scientific names.
#'
#' @details Names from eBird are returned in a single column as: 
#' "Snow Goose - Anser caerulescens". This function provides a means of 
#' separating the common name ("Snow Goose") from the scientific name ("Anser 
#' caerulescens") into two separate columns.
#'
#' @return A data.frame of two columns, \code{Common} and \code{Scientific}.
#'
#' @examples 
#' # Read in data downloaded from eBird
#' user_file <- system.file("extdata", "example-list.csv", package = "lifeR")
#' user_list <- read.csv(file = user_file)
#' # Retrieve a two-column data frame with common names and scientific names
#' species_seen <- SplitNames(x = user_list$Species)
#' # If only common names are required, refer to \code{Common} column
#' species_seen <- SplitNames(x = user_list$Species)$Common
#' 
#' @importFrom stringr str_split_fixed str_trim
#' 
#' @export
SplitNames <- function(x, delim = " - ") {
  # The default of delimiter of a hyphen enclosed by a pair of whitespaces is
  # required given eBird's returned format AND the fact that we do not want to
  # split hyphenated names, e.g. Blue-gray Gnatcatcher
  name_pairs <- stringr::str_split_fixed(string = as.character(x),
                                         pattern = delim,
                                         n = 2)
  data <- data.frame(Common = stringr::str_trim(name_pairs[, 1]),
                     Scientific = stringr::str_trim(name_pairs[, 2]))
  return(data)
}
