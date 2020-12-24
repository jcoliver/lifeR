#' Split vector of names into two column data frame
#'
#' @param x vector of species names, in the format "Common Name - Scientific
#' name"
#' @param delim character separator
#'
#' @details Names from eBird come in a single column as: "Snow Goose - Anser
#' caerulescens". This function provides a means of separating the common name
#' (Snow Goose) from the scientific name (Anser caerulescens) into two separate
#' columns.
#'
#' @return data.frame of two columns, \code{Common} and \code{Scientific}
#'
#' @importFrom stringr str_split_fixed
#' @export
SplitNames <- function(x, delim = " - ") {
  name_pairs <- stringr::str_split_fixed(string = as.character(x),
                                         pattern = delim,
                                         n = 2)
  data <- data.frame(Common = name_pairs[, 1],
                     Scientific = name_pairs[, 2])
  return(data)
}
