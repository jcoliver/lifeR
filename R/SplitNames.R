#' Split vector of names column into scientific and common
#'
#' @param x vector of species names, in the format "Common Name - Scientific
#' name"
#' @param delim character separator
#'
#' @details Names from eBird come in a single column as: "Snow Goose - Anser
#' caerulescens". This function provides a means of separating the common (Snow
#' Goose) from scientific (Anser caerulescens) into two separate columns.
#'
#' @return data.frame of two columns, \code{Common} and \code{Scientific}
#'
#' @export
SplitNames <- function(x, delim = " - ") {
  name.pairs <- strsplit(x = as.character(x), split = delim)
  common <- sapply(name.pairs, "[[", 1)
  scientific <- sapply(name.pairs, "[[", 2)
  data <- data.frame(Common = common,
                     Scientific = scientific)
  return(data)
}
