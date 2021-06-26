#' Drops any species with given patterns in name
#'
#' @param data a data.frame with observations, likely the object from a call to
#' \code{RecentNearby}
#' @param patterns character vector of patterns in \code{colname} column to
#' omit
#' @param colname character vector indicating column with names to search for
#' \code{patterns}
#'
#' @details This utility function provides a means of omitting observations of
#' "other taxa" such as domestics, hybrids, and "sp." observations. If
#' \code{patterns = NULL}, no rows will be dropped.
#'
#' @return Copy of \code{data} with any rows that has values in \code{colname}
#' that match values in \code{patterns}
#'
#' @export
DropPatterns <- function(data,
                         patterns = c("sp.", "/", "Domestic type", "hybrid"),
                         colname = "comName"){

  # Make sure there are patterns to match
  if (!is.null(patterns)) {
    if (!(colname %in% colnames(data))) {
      warning(paste0("No column named ", colname, " in data passed to DropPatterns; all rows returned"))
    }
    # Find indicies of rows with matching names
    to_drop <- grep(x = data[, colname],
                    pattern = paste0(patterns, collapse = "|"))
    if (length(to_drop) > 0) {
      data <- data[-to_drop, ]
    }
  }
  return(data)
}
