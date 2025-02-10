#' Drop species with given patterns in name
#'
#' @param data A data.frame with observations, likely the object from a call to
#' \code{RecentNearby}.
#' @param patterns Character vector of patterns in \code{colname} column to
#' omit; interprets regular expressions.
#' @param colname Character vector indicating column with names to search for
#' \code{patterns}.
#'
#' @details This utility function provides a means of omitting observations of
#' "other taxa" such as domestics, hybrids, and "sp." observations. If
#' \code{patterns = NULL}, no rows will be dropped.
#'
#' @return Copy of \code{data} with any rows that has values in \code{colname}
#' that match values in \code{patterns}.
#' 
#' @examples
#' df <- data.frame(comName = c("Mallard", "Mallard x Mexican Duck hybrid", "Verdin"),
#'                  date = c("2021-01-09", "2021-01-09", "2021-01-09"))
#' df <- DropPatterns(data = df)
#' 
#' @export
#' 
#' @keywords internal
DropPatterns <- function(data,
                         patterns = c("sp\\.", "\\/", "Domestic type", "hybrid"),
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