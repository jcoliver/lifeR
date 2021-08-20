#' Print recent_obs object
#'
#' @param x An object of class \code{recent_obs}.
#' 
#' @export
#' 
#' @keywords internal
print.recent_obs <- function(x, ...) {
  cat("Results of eBird query:\n", sep = "")
  for (i in 1:length(x$query_params)) {
    param_name <- names(x$query_params)[i]
    param_value <- x$query_params[[i]]
    if (param_name == "species_code") {
      cat("Species '", param_value, "'\n", sep = "")
    } else if (param_name == "lat") {
      cat("Latitude: ", param_value, "\n", sep = "")
    } else if (param_name == "lng") {
      cat("Longitude: ", param_value, "\n", sep = "")
    } else if (param_name == "dist") {
      cat("Distance from center: ", param_value, " km\n", sep = "")
    } else if (param_name == "back") {
      cat("Days back: ", param_value, "\n", sep = "")
    } else if (param_name == "hotspot") {
      cat("Restricted to hotspots: ", param_value, "\n", sep = "")
    } else if (param_name == "include_provisional") {
      if (param_value) {
        cat("Includes observations that have not been reviewed\n", sep = "")
      } else {
        cat("Excludes observations that have not been reviewed\n", sep = "")
      }
    } else {
      cat("", param_name, ": ", param_value, "\n", sep = "")
    }
  }
  if (is.null(x$obs)) {
    cat("Zero observations returned from query", sep = "")
  } else {
    cat(nrow(x$obs), " total observations", sep = "")
  }
}
