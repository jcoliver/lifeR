#' Print method for recent_obs object
#'
#' @param obj an object of class "recent_obs"
print.recent_obs <- function(obj) {
  cat("Results of eBird query\n", sep = "")
  for (i in 1:length(obj$query_params)) {
    param_name <- names(obj$query_params)[i]
    param_value <- obj$query_params[[i]]
    if (param_name == "species_code") {
      cat("\tSpecies '", param_value, "'\n", sep = "")
    } else if (param_name == "lat") {
      cat("\tLatitude: ", param_value, "\n", sep = "")
    } else if (param_name == "lng") {
      cat("\tLongitude: ", param_value, "\n", sep = "")
    } else if (param_name == "dist") {
      cat("\tDistance from center: ", param_value, " km\n", sep = "")
    } else if (param_name == "back") {
      cat("\tDays back: ", param_value, "\n", sep = "")
    } else if (param_name == "hotspot") {
      cat("\tRestricted to hotspots: ", param_value, "\n", sep = "")
    } else if (param_name == "include_provisional") {
      if (param_value) {
        cat("\tIncludes observations that have not been reviewed\n", sep = "")
      } else {
        cat("\tExcludes observations that have not been reviewed\n", sep = "")
      }
    } else {
      cat("\t", param_name, ": ", param_value, "\n", sep = "")
    }
  }
  if (is.null(obj$obs)) {
    cat("Zero observations returned from query", sep = "")
  } else {
    cat(nrow(obj$obs), " total observations", sep = "")
  }
}
