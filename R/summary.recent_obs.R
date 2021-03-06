#' Summary method for recent_obs object
#'
#' @param obj an object of class "recent_obs"
summary.recent_obs <- function(obj) {
  print(obj)
  # regardless, print how many localities
  # regardless, print number of observations
  if (obj$query_type == "nearby observations" & !is.null(obj$obs)) {
    # if multiple species, print how many species
    num_species <- length(unique(obj$obs$sciName))
  }
}
