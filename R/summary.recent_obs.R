#' Summary of recent_obs object
#'
#' @param object An object of class "recent_obs".
#' 
#' @export
#' 
#' @keywords internal
summary.recent_obs <- function(object, ...) {
  print(object)
  if (object$query_type == "nearby observations" & !is.null(object$obs)) {
    # if multiple species, print how many species
    num_species <- length(unique(object$obs$sciName))
  }
}
