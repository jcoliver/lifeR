#' Create map of sites identified with highest number of missing species
#' 
#' @param sites data frame with at least the following columns:
#' \describe{
#'   \item{locName}{character name of site}
#'   \item{num_new}{integer number of new species at site}
#'   \item{lat}{double latitude in decimal degrees}
#'   \item{lng}{double longitude in decimal degrees}
#' }
#'  
#' @import ggplot2
#' @importFrom ggmap get_map ggmap
MapSites <- function(sites) {

  # values coming in from data frame 'sites':
  # locName, locId, num_new, lat, lng

  # Add numbers for easier navigating
  sites$locName <- paste0(rownames(sites), ". ", sites$locName)
  
  # Make a shorter version of the location name for map
  sites$print_name <- substr(x = sites$locName,
                             start = 1, 
                             stop = 16)
  # Add in the number of missing species
  sites$print_name <- paste0(sites$print_name, " (", sites$num_new, ")")

  # Determine bounds of map; want to think about centers where international 
  # date line might get involved...
  # Only need to worry about this when longitudes are really around (and on 
  # both sides) if International Date Line
  # Buuuuut, stamen maps (and maybe ggmap) can't handle this right now.
  # if (any(sites$lng > 90) & any(sites$lng < -90)) {
  #   # First subtract 360 from any positive longitudes, to make them more 
  #   # negative
  #   sites$lng[sites$lng > 0] <- sites$lng[sites$lng > 0] - 360
  #   # Now find westernmost longitude
  #   left <- min(sites$lng) + 360
  #   right <- max(sites$lng)
  #   sites$lng[sites$lng < -180] <- sites$lng[sites$lng < -180] + 360
  # } else {
  #   # Normal math works, no temporary transformation necessary
  #   left <- min(sites$lng)
  #   right <- max(sites$lng)
  # }
  
  # Round up/down as appropriate to reduce the chance that points lie on map
  # edges
  left <- floor(min(sites$lng))
  right <- ceiling(max(sites$lng))
  top <- ceiling(max(sites$lat))
  bottom <- floor(min(sites$lat))
  
  map_bounds <- c(left, bottom, right, top)
  
  # TODO: Need catch to deal with time out of ggmap::get_map?
  center_map <- ggmap::get_map(location = map_bounds, 
                               source = "stamen", 
                               maptype = "terrain")

  # Need to color by site name (locName), but use print_name for legend
  sites_map <- ggmap::ggmap(ggmap = center_map) +
    ggplot2::geom_point(data = sites,
               mapping = ggplot2::aes(x = .data$lng, 
                                      y = .data$lat, 
                                      color = .data$print_name),
               size = 3) +
    ggplot2::scale_color_brewer(name = "Site", palette = "Dark2") +
    ggplot2::theme_minimal() +
    ggplot2::xlab(label = "Longitude") +
    ggplot2::ylab(label = "Latitude")

  return(sites_map)
}