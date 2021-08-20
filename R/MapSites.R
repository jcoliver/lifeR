#' Map of sites with highest number of missing species
#' 
#' @param sites A data.frame with at least the following columns:
#' \describe{
#'   \item{locName}{Name of the location.}
#'   \item{locId}{eBird identifier of the location.}
#'   \item{num_new}{Number of new species at site}
#'   \item{lat}{Numeric latitude in decimal degrees}
#'   \item{lng}{Numeric longitude in decimal degrees}
#' }
#' @param center_lng, center_lat Numeric decimal degrees longitude and latitude 
#' of the geographic center used for searching sites.
#' 
#' @details The function is primarily used by \code{SitesReport} via the 
#' template RMarkdown file used to build reports. It is not intended for 
#' standalone use.
#' 
#' @return A ggmap object.
#' 
#' @examples
#' \dontrun{
#'   # Create data frame with required columns
#'   localities <- data.frame(locName = c("Sweetwater Wetlands", "Reid Park"), 
#'   locID = c("L208918", "L227274"), num_new = c(3, 5), 
#'   lat = c(32.279, 32.210), lng = c(-111.022, -110.924))
#'   lifeR:::MapSites(sites = localities)
#' }
#' 
#' @import ggplot2
#' @importFrom ggmap get_map ggmap
#' @export
#' 
#' @keywords internal
MapSites <- function(sites, center_lng = NULL, center_lat = NULL) {
  # Add numbers to the site names for easier navigating in map
  sites$locName <- paste0(rownames(sites), ". ", sites$locName)
  
  # Make a shorter version of the location name for map
  sites$print_name <- substr(x = sites$locName,
                             start = 1, 
                             stop = 16)
  # Add in the number of missing species
  sites$print_name <- paste0(sites$print_name, " (", sites$num_new, ")")

  # Determine bounds of map; ignoring the problem that anti-meridian spanning 
  # boundary can introduce for now
  if (!is.null(center_lng) & !is.null(center_lat)) {
    map_bounds <- c(floor(min(c(sites$lng, center_lng))),
                    floor(min(c(sites$lat, center_lat))),
                    ceiling(max(c(sites$lng, center_lng))),
                    ceiling(max(c(sites$lat, center_lat))))
  } else {
    map_bounds <- c(floor(min(sites$lng)),
                    floor(min(sites$lat)),
                    ceiling(max(sites$lng)),
                    ceiling(max(sites$lat)))
  }

  # Ensure lat/lng are in bounds
  map_bounds <- CoordInBounds(x = map_bounds,
                              latitude = c(FALSE, TRUE, FALSE, TRUE))
  
  # Want to be sure stamen maps is responsive
  # First use ggmap::get_map to get URLs of all map tiles, check each of them 
  # for 200 status, then do query again as long as all the tiles are returning 
  # status 200
  map_urls <- ggmap::get_map(location = map_bounds,
                             source = "stamen",
                             maptype = "terrain",
                             urlonly = TRUE)
  # Run curl_fetch_memory on each tile URL
  test_tiles <- lapply(X = map_urls, FUN = curl::curl_fetch_memory)
  # Pull out status_code element from each of the test_tiles sub-lists
  statuses <- unlist(lapply(test_tiles, "[[", "status_code"))
  if (all(statuses == 200)) {
    # All tiles look good, proceed with mapping
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
  } else { # One or more tiles wasn't returned
    # Warn user about problems with map and return NULL
    warning("The map server did not respond to request, maps may not be drawn.")
    return(NULL)
  }
}

#' Determine if coordinate is in bounds, and if not, return closed valid value
#' 
#' @param x numeric decimal degree, longitude or latitude
#' @param direction logical indicating whether \code{x} is latitude or not 
#' (i.e. is longitude)
#' 
#' @details A helper function designed to keep map bounds from using invalid 
#' coordinates (latitudes outside of -90 and 90; longitudes outside of -180 and 
#' 180). Will round values to nearest valid value. A more feature-rich approach 
#' could treat longitudes a little more carefully, where values outside the 
#' range are updated with the antimeridian in mind. For example, a longitude of 
#' 182 would become -178. However, drawing polygons that include the 
#' antimeridian are a nightmare, and since that is what will be done with this 
#' helper function, it will simply round down to 180.
#' 
#' @return a copy of the original numeric vector of decimal degrees, \code{x}, 
#' with any invalid values (i.e. a latitude > 90) corrected to their closest 
#' valid value
#' 
#' @examples 
#' \dontrun{
#'   # Vector of decimal degrees
#'   vals <- c(78, 93, -112, 184)
#'   # Vector indicating latitude or longitude
#'   dirs <- c(TRUE, TRUE, FALSE, FALSE)
#'   lifeR:::CoordInBounds(x = vals, latitude = dirs)
#'   # [1] 78   90 -112  180
#' }
#' 
#' @keywords internal
CoordInBounds <- function(x, latitude) {
  # Start by fixing any outside of -180 to 180 (which takes care of longitudes)
  x[x < -180] <- -180
  x[x > 180] <- 180
  # Now deal with any remaining latitudes that are out of bounds
  x[latitude & x < -90] <- -90
  x[latitude & x > 90] <- 90
  return(x)
}