# Testing leaflet
# Jeff Oliver
# jcoliver@arizona.edu
# 2023-11-19

library(leaflet)

localities <- data.frame(locName = c("Sweetwater Wetlands", "Reid Park"),
                         locID = c("L208918", "L227274"), 
                         num_new = c(3, 5),
                         lat = c(32.279, 32.210), 
                         lng = c(-111.022, -110.924))
sites <- localities

# Add numbers to the site names for easier navigating in map
sites$locName <- paste0(rownames(sites), ". ", sites$locName)

# Make a shorter version of the location name for map
sites$print_name <- substr(x = sites$locName,
                           start = 1, 
                           stop = 16)
# Add in the number of new species
sites$print_name <- paste0(sites$print_name, " (", sites$num_new, ")")

# Convert print_name to a factor and level it here for proper ordering of 
# sites (only really necessary when number of sites is >= 10)
sites$print_name <- factor(x = sites$print_name,
                           levels = sites$print_name)

# Start by getting minimum bounds for points to be plotted
if (!is.null(center_lng) & !is.null(center_lat)) {
  map_bounds <- c("left" = min(c(sites$lng, center_lng)),
                  "bottom" = min(c(sites$lat, center_lat)),
                  "right" = max(c(sites$lng, center_lng)),
                  "top" = max(c(sites$lat, center_lat)))
} else {
  map_bounds <- c("left" = min(sites$lng),
                  "bottom" = min(sites$lat),
                  "right" = max(sites$lng),
                  "top" = max(sites$lat))
}

# Find original spans of map and calculate 5% for padding
lng_pad <- (map_bounds["right"] - map_bounds["left"]) * 0.05
lat_pad <- (map_bounds["top"] - map_bounds["bottom"]) * 0.05

# Update map_bounds with this 5% padding
map_bounds["left"] <- map_bounds["left"] - lng_pad
map_bounds["right"] <- map_bounds["right"] + lng_pad
map_bounds["bottom"] <- map_bounds["bottom"] - lat_pad
map_bounds["top"] <- map_bounds["top"] + lat_pad

# Now check the spans to see if map needs to be wider
lng_span <- map_bounds["right"] - map_bounds["left"]
lat_span <- map_bounds["top"] - map_bounds["bottom"]

# If taller than wide, add enough longitude to make it square
if (lat_span > lng_span) {
  lng_pad <- (lat_span - lng_span) / 2
  map_bounds["left"] <- map_bounds["left"] - lng_pad
  map_bounds["right"] <- map_bounds["right"] + lng_pad
}

names(map_bounds) <- NULL

center_lng <- -110.95
center_lat <- 32.23

# Note: fitBounds complains a bit with a named vector (like map_bounds), either
# drop the names or change it to a named list...
# Create base map
sites_map <- leaflet::leaflet(sites) %>%
  leaflet::fitBounds(map_bounds[1], map_bounds[2], 
            map_bounds[3], map_bounds[4]) %>% # xmin, ymin, xmax, ymax
  leaflet::addTiles()

print(sites_map)

# If the coordinates for the center are not NULL, add them as a point
if (!is.null(center_lat) & !is.null(center_lng)) {
  # Add center point; start by creating the icon
  center_icon <- leaflet::makeAwesomeIcon(icon = "home",
                                          library = "glyphicon",
                                          markerColor = "white",
                                          iconColor = "black",
                                          squareMarker = TRUE)
  # Add the icon to the map
  sites_map <- sites_map %>%
    leaflet::addAwesomeMarkers(lng = center_lng,
                               lat = center_lat,
                               icon = center_icon)
  print(sites_map)
}

# Create color palette for sites; Paired has a max of 12 and min of 3; asking 
# for max, then reducing down to actual number of sites (to avoid warning if 
# number of sites is < 3.
# site_colors <- brewer.pal(n = 12, name = "Paired")
# site_colors <- site_colors[1:nrow(sites)]

site_palette <- leaflet::colorFactor(palette = "Paired",
                                     domain = sites$print_name)

# Add points for sites
sites_map <- sites_map %>%
  leaflet::addCircleMarkers(radius = 8,
                            fillColor = ~site_palette(print_name),
                            fillOpacity = 1.0,
                            stroke = TRUE,
                            color = "#000000", # stroke
                            weight = 2.0,
                            opacity = 1.0)

print(sites_map)

# Add legend
sites_map <- sites_map %>%
  leaflet::addLegend(position = "bottomright", 
                     pal = site_palette, 
                     values = ~print_name,
                     title = "Top Sites",
                     opacity = 1)

print(sites_map)


# OLD

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
  
  # Need to color by site name, using print_name for legend
  sites_map <- ggmap::ggmap(ggmap = center_map) +
    ggplot2::geom_point(data = sites,
                        mapping = ggplot2::aes(x = .data$lng, 
                                               y = .data$lat, 
                                               fill = .data$print_name),
                        size = 3,
                        color = "black",
                        shape = 21) +
    ggplot2::scale_fill_brewer(name = "Site", palette = "Paired") +
    ggplot2::theme_minimal() +
    ggplot2::xlab(label = "Longitude") +
    ggplot2::ylab(label = "Latitude")
  
  # If the coordinates for the center are not NULL, add them as a point
  if (!is.null(center_lat) & !is.null(center_lng)) {
    center_df <- data.frame(lng = center_lng, lat = center_lat)
    sites_map <- sites_map +
      ggplot2::geom_point(data = center_df,
                          mapping = ggplot2::aes(x = .data$lng, 
                                                 y = .data$lat),
                          size = 4,
                          shape = 16, # circled colored white
                          color = "white") +
      ggplot2::geom_point(data = center_df,
                          mapping = ggplot2::aes(x = .data$lng, 
                                                 y = .data$lat),
                          size = 4,
                          shape = 10, # black circle outline with center plus
                          color = "black")
  }
}