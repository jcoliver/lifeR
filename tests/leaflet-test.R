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

map_bounds <- numeric(4)
names(map_bounds) <- c("left", "right", "bottom", "top")
map_bounds["left"] <- -111
map_bounds["right"] <- -110
map_bounds["bottom"] <- 32
map_bounds["top"] <- 32.5
names(map_bounds) <- NULL

# Note: fitBounds complains a bit with a named vector (like map_bounds), either
# drop the names or change it to a named list...
# Create base map
sites_map <- leaflet::leaflet(localities) %>%
  leaflet::fitBounds(map_bounds[1], map_bounds[3], 
            map_bounds[2], map_bounds[4]) %>% # xmin, ymin, xmax, ymax
  leaflet::addTiles()

print(sites_map)

# Add center point
ci <- leaflet::makeAwesomeIcon(icon = "home",
                               library = "glyphicon",
                               markerColor = "white",
                               iconColor = "black",
                               squareMarker = TRUE)

sites_map <- sites_map %>%
  # leaflet::addCircleMarkers(lng = -110.95,
  #                           lat = 32.23,
  #                           radius = 8,
  #                           fillColor = "white", 
  #                           fillOpacity = 0.75,
  #                           stroke = TRUE,
  #                           color = "#000000",
  #                           weight = 1.0,
  #                           opacity = 0.75)
  leaflet::addAwesomeMarkers(lng = -110.95,
                             lat = 32.23,
                             icon = ci)

print(sites_map)

# Add points for sites
sites_map <- sites_map %>%
  leaflet::addCircleMarkers(radius = 8,
                            fillColor = "green",
                            fillOpacity = 1.0,
                            stroke = TRUE,
                            color = "#000000", # stroke
                            weight = 2.0,
                            opacity = 1.0)

print(sites_map)
