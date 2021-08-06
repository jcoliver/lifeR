#' Create map of sites identified with highest number of missing species
#' 
#' 
#' @importFrom ggmap get_map ggmap
#' @export
MapSites <- function(sites) {

  # values coming in from data frame 'sites':
  # locName, num_new, lat, lng

  # Make a shorter version of the location name for map
  sites$print_name <- substr(x = sites$locName,
                             start = 1, 
                             stop = 12)
  # Add in the number of missing species
  sites$print_name <- paste0(sites$print_name, " (", sites$num_new, ")")

  # Determine bounds of map; want to think about centers where international 
  # date line might get involved...
  

  
  # From Target-report.Rmd below here
  map.data <- lapply(target.list, "[", c("site.name", "num.species", "coordinates"))
  map.data <- map.data[1:num.to.print]
  map.df <- data.frame(site.name = as.character(names(map.data)),
                       num.species = as.vector(unlist(sapply(map.data, "[", "num.species"))),
                       latitude = t(sapply(map.data, "[[", "coordinates"))[, "lat"],
                       longitude = t(sapply(map.data, "[[", "coordinates"))[, "lng"])
  # Oddly, this does *not* work if we try to do it when initializing df...
  map.df$site.name <- as.character(map.df$site.name)
  
  # Order by decreasing # species (could have been done already)
  map.df <- map.df[order(map.df$num.species, decreasing = TRUE), ]
  rownames(map.df) <- NULL
  
  # Make a column with abbreviated name and unseen species count
  map.df$print.name <- paste0(rownames(map.df), " - ", 
                              substr(x = map.df$site.name,
                                     start = 1,
                                     stop =  10))
  map.df$print.name <- paste0(map.df$print.name, " (", map.df$num.species, ")")
  
  map.bounds <- c(floor(min(c(map.df$longitude, center.lng))),
                  floor(min(c(map.df$latitude, center.lat))),
                  ceiling(max(c(map.df$longitude, center.lng))),
                  ceiling(max(c(map.df$latitude, center.lat))))
  
  # TODO: Move mapping to functions file and place inside a conditional in case
  # retrival via ggmap::get_map times out
  center.map <- get_map(location = map.bounds, 
                        source = "stamen", 
                        maptype = "terrain")
  
  targets.map <- ggmap(center.map) +
    geom_point(data = map.df,
               mapping = aes(x = longitude, y = latitude, color = print.name),
               size = 3) +
    scale_color_brewer(name = "Site", palette = "Dark2") +
    theme_bw() +
    xlab(label = "Longitude") +
    ylab(label = "Latitude")
  
  print(targets.map)  
}