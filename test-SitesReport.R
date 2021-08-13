# testing TargetReport
# Jeffrey C. Oliver
# jcoliver@email.arizona.edu
# 2021-02-11

rm(list = ls())

################################################################################

devtools::load_all()

# locs <- c(32.2394119, -110.9387534)
# loc_names <- "CenterOne"
# Three centers, the second one is unlikely to return any results
locs <- matrix(data = c(45.2, -109.1, 39.6, -111.1, 38.1, -121.9), 
               nrow = 3, 
               byrow = TRUE)
loc_names <- c("CenterOne", "CenterTwo", "CenterThree")
keyfile <- "ebird-key.txt"
listfile <- "~/Documents/Personal/ebird-targets/data/year-lists/ebird_world_year_2021_list.csv"

# Read in key from file
key <- scan(file = keyfile, what = "character")
# Read in file with species that have been seen
seen <- read.csv(file = listfile)
# Pull out the common name
my_species <- SplitNames(x = seen$Species)$Common

lifeR::SitesReport(centers = locs,
                   center_names = loc_names, 
                   ebird_key = key,
                   report_format = "html",
                   species_seen = my_species)

################################################################################
# testing out purrr::map

df <- data.frame(lon = c(-109, -110, -111),
                 lat = c(39, 42, 41),
                 names = c("C 1", "C 2", "C 3"))

centers_list <- split(x = df,
                      f = seq(nrow(df)))


print_stuff <- function(x) {
  print(paste0("Coordinates for ", x$names, ": ", x$lat, ", ", x$lon))
}

purrr::map(centers_list, print_stuff)
