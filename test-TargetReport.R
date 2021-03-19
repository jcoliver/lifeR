# testing TargetReport
# Jeffrey C. Oliver
# jcoliver@email.arizona.edu
# 2021-02-11

rm(list = ls())

################################################################################

library(lifeR)

locs <- c(32.2394119, -110.9387534)
loc_names <- "CenterOne"
# locs <- matrix(data = c(45, -109, 39, -111), nrow = 2, byrow = TRUE)
# loc_names <- c("CenterOne", "CenterTwo")
keyfile <- "ebird-key.txt"
listfile <- "~/Documents/Personal/ebird-targets/data/year-lists/ebird_world_year_2021_list.csv"

lifeR::TargetReport(centers = locs,
                    list_file = listfile,
                    center_names = loc_names,
                    key_file = keyfile)

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
