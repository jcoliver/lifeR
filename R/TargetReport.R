#' Create report for sites to target
#'
#' @param centers numeric vector or matrix of latitude and longitude
#' coordinates; vector should be of length 2, while matrix should have two
#' columns
#' @param key_file path to file with eBird API key
#' @param list_file path to file of user's observations; this should be a csv
#' file of observations downloaded from your My eBird page at
#' \url{https://ebird.org/myebird}
#' @param center_names optional character vector of names to use for each pair
#' of latitude and longitude coordinates in \code{centers}
#' @param max_sites integer maximum number of sites to return for each pair of
#' coordinates defined in \code{centers}
#' @param dist numeric radius (in kilometers) of area from each point defined
#' by coordinates in \code{centers} from which to return recent observations
#' @param back integer number of days back to search for observations
#' @param hotspot logical indicating whether or not to restrict results to
#' hotspot locations
#' @param include_provisional logical indicating whether not to include
#' observations which have not yet been reviewed
#' @param max_tries integer maximum number of query attempts to try (only for
#' expert use)
#' @param timeout_sec integer time to allow before query is aborted (only for
#' expert use)
#' @param verbose logical determining whether or not to print messages during
#' queries
#' @param drop_patterns character vector of patterns in species' names to
#' exclude certain species from consideration, such as domesticated species,
#' hybrids, and observations not identified to species level (e.g.
#' "Toxostoma sp.")
#'
#' @details The function uses the eBird API (see \url{https://documenter.getpostman.com/view/664302/S1ENwy59})
#' to build the target report. Queries to the eBird API require a user key; more
#' information on obtaining a key can be found at the eBird API documentation.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr bind_cols
#' @export
TargetReport <- function(centers,
                         key_file,
                         list_file, # TODO: Maybe make this null and add functionality
                         center_names = NULL,
                         max_sites = 5,
                         dist = 50,
                         back = 4,
                         hotspot = TRUE,
                         include_provisional = FALSE,
                         max_tries = 5,
                         timeout_sec = 30,
                         verbose = TRUE, # TODO: default TRUE?
                         drop_patterns = c("sp.", "/", "Domestic type", "hybrid")) {

  # TODO: consider how we will iterate. Will need to create a list (or data
  # frame) that has coordinate pairs AND center names (if they exist).

  # TODO: Need to be explicit in documentation about what order latitude and
  # longitude appear and if named elements or column names is supported.
  # Currently assume first element/column is latitude and second element is
  # longitude

  # centers will need to ultimately be a list, but start by making sure it is
  # a matrix, doing transformation of data frame or vector as appropriate
  if (is.data.frame(centers)) {
    centers <- as.matrix(centers)
  } else if (is.vector(centers)) {
    if (length(centers) %% 2 == 0) {
      centers <- matrix(data = centers, ncol = 2)
    } else {
      stop("TargetReport passed odd-length centers vector")
    }
  }
  if (ncol(centers) != 2) {
    stop("TargetReport requires centers matrix with two columns")
  }

  # Make sure these are numbers
  if (typeof(centers) != "double") {
    stop("TargetReport requires centers data that are type double")
  }

  # Add in names if user passed those along; need to make sure they are the
  # right length. If not, message user and proceed as if user had not provided
  # names
  user_supplied_names <- FALSE
  if (!is.null(center_names)) {
    if (length(center_names) == nrow(centers)) {
      user_supplied_names <- TRUE
    } else {
      message("Number of center names does not match number of centers passed to TargetReport; names will be auto-generated")
    }
  }
  if (!user_supplied_names) {
    center_names <- paste("Center", 1:nrow(centers))
  }
  centers_df <- data.frame(centers, center_names)
  colnames(centers_df) <- c("lat", "lng", "name")

  # Convert centers to a list for ease of iteration. Each element is a one-row
  # data frame
  centers_list <- split(x = centers_df,
                        f = seq(nrow(centers_df)))

  # key_file
  if (!file.exists(key_file)) {
    stop(paste0("Could not find key_file ", key_file, " for TargetReport"))
  }

  key <- scan(file = key_file)

  # list_file (may be optional later on)
  if (!file.exists(list_file)) {
    stop(paste0("Could not find list_file ", list_file, " for TargetReport"))
  }
  # Read in user's list.
  list_user <- readr::read_csv(file = list_file)

  # TODO: Could be defensive here and check for Species column

  # SplitNames
  list_user <- dplyr::bind_cols(list_user,
                                SplitNames(list_user$Species))

  # Something to hold data for each center...could just append to centers_list?

  # The below needs to run for EACH center
  # Since we are updating the centers_list object, we need the indexes
  for (i in 1:length(centers_list)) {
    center <- centers_list[[i]]

    # RecentNearby
    recent_obs <- RecentNearby(key = key,
                               lat = center$lat,
                               lng = center$lng,
                               dist = dist,
                               back = back,
                               hotspot = hotspot,
                               include_provisional = include_provisional,
                               max_tries = max_tries,
                               timeout_sec = timeout_sec,
                               verbose = verbose)

    # DropPatterns

    # do set difference between user's list and result of RecentNearby post-drop

    # For each species remaining, run
    # RecentNearbySpecies
    # combine all this stuff, identify the targets, do some tidyverse here before
    # sending to Target report; will probably need to just create a list (or
    # fancier object) with name, lat/long, results of set diff

  }
  # end iteration over each center

  # Using template, pass info to an RMarkdown template file

  # Will need to grab rmarkdown template that lives in inst/rmd like this:
  # report_template <- system.file("rmd", "Targets.Rmd", package = "lifeR")
  # See https://stackoverflow.com/questions/30377213/how-to-include-rmarkdown-file-in-r-package
  # and https://r-pkgs.org/inst.html
  # Parameters can be passed through params argument of rmarkdown::render
  # See https://rmarkdown.rstudio.com/lesson-6.html and
  # https://bookdown.org/yihui/rmarkdown/params-knit.html
  # rmarkdown::render(input = report_template, output_format = "html")
  # Parameters passed to params in rmarkdown::render MUST be declared in the
  # YAML header. Parameters can include complex classes such as list.
  # Remember to refer to parameters as part of the params list object, e.g.
  # params$param_one, params$param_two




}
