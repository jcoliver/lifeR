#' Create report for sites with most unseen species
#'
#' @param centers numeric vector or matrix of latitude and longitude
#' coordinates; vector should be of length 2, e.g. 
#' \code{c(latitude, longitude)}, while matrix should have two columns (first 
#' column is latitude, second column is longitude)
#' @param key_file path to file with eBird API key
#' @param list_file path to file of user's observations; this should be a csv
#' file of observations downloaded from your My eBird page at
#' \url{https://ebird.org/myebird}
#' @param center_names optional character vector of names to use for each pair
#' of latitude and longitude coordinates in \code{centers}
#' @param report_filename name of output file
#' @param report_dir destination folder for the output file
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
#' to build the report. Queries to the eBird API require a user key; more
#' information on obtaining a key can be found at the eBird API documentation.
#'
#' @importFrom readr read_csv cols
#' @import dplyr
#' @importFrom rmarkdown render
#' @export
SitesReport <- function(centers,
                         key_file, # TODO: Make flexible so user could pass key string instead?
                         list_file, # TODO: Maybe make this null and add functionality
                         center_names = NULL,
                         report_filename = "Goals-Report", #TODO: suffix?
                         report_dir = getwd(),
                         max_sites = 5,
                         dist = 50,
                         back = 4,
                         hotspot = TRUE,
                         include_provisional = FALSE,
                         max_tries = 5,
                         timeout_sec = 30,
                         verbose = TRUE, # TODO: default should be FALSE?
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
      stop("SitesReport passed odd-length centers vector")
    }
  }
  if (ncol(centers) != 2) {
    stop("SitesReport requires centers matrix with two columns")
  }

  # Make sure these coordinates are numbers
  if (typeof(centers) != "double") {
    stop("SitesReport requires centers data that are type double")
  }

  # Add in names if user passed those along; need to make sure they are the
  # right length. If not, message user and proceed as if user had not provided
  # names
  user_supplied_names <- FALSE
  if (!is.null(center_names)) {
    if (length(center_names) == nrow(centers)) {
      user_supplied_names <- TRUE
    } else {
      message("Number of center names does not match number of centers passed to SitesReport; names will be auto-generated")
    }
  }
  if (!user_supplied_names) {
    center_names <- paste("Center", 1:nrow(centers))
  }
  centers_df <- data.frame(centers, center_names)
  colnames(centers_df) <- c("lat", "lng", "name")

  # Convert centers to a list for ease of iteration. Each element is a one-row
  # data frame with lat, lng, and name
  centers_list <- split(x = centers_df,
                        f = seq(nrow(centers_df)))

  # TODO: Delete this save when done testing
  saveRDS(centers_list, file = "centers-list.Rds")
  
  
  # Reality checks to ensure necessary files exist
  # key_file
  if (!file.exists(key_file)) {
    stop(paste0("Could not find key_file ", key_file, " for SitesReport"))
  }

  key <- scan(file = key_file, what = "character")

  # list_file (may be optional later on)
  if (!file.exists(list_file)) {
    stop(paste0("Could not find list_file ", list_file, " for SitesReport"))
  }
  # Read in user's list
  species_user <- readr::read_csv(file = list_file, col_types = readr::cols())

  # TODO: Could be defensive here and check for Species column

  # Names need to be split into common name column and scientific name column
  species_user <- dplyr::bind_cols(species_user,
                                SplitNames(species_user$Species))

  message(paste("User's species count", nrow(species_user)))

  # Use purrr::map to iterate over each coordinate pair?
  # center_results <- purrr::map(centers_list,
  #                              CenterQuery, # pass parameters to CenterQuery
  #                              key = key,
  #                              species_user = species_user,
  #                              dist = dist,
  #                              back = back,
  #                              hotspot = hotspot,
  #                              include_provisional = include_provisional,
  #                              max_tries = max_tries,
  #                              timeout_sec = timeout_sec,
  #                              verbose = verbose)


  save_queries <- TRUE

  # list to store results of various queries; each element will have two child
  # elements:
  #    center_info: the original centers_list data.frame
  #    results:     information for the top sites to report
  results_list <- list()
  
  # The below needs to run for EACH center
  for (i in 1:length(centers_list)) {
    center <- centers_list[[i]]

    # TODO: skip all this saving junk once it works
    # See START/END comments for boundaries
    rn_query_save <- paste0(center$name, "_rn.Rds")
    if (file.exists(rn_query_save)) {
      message(paste("Loading in from ", rn_query_save))
      recent_obs <- readRDS(file = rn_query_save)
    } else {
      #### START only this remains following testing
      # Do RecentNearby query to find all species recently seen within a radius
      # of dist from the coordinates of the current center
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
      #### END only this remains following testing
      if (save_queries) {
        message(paste("Saving to ", rn_query_save))
        saveRDS(recent_obs, file = rn_query_save)
      }
    }

    # Pull out the species list from the RecentNearby object
    species_all <- recent_obs$obs

    # DropPatterns
    species_all <- DropPatterns(data = species_all)

    message(paste("All species count", nrow(species_all)))

    # Perform set difference, getting list of all nearby species that are *NOT*
    # on user's list: species_unseen = species_all \ species_user
    species_unseen <- species_all[!(species_all$comName %in% species_user$Common), ]

    message(paste("Missing species count", nrow(species_unseen)))
    # nearby.missing <- nearby.obs[!(nearby.obs$comName %in% current.list$Common), ]

    # TODO: skip all this saving junk once it works
    # See START/END comments for boundaries
    rns_query_save <- paste0(center$name, "_rns.Rds")
    if (file.exists(rns_query_save)) {
      message(paste("Loading in from ", rns_query_save))
      nearby_list <- readRDS(file = rns_query_save)
    } else {
      #### START only this remains following testing
      # List to hold nearby observations of species to be seen; will be indexed 
      # by eBird's species code
      nearby_list <- list()
      
      # For each species remaining in species_unseen$speciesCode, run 
      # RecentNearbySpecies
      for (species_code in species_unseen$speciesCode) {
        # message(paste("Querying", speciesCode))
        
        nearby_sp_obs <- RecentNearbySpecies(key = key,
                                             species_code = species_code,
                                             lat = center$lat,
                                             lng = center$lng,
                                             dist = dist,
                                             back = back,
                                             hotspot = hotspot,
                                             include_provisional = include_provisional,
                                             max_tries = max_tries,
                                             timeout_sec = timeout_sec,
                                             verbose = verbose)
        # Extract just the observations data frame from the query, but only if 
        # there were results returned (obs is not NULL)
        if (!is.null(nearby_sp_obs$obs)) {
          nearby_list[[species_code]] <- nearby_sp_obs$obs
        }
        Sys.sleep(time = 0.5) # So we're not hammering on eBird's server
      }
      #### END only this remains following testing
      if (save_queries) {
        message(paste("Saving to ", rns_query_save))
        saveRDS(nearby_list, file = rns_query_save)
      }
    }
    
    # Put all results together in single data frame
    all_nearby <- dplyr::bind_rows(nearby_list)

    # Do counts for each site to identify which sites have the most unseen 
    # species, only retain results for top X sites, where X = max_sites
    top_site_counts <- all_nearby %>%
      dplyr::group_by(locId) %>%
      dplyr::summarize(total_unseen = n()) %>%
      dplyr::arrange(dplyr::desc(total_unseen)) %>%
      dplyr::slice(1:max_sites)
        
    # grab the data from those sites identified in top_sites
    top_nearby <- all_nearby %>%
      filter(locId %in% top_site_counts$locId) %>%
      select(-obsValid, - obsReviewed, -locationPrivate, -subId)
    
    # attach to the 
    # centers_list element, which will be passed to the RMarkdown template for 
    # additional formatting & processing
    # Create the results_list element at the appropriate index (i). It will be 
    # a 2-element list, with the original center information (lat, lng, name) 
    # and the results for the top sites
    results_list[[i]] <- list(center_info = center,
                              results = top_nearby)

  }
  # end iteration over each center

  # Package the settings of this report to be printed at the end
  report_details <- list(max_sites = 5,
                         dist = 50,
                         back = 4,
                         hotspot = TRUE,
                         include_provisional = FALSE,
                         drop_patterns = c("sp.", "/", "Domestic type", "hybrid"))
  
  # TODO: Delete these saves when done testing
  saveRDS(results_list, file = "results-list.Rds")
  saveRDS(report_details, file = "report-details.Rds")

  # Using template, pass info to an RMarkdown template file
  report_template <- system.file("rmd", "Report-Template.Rmd", 
                                 package = "lifeR")
  
  output_format <- "html_document"  
  rmarkdown::render(input = report_template, 
                    output_format = output_format,
                    output_file = report_filename, # knitr takes care of extension
                    output_dir = report_dir,
                    params = list(results_list = results_list,
                                  report_details = report_details),
                    quiet = verbose)

  # Will need to grab rmarkdown template that lives in inst/rmd like this:
  # report_template <- system.file("rmd", "Report-Template.Rmd", package = "lifeR")
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

#' Perform search and site selection for a single coordinate pair
#'
#' @param x list with center coordinate information (see Details)
#' @param key character vector of user's eBird API key
#' @param species_user data frame of species that user has observed
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
#' @details The first argument of this function must be a list of data frames,
#' each with a single row of data and at least three columns:
#' \itemize{
#'  \item{"lat"}{Latitude in decimal degrees}
#'  \item{"lng"}{Longitude in decimal degrees}
#'  \item{"name"}{Name of center to use in output report}
#' }
#'
#' @return list of query results.
#' #' \itemize{
#'  \item{"center_info"}{List including latitude, longitude, and center name}
#'  \item{"target_sites"}{List of target sites and potential species}
#' }
CenterQuery <- function(x, key,
                        species_user,
                        max_sites,
                        dist,
                        back,
                        hotspot,
                        include_provisional,
                        max_tries,
                        timeout_sec,
                        verbose,
                        drop_patterns) {


  # Query eBird for recent observations near the center
  recent_obs <- RecentNearby(key = key,
                             lat = x$lat,
                             lng = x$lng,
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
