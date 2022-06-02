#' Create report for sites with most unseen species
#'
#' @param centers Numeric vector or matrix of latitude and longitude
#' coordinates; vector should be of length 2, e.g. 
#' \code{c(latitude, longitude)}, while matrix should have two columns (first 
#' column is latitude, second column is longitude).
#' @param ebird_key Character vector with eBird API key.
#' @param species_seen Character vector of species that have already been seen.
#' @param center_names Character vector of names to use for each pair of 
#' latitude and longitude coordinates in \code{centers}.
#' @param report_filename Name of output file without file extension (see 
#' \code{report_format}); e.g. if \code{report_filename} is "sites-2021" and 
#' \code{report_format} is "html", the report will be saved to sites-2021.html.
#' @param report_dir Destination folder for the output file; if \code{NULL}, 
#' report will be saved to working directory.
#' @param report_format File format for report; takes one of two values: "html" 
#' or "pdf".
#' @param max_sites Maximum number of sites to return for each pair of
#' coordinates defined in \code{centers}; maximum is 12.
#' @param dist Numeric radius in kilometers of distance from each geographic 
#' center point defined by coordinates in \code{centers} from which to return 
#' recent observations.
#' @param back Number of days back to search for observations.
#' @param hotspot Logical indicating whether or not to restrict results to
#' hotspot locations.
#' @param include_provisional Logical indicating whether not to include
#' observations which have not yet been reviewed.
#' @param max_tries Maximum number of query attempts to try (only for
#' expert use).
#' @param timeout_sec Integer time to allow before query is aborted (only for
#' expert use).
#' @param messages Character indicating the degree to which messages are 
#' printed during the report assembly process. Options are "minimal", "none", 
#' or "verbose".
#' @param drop_patterns Character vector of patterns in species' names to
#' exclude certain species from consideration, such as domesticated species,
#' hybrids, and observations not identified to species level (e.g.
#' "Toxostoma sp.").
#'
#' @details The function uses the eBird API (see \url{https://documenter.getpostman.com/view/664302/S1ENwy59})
#' to build the report. Queries to the eBird API require a user key; you can 
#' request an eBird API key by logging into your eBird account and navigating 
#' to \url{https://ebird.org/api/keygen}. See examples and vignette for using 
#' your eBird API key.
#'
#' @return Silently returns a list with two named elements:
#' \describe{
#'   \item{results_list}{A list where each element is a list of the results of 
#'   queries for a center. Each element is a list with two named elements:}
#'     \describe{
#'       \item{center_info}{A list with latitude (\code{lat}), longitude 
#'       (\code{longitude}), and name \code{name} of the geographic center.}
#'       \item{results}{A tibble of observations from the top sites (with a 
#'       maximum number of sites defined by \code{max_sites}).}
#'     }
#'   \item{report_details}{A list containing the settings used to build this 
#'   report, such as days back and distances.}
#' }
#' 
#' @examples 
#' \dontrun{
#'   # Read in data downloaded from eBird
#'   list_file <- system.file("extdata", "example-list.csv", package = "lifeR")
#'   user_list <- read.csv(file = list_file)
#'   # Only common names are required
#'   my_species <- SplitNames(x = user_list$Species)$Common
#'   # Read in eBird API key from a text file
#'   key <- scan(file = "ebird-key.txt", what = "character")
#' 
#'   # A single center requires vector of coordinates
#'   locs <- c(45, -109)
#'   SitesReport(centers = locs, ebird_key = key, 
#'   species_seen = my_species)
#'   
#'   # For multiple centers, pass a matrix to centers argument
#'   loc_mat <- matrix(data = c(33, -109, 39, -119.1), nrow = 2, byrow = TRUE)
#'   loc_names <- c("Brushy Mountain", "Yerington")
#'   SitesReport(centers = loc_mat, ebird_key = key, 
#'   species_seen = my_species, center_names = loc_names)
#' }
#' @importFrom readr read_csv cols
#' @import dplyr
#' @importFrom rmarkdown render
#' @export
SitesReport <- function(centers,
                        ebird_key,
                        species_seen,
                        center_names = NULL,
                        report_filename = "Goals-Report",
                        report_dir = getwd(),
                        report_format = c("html", "pdf"),
                        max_sites = 5,
                        dist = 50,
                        back = 4,
                        hotspot = TRUE,
                        include_provisional = FALSE,
                        max_tries = 5,
                        timeout_sec = 30,
                        messages = c("minimal", "none", "verbose"), 
                        drop_patterns = c("sp.", "/", "Domestic type", "hybrid")) {

  # Grab report format; doing this early to ensure proper format is indicated
  report_format <- match.arg(report_format)
  messages <- match.arg(messages)
  
  # the centers object will need to ultimately be a list, but start by making 
  # sure it is a matrix, doing transformation of data frame or vector as 
  # appropriate
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
  if (!is.numeric(centers)) {
    stop("SitesReport requires numeric centers data (decimal longitude & latitude)")
  }

  # Set a ceiling on the maximum number of sites; otherwise map is 
  # incomprehensible
  if (!is.numeric(max_sites)) {
    max_sites <- 5
  }
  if (max_sites > 12) {
    max_sites <- 12
    if(messages %in% c("minimal", "verbose")) {
      message("Maximum number of sites per center is 12.")
    }
  }
  
  # Add in center names if user passed those along; need to make sure they are 
  # the right length. If not, message user and proceed as if user had not 
  # provided names
  user_supplied_names <- FALSE
  if (!is.null(center_names)) {
    if (length(center_names) == nrow(centers)) {
      user_supplied_names <- TRUE
    } else {
      if(messages %in% c("minimal", "verbose")) {
        message("Number of center names does not match number of centers passed to SitesReport; names will be auto-generated")
      }
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

  # If user passed a list of species they have seen, assign that here  
  species_user <- character(0)
  if (!is.null(species_seen)) {
    species_user <- species_seen
  }
  
  # list to store results of various queries; each element will be a list with 
  # two child elements:
  #    center_info: the original centers_list data.frame
  #    results:     information for the top sites to report
  results_list <- list()
  
  # The below needs to run for EACH center, not running this in parallel 
  # because we want to be nice to eBird's servers
  for (i in 1:length(centers_list)) {
    center <- centers_list[[i]]
    
    if (messages %in% c("minimal", "verbose")) {
      message(paste0("Requesting nearby sightings for ", center$name, "..."))
    }
    
    # Do RecentNearby query to find all species recently seen within a radius
    # of dist from the coordinates of the current center
    recent_obs <- RecentNearby(key = ebird_key,
                               lat = center$lat,
                               lng = center$lng,
                               dist = dist,
                               back = back,
                               hotspot = hotspot,
                               include_provisional = include_provisional,
                               max_tries = max_tries,
                               timeout_sec = timeout_sec,
                               verbose = (messages == "verbose"))
    
    # See if any observations were returned; if not, let the user know and set
    # results element to NULL
    if (is.null(recent_obs$obs)) {
      if (messages %in% c("minimal", "verbose")) {
        message(paste0("No recent observations near ", center$name, " found."))
      }
      results_list[[i]] <- list(center_info = center,
                                results = NULL)
    } else {
      # Pull out the species list from the RecentNearby object
      species_all <- recent_obs$obs
      
      # DropPatterns removes things like "hybrid", "sp."
      species_all <- DropPatterns(data = species_all, 
                                  patterns = drop_patterns)
      
      # Perform set difference, getting list of all nearby species that are *NOT*
      # on user's list: species_unseen = species_all \ species_user; if user did 
      # not pass list of species already seen, just use all values remaining in 
      # species_all as species_unseen
      
      if (length(species_user) > 0) {
        species_unseen <- species_all[!(species_all$comName %in% species_user), ]
      } else {
        species_unseen <- species_all
      }

      # Only proceed if there are any unseen species
      if (nrow(species_unseen) < 1) {
        if (messages %in% c("minimal", "verbose")) {
          message(paste0("No unseen species found for center ", center$name))
        }
        results_list[[i]] <- list(center_info = center,
                                  results = NULL)
      } else {
        if (messages %in% c("minimal", "verbose")) {
          message(paste("Querying eBird for", nrow(species_unseen), "species..."))
        }
        
        # List to hold nearby observations of species to be seen; will be indexed 
        # by eBird's species code
        nearby_list <- list()
        
        # For each species in species_unseen$speciesCode, run RecentNearbySpecies
        # to find locations where they have recently been seen
        for (species_code in species_unseen$speciesCode) {
          nearby_sp_obs <- RecentNearbySpecies(key = ebird_key,
                                               species_code = species_code,
                                               lat = center$lat,
                                               lng = center$lng,
                                               dist = dist,
                                               back = back,
                                               hotspot = hotspot,
                                               include_provisional = include_provisional,
                                               max_tries = max_tries,
                                               timeout_sec = timeout_sec,
                                               verbose = (messages == "verbose"))
          # Extract just the observations data frame from the query, but only if 
          # there were results returned (obs is not NULL)
          if (!is.null(nearby_sp_obs$obs)) {
            nearby_list[[species_code]] <- nearby_sp_obs$obs
          }
          Sys.sleep(time = 0.1) # So we're not hammering on eBird's server
          
        } # end iteration over all unseen species
        
        # Put all results together for this center in single data frame
        all_nearby <- dplyr::bind_rows(nearby_list)
        
        # Proceed to find top X sites only if at least some sites with unseen 
        # species were found
        if (nrow(all_nearby) < 1) {
          if (messages %in% c("minimal", "verbose")) {
            message(paste0("No sites found for center ", center$name))
          }
          results_list[[i]] <- list(center_info = center,
                                    results = NULL)
        } else {
          
          # Do counts for each site to identify which sites have the most unseen 
          # species, only retain results for top X sites, where X = max_sites
          top_site_counts <- all_nearby %>%
            dplyr::group_by(.data$locId) %>%
            dplyr::summarize(total_unseen = dplyr::n()) %>%
            dplyr::arrange(dplyr::desc(.data$total_unseen)) %>%
            dplyr::slice(1:max_sites)
          
          # grab the data from those sites identified in top_sites
          top_nearby <- all_nearby %>%
            filter(.data$locId %in% top_site_counts$locId) %>%
            select(-.data$obsValid, -.data$obsReviewed, -.data$locationPrivate)
          
          # Create the results_list element at the appropriate index (i). It will be 
          # a 2-element list, with the original center information (lat, lng, name) 
          # and the results for the top sites; will be passed to RMarkdown template
          # for additional formatting & processing
          results_list[[i]] <- list(center_info = center,
                                    results = top_nearby)
        } # end conditional for at least one site with missing species returned
        
      } # end conditional for at least one unseen species found
      
    } # end conditional for at least one nearby observation found
    
  } # end iteration over each center
  
  # Package the settings of this report to be printed at the end of the report
  report_details <- list(max_sites = max_sites,
                         dist = dist,
                         back = back,
                         hotspot = hotspot,
                         include_provisional = include_provisional,
                         drop_patterns = drop_patterns)
  
  # Locate the template RMarkdown file
  report_template <- system.file("rmd", "Report-Template.Rmd", 
                                 package = "lifeR")
  
  # Create the output_format as expected by rmarkdown::render
  output_format <- paste0(report_format, "_document")

  # Use template and pass information about sites and centers for knitting  
  rmarkdown::render(input = report_template, 
                    output_format = output_format,
                    output_file = report_filename,
                    output_dir = report_dir,
                    params = list(results_list = results_list,
                                  report_details = report_details),
                    quiet = !(messages == "verbose"))
 
  if (messages %in% c("minimal", "verbose")) {
    message(paste0("Report written to ", report_dir, "/", report_filename, 
                   ".", report_format))
  }
   
  # Silently return the results_list and report_details
  invisible(x = list(results_list = results_list,
                     report_details = report_details))
}