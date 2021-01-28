#' Create report for sites to target
#'
#' @param centers numeric vector or matrix of latitude and longitude coordinates
#' @param key_file path to file with eBird API key
#' @param list_file path to file of user's observations
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
                         verbose = TRUE,
                         drop_patterns = c("sp.", "/", "Domestic type", "hybrid")) {

  # Grab things we need (or stop) and qa/qc
  # Centers vector or matrix make sure they're realistic
  if (is.data.frame(centers)) {
    centers <- as.matrix(centers)
  }


  # key_file

  # list_file (may be optional later on)

  # Read in user's list.

  # SplitNames

  # RecentNearby

  # DropPatterns

  # The below needs to run for EACH center

    # do set difference between user's list and result of RecentNearby post-drop

    # For each species remaining, run
    # RecentNearbySpecies
    # combine all this stuff, identify the targets, do some tidyverse here before
    # sending to Target report; will probably need to just create a list (or
    # fancier object) with name, lat/long, results of set diff

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
