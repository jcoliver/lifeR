#' Retrieve recent nearby observations of a species
#'
#' @param key Character eBird API key.
#' @param species_code Species code for species of interest; usually a
#' six-character string such as "purmar" or "batpig". See
#' \url{https://ebird.org/science/the-ebird-taxonomy} for more information.
#' @param lat Numeric decimal degree latitude; use negative values for southern 
#' latitudes (i.e. -46.86, \emph{not} "46.86 S").
#' @param lng Numeric decimal degree longitude; use negative values for western
#' longitudes (i.e. -72.08, \emph{not} "72.08 W").
#' @param dist Numeric radius in kilometers of distance from geographic center 
#' point given by \code{lat} and \code{lng} from which to return recent 
#' observations of a species.
#' @param back Integer number of days back to search for observations.
#' @param hotspot Logical indicating whether or not to restrict results to
#' hotspot locations.
#' @param include_provisional Logical indicating whether or not to include
#' observations which have not yet been reviewed.
#' @param max_tries Integer maximum number of query attempts to try.
#' @param timeout_sec Integer time to allow before query is aborted.
#' @param verbose Logical determining whether or not to print messages during
#' queries.
#'
#' @details The function uses the eBird API (see \url{https://documenter.getpostman.com/view/664302/S1ENwy59})
#' to query recent sightings of a species. Queries to the eBird API require a 
#' user key; you can request an eBird API key by logging into your eBird 
#' account and navigating to \url{https://ebird.org/api/keygen}. See examples 
#' and vignette for using your eBird API key.
#'
#' @return An object of class "recent_obs" with the following elements:
#' \describe{
#'   \item{query_type}{The type of query performed.}
#'   \item{query_parameters}{List of query parameters passed in request,
#'   including the species code.}
#'   \item{obs}{Data frame of observations returned from query; if no
#'   observations are returned, \code{obs} is \code{NULL}. Columns include:}
#'     \describe{
#'       \item{speciesCode}{The (usually) six-letter species code, see 
#'       \url{https://ebird.org/science/the-ebird-taxonomy}}
#'       \item{comName}{Species' common name.}
#'       \item{sciName}{Speices' scientific name.}
#'       \item{locId}{eBird identifier of the location.}
#'       \item{locName}{Name of the location.}
#'       \item{obsDt}{Observation date as character string in the format 
#'       "YYYY-MM-DD HH:MM".}
#'       \item{howMany}{Number of individuals.}
#'       \item{lat}{Decimal latitude.}
#'       \item{lng}{Decimal longitude.}
#'       \item{obsValid}{Logical indicating if observation marked as valid.}
#'       \item{obsReviewed}{Logical indicating if observation has been reviewed.}
#'       \item{locationPrivate}{Logical indicating whether or not location is 
#'       designated as private.}
#'       \item{subId}{Checklist ID for this observation.}
#'     }
#' }
#'
#' @importFrom curl curl new_handle
#' @importFrom jsonlite fromJSON
#' @export
#'
#' @examples
#' \dontrun{
#'   # Read eBird key in from file
#'   key <- scan(file = "ebird-key.txt", what = "character")
#'   # Search for observations of Verdin within 5 km from lat/lng coordinates
#'   recent <- RecentNearbySpecies(key = key, species_code = "verdin",
#'                                 lat = 32.28, lng = -111.02, dist = 5)
#' }
RecentNearbySpecies <- function(key,
                                species_code,
                                lat = 32.241,
                                lng = -110.938,
                                dist = 50,
                                back = 4,
                                hotspot = TRUE,
                                include_provisional = FALSE,
                                max_tries = 5,
                                timeout_sec = 30,
                                verbose = TRUE) {

  request <- paste0("https://api.ebird.org/v2/data/obs/geo/recent/",
                    species_code, "?",
                    "&lat=", lat,
                    "&lng=", lng,
                    "&dist=", dist,
                    "&back=", back,
                    "&hotspot=", tolower(as.character(hotspot)),
                    "&includeProvisional=", tolower(as.character(include_provisional)),
                    "&key=", key)

  observations <- character(0)
  tries <- 0

  if (verbose) {
    message(paste0("Requesting ", species_code, "..."))
  }

  while(class(observations) != "data.frame" && tries < max_tries) {
    if (tries > 0) {
      if (verbose) {
        message(paste0("...attempt ", tries, " failed for ", species_code, ". Requesting again"))
      }
    }
    ebird_connection <- curl::curl(request,
                                   handle = curl::new_handle(CONNECTTIMEOUT = timeout_sec))
    observations <- try(expr = {
      ebirdJSON <- readLines(ebird_connection, warn = FALSE)
      jsonlite::fromJSON(txt = ebirdJSON)
    }, silent = TRUE)
    close(ebird_connection)

    tries <- tries + 1
  }
  if (class(observations) != "data.frame"){
    message(paste0("Failed request for ", species_code, " after ", tries, " tries."))
    observations <- NULL
  }

  # Want to return the query parameters along with any results
  query_params = list(species_code = species_code,
                      lat = lat,
                      lng = lng,
                      dist = dist,
                      back = back,
                      hotspot = hotspot,
                      include_provisional = include_provisional)

  # Create the object and class it appropriately
  query_result <- list(query_type = "nearby species",
                       query_params = query_params,
                       obs = observations)
  class(query_result) <- "recent_obs"
  return(query_result)
}
