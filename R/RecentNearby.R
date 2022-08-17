#' Recent nearby eBird observations
#'
#' @param key Character eBird API key.
#' @param lat Numeric latitude; use negative values for southern latitudes
#' (i.e. -46.86, \emph{not} "46.86 S).
#' @param lng Numeric longitude; use negative values for western
#' longitudes (i.e. -72.08, \emph{not} "72.08 W").
#' @param dist Numeric radius in kilometers of distance from geographic center 
#' point given by \code{lat} and \code{lng} from which to return recent 
#' observations.
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
#' @details The function uses the eBird API (see \url{https://documenter.getpostman.com/view/664302/S1ENwy59/})
#' to query recent sightings. Queries to the eBird API require a user key; more
#' information on obtaining a key can be found at the eBird API documentation.
#'
#' @return An object of class "recent_obs" with the following elements:
#' \describe{
#'   \item{query_type}{The type of query performed.}
#'   \item{query_parameters}{List of query parameters passed in request.}
#'   \item{obs}{data frame of observations returned from query; if no
#'   observations are returned, \code{obs} is \code{NULL}}. Columns include:
#'     \describe{
#'       \item{speciesCode}{The (usually) six-letter species code, see 
#'       \url{https://science.ebird.org/en/use-ebird-data/the-ebird-taxonomy/}}
#'       \item{comName}{Species' common name.}
#'       \item{sciName}{Species' scientific name.}
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
#'    }
#' }
#'
#' @examples
#' \dontrun{
#'   # Read eBird key in from file
#'   key <- scan(file = "ebird-key.txt", what = "character")
#'   # Search for observations 5 km from lat/lng coordinates
#'   recent <- RecentNearby(key = key, lat = 32.28, lng = -111.02, dist = 5)
#' }
#'
#' @importFrom curl curl new_handle curl_fetch_memory
#' @importFrom jsonlite fromJSON
#' 
#' @export
RecentNearby <- function(key,
                         lat = 32.241,
                         lng = -110.938,
                         dist = 50,
                         back = 4,
                         hotspot = TRUE,
                         include_provisional = FALSE,
                         max_tries = 5,
                         timeout_sec = 30,
                         verbose = TRUE) {

  # need to coerce logicals to character
  request <- paste0("https://api.ebird.org/v2/data/obs/geo/recent?",
                    "&lat=", lat,
                    "&lng=", lng,
                    "&dist=", dist,
                    "&back=", back,
                    "&hotspot=", tolower(as.character(hotspot)),
                    "&includeProvisional=", tolower(as.character(include_provisional)),
                    "&key=", key)
  observations <- character(0)
  tries <- 0
  success <- FALSE
  while(!success && tries < max_tries) {
    if (tries > 0) {
      if (verbose) {
        message(paste0("...attempt ", tries, " failed; requesting again"))
      }
    }

    ebird_connection <- curl::curl(request,
                                   handle = curl::new_handle(CONNECTTIMEOUT = timeout_sec))

    # Make sure that request is OK
    test_connection <- curl::curl_fetch_memory(request)

    # Status 200, OK to proceed
    if (test_connection$status_code == 200) {
      observations <- try(expr = {
        ebirdJSON <- readLines(ebird_connection, warn = FALSE)
        success <- TRUE
        # If query returns zero observations, it returns an empty JSON object
        # "[]"
        if (nchar(ebirdJSON) == 2) {
          # This looks odd, but it is how we assign value is assigned to zero
          # observations
          NULL
        } else {
          jsonlite::fromJSON(txt = ebirdJSON)
        }
      }, silent = TRUE)
    }
    close(ebird_connection)
    tries <- tries + 1
  }

  if (!success & max_tries <= tries){
    message(paste0("Failed request for ", lat, ", ", lng, " after ", tries, " tries."))
    observations <- NULL
  }

  # Want to return the query parameters along with any results
  query_params = list(lat = lat,
                      lng = lng,
                      dist = dist,
                      back = back,
                      hotspot = hotspot,
                      include_provisional = include_provisional)

  # Create the object and class it appropriately
  query_result <- list(query_type = "nearby observations",
                       query_params = query_params,
                       obs = observations)

  class(query_result) <- "recent_obs"
  return(query_result)
}
