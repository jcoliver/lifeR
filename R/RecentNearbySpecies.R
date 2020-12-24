#' Retrieve recent nearby observations of a species
#'
#' @param key character eBird API key
#' @param species_code character code for species of interest; usually a
#' six-character string such as "purmar" or "batpig". See
#' \url{https://ebird.org/science/the-ebird-taxonomy}
#' for more information.
#' @param lat numeric latitude; use negative values for southern latitudes
#' (i.e. -46.86, \emph{not} "46.86 S)
#' @param lng numeric longitude; use negative values for western
#' longitudes (i.e. -72.08, \emph{not} "72.08 W")
#' @param dist numeric radius (in kilometers) of area from center point
#' given by \code{lat} and \code{lng} from which to return recent observations
#' @param back integer number of days back to search for observations
#' @param hotspot logical indicating whether or not to restrict results to
#' hotspot locations
#' @param include_provisional logical indicating whether not to include
#' observations which have not yet been reviewed
#' @param max_tries integer maximum number of query attempts to try
#' @param timeout_sec integer time to allow before query is aborted
#' @param verbose logical determining whether or not to print messages during
#' queries
#'
#' @return An object of class "recent_obs" with the following elements:
#' \describe{
#'   \item{query_type}{the type of query performed}
#'   \item{query_parameters}{list of query parameters passed in request,
#'   including the species code}
#'   \item{obs}{data frame of observations returned from query; if no
#'   observations are returned, \code{obs} is NULL}
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
                                dist = 80,
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
