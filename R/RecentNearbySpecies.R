#' Retrieve recent nearby observations of a species
#'
#' @param key character eBird API key
#' @param species_code character code for species of interest; usually a
#' six-character string such as "purmar" or "batpig". See
#' \link{https://ebird.org/science/the-ebird-taxonomy} for more information.
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
#'
#' @importFrom curl curl new_handle
#' @importFrom jsonlite fromJSON
#' @export
RecentNearbySpecies <- function(key, species_code, lat = 32.241, lng = -110.938,
                                dist = 80, back = 4, hotspot = "true",
                                max_tries = 5, timeout_sec = 30) {

  request <- paste0("https://api.ebird.org/v2/data/obs/geo/recent/",
                    species_code, "?",
                    "&lat=", lat,
                    "&lng=", lng,
                    "&dist=", distance,
                    "&back=", back,
                    "&hotspot=", tolower(as.character(hotspot)),
                    "&key=", key)

  obs_request <- character(0)
  tries <- 0
  message(paste0("Requesting ", species_code, "..."))
  while(class(obs_request) != "data.frame" && tries < max_tries) {
    if (tries > 0) {
      message(paste0("...attempt ", tries, " failed for ", species_code, ". Requesting again"))
    }
    # Allow for longer host resolution times via the handle/CONNECTTIMEOUT
    # options of curl::curl
    ebird_connection <- curl::curl(request,
                                   handle = curl::new_handle(CONNECTTIMEOUT = timeout_sec))
    obs_request <- try(expr = {
      ebirdJSON <- readLines(ebird_connection, warn = FALSE)
      jsonlite::fromJSON(txt = ebirdJSON)
    }, silent = TRUE)
    close(ebird_connection)

    tries <- tries + 1
  }
  if (class(obs_request) != "data.frame"){
    message(paste0("Failed request for ", species_code, " after ", tries, " tries."))
  }
  return(obs_request)
}
