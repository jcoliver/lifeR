#' Retrieve recent nearby observations
#'
#' @param key character eBird API key
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
#' @details The function uses the eBird API (see \link{https://documenter.getpostman.com/view/664302/S1ENwy59})
#' to query recent citings. Queries to the eBird API require a user key; more
#' information on obtaining a key can be found at the eBird API documentation.
#'
#' @return a data frame of recent observations
#'
#' @importFrom curl curl new_handle
#' @importFrom jsonlite fromJSON
#' @export
RecentNearby <- function(key,
                         lat = 32.241,
                         lng = -110.938,
                         dist = 80,
                         back = 4,
                         hotspot = TRUE,
                         include_provisional = FALSE,
                         max_tries = 5,
                         timeout_sec = 30) {

  # need to coerce logicals to character
  request <- paste0("https://ebird.org/ws2.0/data/obs/geo/recent?",
                    "&lat=", lat,
                    "&lng=", lng,
                    "&dist=", distance,
                    "&back=", days.back,
                    "&hotspot=", tolower(as.character(hotspot)),
                    "&includeProvisional=", tolower(as.character(include_provisional)),
                    "&key=", key)
  obs_request <- character(0)
  tries <- 0
  message(paste0("Requesting ", request, "..."))
  while(class(obs_request) != "data.frame" && tries < max_tries) {
    if (tries > 0) {
      message(paste0("...attempt ", tries, " failed; requesting again"))
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
    message(paste0("Failed request for ", request, " after ", tries, " tries."))
  }
  return(obs_request)
}
