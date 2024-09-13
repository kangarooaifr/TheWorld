
# https://nominatim.openstreetmap.org/reverse?format=json&lat=52.5487429714954&lon=-1.81602098644987&zoom=18&addressdetails=1

reverse_geocoding <- function(lng, lat, zoom = 18, addressdetails = 1){

  cat("[reverse_geocoding] Calling nominatim API \n")
    
  # -- init
  address <- "https://nominatim.openstreetmap.org"
  endpoint <- "reverse"
  format <- "json"
  
  # -- build parameters
  params <- paste(paste0("format=", format),
                  paste0("lat=", lat),
                  paste0("lon=", lng),
                  paste0("zoom=", zoom),
                  paste0("addressdetails=", addressdetails),
                  sep = "&")
  
  # -- build query
  url <- paste(address, endpoint, sep = "/")
  url <- paste(url, params, sep = "?")
  cat("-- url =", url, "\n")
  
  # -- call API
  result <- tryCatch(jsonlite::fromJSON(url), error = function(c) return(list()))

  # -- return
  cat("-- output osm_id =", result$osm_id, "\n")
  result
  
}
