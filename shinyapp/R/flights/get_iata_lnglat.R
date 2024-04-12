

# -- function definition
get_iata_lnglat <- function(airports, iata.code){
  
  # -- get longitude & latitude
  lng = airports[airports['IATA'] == iata.code, ]$Longitude
  lat = airports[airports['IATA'] == iata.code, ]$Latitude
  
  cat("IATA code", iata.code, "lng =", lng, "lat =", lat, "\n")
  
  # -- return
  c(lng, lat)
  
}
