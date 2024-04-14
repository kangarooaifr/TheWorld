

# -- function definition
airport_coord <- function(airports, id = NULL, iata = NULL){

  # -- check param
  if(is.null(id) && is.null(iata))
    return(NULL)
  
  # -- check arg
  if(!is.null(iata))
    id <- airports[airports$iata == iata, 'id']
  
  # -- get longitude / latitude
  name <- airports[airports$id == id, 'name']
  lng <- airports[airports$id == id, 'longitude']
  lat <- airports[airports$id == id, 'latitude']
  cat("[airport_coord]", name, "- lng =", lng, "/ lat =", lat, "\n")
  
  # -- return
  c(lng, lat)
  
}
