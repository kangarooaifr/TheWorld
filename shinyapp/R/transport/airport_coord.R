

# -- function definition
airport_coord <- function(airports, id = NULL, iata = NULL){
  
  # -- check param
  if(is.null(id) && is.null(iata))
    return(NULL)
  
  # -- case: id
  if(!is.null(id)){
    col <- 'id'
    value <- id}
  
  # -- case: iata
  if(!is.null(iata)){
    col <- 'iata'
    value <- iata}
  
  # -- get longitude / latitude
  lng = airports[airports[col] == value, ]$lng
  lat = airports[airports[col] == value, ]$lat
  
  # -- return
  c(lng, lat)
  
}
