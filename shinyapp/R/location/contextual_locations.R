

# --------------------------------------------------------------------------
# Contextual (temporary) locations
# --------------------------------------------------------------------------

# -- function definition
contextual_locations <- function(locations, airports = NULL, railway_stations = NULL, bus_stations = NULL, bounds){
  
  cat("[contextual_locations] Get contextual locations \n")
  
  # -- get locations
  # -------------------------------------
  locations <- bounding_box(locations, bounds)
  cat("-- locations =", nrow(locations), "\n")
  
  
  # -- get airports
  # -------------------------------------
  if(!is.null(airports)){
    
    airports <- bounding_box(airports, bounds)
    cat("-- airports =", nrow(airports), "\n")
    
    # -- turn airports into locations & merge
    if(dim(airports)[1] > 0){
      
      airports <- airport_to_location(airports)
      locations <- rbind(locations, airports)}}
  
  
  # -- get railway_stations
  # -------------------------------------
  if(!is.null(railway_stations)){
    
    railway_stations <- bounding_box(railway_stations, bounds)
    cat("-- railway_stations =", nrow(railway_stations), "\n")
    
    # -- turn railway stations into locations & merge
    if(dim(railway_stations)[1] > 0){
      
      railway_stations <- railway_to_location(railway_stations)
      locations <- rbind(locations, railway_stations)}}
  
  
  # -- get bus_stations
  # -------------------------------------
  if(!is.null(bus_stations)){
    
    bus_stations <- bounding_box(bus_stations, bounds)
    cat("-- bus_stations =", nrow(bus_stations), "\n")
    
    # -- turn bus stations into locations & merge
    if(dim(bus_stations)[1] > 0){
      
      bus_stations <- bus_to_location(bus_stations)
      locations <- rbind(locations, bus_stations)}}
  
  
  # -- return
  # -------------------------------------
  cat(">> output dim =", dim(locations), "\n")
  locations
  
}
