

# --------------------------------------------------------------------------
# Contextual (temporary) locations
# --------------------------------------------------------------------------

# -- function definition
contextual_locations <- function(map, locations){
  
  
  cat("[contextual_locations] Get contextual locations \n")
  
  # -------------------------------------
  # locations
  # -------------------------------------
  
  # -- get locations
  ctx_locations <- bounding_box(locations$items(), map$bounds())
  cat("-- locations =", nrow(ctx_locations), "\n")
  
  
  # -------------------------------------
  # airports
  # -------------------------------------
  
  # -- check zoom level
  if(map$zoom() >= setting("airports_level")){
    
    # -- get airports
    ctx_airports <- bounding_box(locations$airports, map$bounds())
    cat("-- airports =", nrow(ctx_airports), "\n")
    
    # -- turn airports into locations & merge
    if(nrow(ctx_airports) > 0){
      
      ctx_airports <- airport_to_location(ctx_airports)
      ctx_locations <- rbind(ctx_locations, ctx_airports)}}
  
  
  # -------------------------------------
  # railway stations
  # -------------------------------------

  # -- check zoom level
  if(map$zoom() >= setting("railway_stations_level")){
    
    # -- get stations
    ctx_railway_stations <- bounding_box(locations$railway_stations, map$bounds())
    cat("-- railway_stations =", nrow(ctx_railway_stations), "\n")
    
    # -- turn railway stations into locations & merge
    if(nrow(ctx_railway_stations) > 0){
      
      ctx_railway_stations <- railway_to_location(ctx_railway_stations)
      ctx_locations <- rbind(ctx_locations, ctx_railway_stations)}}
  
  
  # -------------------------------------
  # bus stations
  # -------------------------------------   
  
    # -- check zoom level
    if(map$zoom() >= setting("bus_stations_level")){
    
      # -- get stations
    ctx_bus_stations <- bounding_box(locations$bus_stations, map$bounds())
    cat("-- bus_stations =", nrow(ctx_bus_stations), "\n")
    
    # -- turn bus stations into locations & merge
    if(nrow(ctx_bus_stations) > 0){
      
      ctx_bus_stations <- bus_to_location(ctx_bus_stations)
      ctx_locations <- rbind(ctx_locations, ctx_bus_stations)}}
  
  
  # -------------------------------------
  # return
  # -------------------------------------
  cat(">> output dim =", dim(ctx_locations), "\n")
  ctx_locations
  
}
