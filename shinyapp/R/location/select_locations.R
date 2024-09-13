
# locations = c(locations, list('airports' = airports,
#                               'seaports' = seaports,
#                               'railway_stations' = railway_stations,
#                               'bus_stations' = bus_stations))
# 
# pattern = list(id = c(...),
#                type = "city",
#                been.there = TRUE)
#
#
# result = c("locations", "airports", "seaports", "railway_stations", "bus_stations")


select_locations <- function(locations, pattern, result){
  
  # -- get values
  location_items <- locations$items()
  airport_items <- locations$airports
  seaport_items <- locations$seaports()
  railway_items <- locations$railway_stations
  bus_items <- locations$bus_stations

  # -- init (return value)
  s_result <- data.frame()
  s_locations <- data.frame()
  s_airports <- data.frame()
  
  # -- locations
  if("locations" %in% result){
    
    # -- id
    if("id" %in% names(pattern))
      s_locations <- location_items[location_items$id %in% pattern[['id']], ]
      
    else {
      
      # -- get classes
      classes <- sapply(pattern, class)

      # -- init
      query <- vector()
      
      # -- case %in% 
      if("character" %in% classes)
        query <- c(query, paste(paste0("location_items$", names(pattern[classes == "character"])), "%in%", paste0("'", pattern[classes == "character"], "'")))
      
      # -- case logical
      if("logical" %in% classes)
        query <- c(query, paste(paste0("location_items$", names(pattern[classes == "logical"])), "==", pattern[classes == "logical"]))
      
      # -- concatenate
      if(length(query) > 1)
        query <- paste(query, collapse = " & ")
      
      # -- query
      if(length(query) > 0)
        s_locations <- location_items[eval(parse(text = query)), ]}}
  
  
  # -- airports
  if("airports" %in% result){

    # -- id
    if("id" %in% names(pattern)){
      
      # -- slice
      s_airports <- airport_items[airport_items$id %in% pattern[['id']], ]
      
      # -- make locations from airports
      if(dim(s_airports)[1] > 0)
        s_airports <- airport_to_location(s_airports)}}
  
  
  # -- merge selections
  if(nrow(s_locations) > 0)
    s_result <- s_locations
  
  if(nrow(s_airports) > 0)
    s_result <- rbind(s_result, s_airports)
    
  
  # -- return
  cat("[select_locations] output dim =", dim(s_result),"\n")
  s_result
  
}