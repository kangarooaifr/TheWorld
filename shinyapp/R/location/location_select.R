

# -- function definition
location_select <- function(r, id, location_id){
  
  cat("[location_select] Select locations, nb =", length(location_id), "\n")
  
  # -- get item reference
  r_items <- kitems::items_name(id)
  locations <- r[[r_items]]()
  
  # -- apply selection
  locations <- locations[locations$id %in% location_id, ]
  
  # -- check output dim
  if(dim(locations)[1] != length(location_id)){
    
    # -- check for airports
    airports <- r$airports[r$airports$id %in% location_id, ]
    
    # -- make locations from airports
    if(dim(airports)[1] > 0)
      tmp_locations <- airport_to_location(airports)
    
    # -- merge
    locations <- rbind(locations, tmp_locations)}
  
  # -- return
  cat("[location_select] output dim =", dim(locations),"\n")
  locations
  
}
