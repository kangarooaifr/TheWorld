

# -- function definition
add_markers <- function(locations, map_proxy, group_id, icons){
  
  cat("[wm_add_markers] Add markers to map \n")
  
  # -- check dim
  if(dim(locations)[1] == 0)
    return(NULL)
  
  # -- update map (proxy)
  map_proxy %>%
    
    # -- cleanup
    clearGroup(group_id) %>%
    
    # -- Add markers
    addAwesomeMarkers(data = locations,
                      lng = ~lng,
                      lat = ~lat,
                      group = group_id,
                      icon = ~icons[icon],
                      label = ~name,
                      popup = ~popup,
                      clusterOptions = NULL)
  
}
