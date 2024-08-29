

# -- function definition
add_markers <- function(locations, map_proxy, icons){
  
  cat("[wm_add_markers] Add markers to map \n")
  
  # -- check dim
  if(dim(locations)[1] == 0)
    return(NULL)

  # -- update map (proxy)
  map_proxy %>%
    
    # -- Add markers
    addAwesomeMarkers(data = locations,
                      lng = ~lng,
                      lat = ~lat,
                      layerId = ~as.character(id),
                      group = ~type,
                      icon = ~icons[icon],
                      label = ~name,
                      popup = ~popup,
                      clusterOptions = NULL) %>%
    
    # -- Map overlay checkbox (hide / show groups)
    addLayersControl(overlayGroups = unique(locations$type))
  
}
