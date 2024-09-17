

# ------------------------------------------------------------------------------
# Module Server logic
# ------------------------------------------------------------------------------

contextualLocation_Server <- function(id, map, locations, exclude, icons) {
  moduleServer(id, function(input, output, session) {
    
    # --------------------------------------------------------------------------
    # Parameters
    # --------------------------------------------------------------------------
    
    # -- trace
    MODULE <- paste0("[", id, "]")
    cat(MODULE, "Starting module server... \n")
    
    # get namespace
    ns <- session$ns
    
    # -- contextual locations
    cache_contextual <- reactiveVal(NULL)
    add_to_map <- reactiveVal(NULL)
    remove_from_map <- reactiveVal(NULL) 
    
    
    # --------------------------------------------------------------------------
    # Select locations
    # --------------------------------------------------------------------------
    
    # -- Select contextual locations (given map bounds)
    ctx_locations <- reactive(
      
      # -- check zoom level
      if(map$zoom() >= setting("contextual_locations_level")){
        
        # -- get contextual locations
        x <- contextual_locations(map, locations)
        
        # -- Remove locations already in exclude list
        x <- x[!x$id %in% exclude()$id, ]
        
      } else {
        
        # -- check cache content
        if(!is.null(cache_contextual())){
          
          cat(MODULE, "Clear contextual locations \n")
          data.frame()
          
        } else NULL
        
      }) %>% bindEvent(map$bounds())
    
    
    # --------------------------------------------------------------------------
    # Manage cache, add & remove
    # --------------------------------------------------------------------------
    
    # -- Define locations & groups to add / remove (vs cached ones)
    observe({        
      
      # -- Extract locations to remove & add
      locations_to_remove <- cache_contextual()[!cache_contextual()$id %in% ctx_locations()$id, ]$id
      locations_to_add <- ctx_locations()[!ctx_locations()$id %in% cache_contextual()$id, ]
      
      cat(MODULE, "Computing actions on map \n")
      cat("-- locations to add =", length(locations_to_add), "\n")
      cat("-- locations to remove =", length(locations_to_remove), "\n")
      
      # -- Extract groups to remove (not in exclude!)
      groups_in_cache <- unique(cache_contextual()[!cache_contextual()$id %in% ctx_locations()$id, ]$type)
      groups_to_remove <- groups_in_cache[!groups_in_cache %in% ctx_locations()$type]
      groups_to_remove <- groups_to_remove[!groups_to_remove %in% exclude()$type]
      
      # -- store in cache (or clear cache!)
      cache_contextual(
        
        if(nrow(ctx_locations()) != 0)
          ctx_locations()[c("id", "type")]
        
        else
          NULL)
      
      # -- trigger add (NULL won't trigger add marker!)
      add_to_map(
        
        # -- check dim
        if(nrow(locations_to_add) > 0)
          list(locations = locations_to_add, 
               groups = unique(locations_to_add$type))
        
        else NULL)
      
      # -- trigger remove (NULL won't trigger add marker!)
      remove_from_map(
        
        # -- check dim
        if(length(locations_to_remove) > 0)
          
          list(locations = locations_to_remove,
               groups = groups_to_remove)
        else NULL)
      
    }) %>% bindEvent(ctx_locations())
    
    
    # --------------------------------------------------------------------------
    # Add / remove markers
    # --------------------------------------------------------------------------
    
    # -- Remove locations & groups from map
    observe({
      
      cat(MODULE, "Remove markers from map, nb =", length(remove_from_map()$locations), "\n")
      removeMarker(map$proxy, layerId = as.character(remove_from_map()$locations))
      
      # -- Remove from cache
      map_layers_control(map$layer_control, overlayGroups = remove_from_map()$groups, remove = TRUE)
      
    }) %>% bindEvent(remove_from_map())
    
    
    # -- Add locations & groups to map
    observe({
      
      cat(MODULE, "Add markers on map, nb =", nrow(add_to_map()$locations), "\n")
      
      # -- get value
      locations_to_add <- add_to_map()$locations
      
      # -- add icon & popup columns
      locations_to_add <- location_icon(locations_to_add)
      locations_to_add$popup <- location_popups(locations_to_add, ns, activity = NULL)
      
      # -- display on map
      add_markers(locations_to_add, map_proxy = map$proxy, icons = icons)
      
      # -- Add in cache
      map_layers_control(map$layer_control, overlayGroups = add_to_map()$groups)
      
    })  %>% bindEvent(add_to_map())
    
    
    # --------------------------------------------------------------------------
    # Return
    # --------------------------------------------------------------------------
    
    
  })
}
