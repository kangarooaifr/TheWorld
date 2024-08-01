

# ------------------------------------------------------------------------------
# Shiny module: dependencies
# ------------------------------------------------------------------------------

# -- Library
library(geosphere)
library(dplyr)


# ------------------------------------------------------------------------------
# Module Server logic
# ------------------------------------------------------------------------------

route_Server <- function(id, r, path, map_proxy) {
  moduleServer(id, function(input, output, session) {
    
    # get namespace
    ns <- session$ns
    
    # -- id
    group_id <- "routes"
    
    # -------------------------------------
    # Data manager (routes)
    # -------------------------------------
    
    # -- module id
    kitems_id <- "route"
    
    # -- launch kitems sub module
    kitems::kitemsManager_Server(id = kitems_id, r, path$data)
    
    # -- items name
    r_items <- kitems::items_name(id = kitems_id)
    r_data_model <- kitems::dm_name(id = kitems_id)
    r_trigger_add <- kitems::trigger_add_name(id = kitems_id)
    r_trigger_delete <- kitems::trigger_delete_name(id = kitems_id)
    r_trigger_create <- kitems::trigger_create_name(id = kitems_id)
    
    
    # -------------------------------------
    # Add route
    # -------------------------------------
    
    observeEvent(input$add_route, {
      
      # -- display modal
      showModal(modalDialog(
        
        title = "Select route",
        
        "Select the origin.",
        
        # -- create the selectizeInput with empty choices (perfo)
        selectizeInput(inputId = ns("select_origin"), label = "Origin", choices = NULL),
        selectizeInput(inputId = ns("select_destination"), label = "Destination", choices = NULL),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton(inputId = ns("confirm_route"), label = "OK")
        )
      ))
      
      # -- compute available choices:
      if(input$transport_mode == "air"){
        
        choices <- r$airports$id
        names(choices) <- r$airports$iata
        
      } else if(input$transport_mode == "sea"){
        
        choices <- r$seaports()$id
        names(choices) <- paste(r$seaports()$city, r$seaports()$name, sep = "-")
        
      }
      choices <- as.list(choices)
      
      # -- update the selectizeInput with choices (server)
      updateSelectizeInput(session, 'select_origin', choices = choices, server = TRUE)
      updateSelectizeInput(session, 'select_destination', choices = choices, server = TRUE)
      
      # -- observe modal action btn
      observeEvent(input$confirm_route, {
        
        # -- close
        removeModal()
        
        # -- declare cache function
        cache_origin <<- function() {
          cat("[cache_origin] origin =", input$select_origin, "\n ")
          input$select_origin}
        
        # -- declare cache function
        cache_destination <<- function() {
          cat("[cache_origin] origin =", input$select_destination, "\n ")
          input$select_destination}
        
        # -- call trigger
        r[[r_trigger_create]](r[[r_trigger_create]]() + 1)
        
      })
      
    })
    
    
    # -------------------------------------
    # Search
    # -------------------------------------
    
    # -- declare trigger
    r$route_search_string <- NULL
    
    
    # -- observe trigger & expose connector
    r$route_search_result <- eventReactive(r$route_search_string, {
      
      # -- check for empty string (otherwise the whole df is returned)
      if(identical(r$route_search_string, ""))
        NULL
      
      else {
        
        cat("[route] Trigger, search string =", r$route_search_string, "\n")
        
        # -- filter items
        result <- r[[r_items]]() %>%
          filter_all(any_vars(grepl(r$route_search_string, .)))

        # -- air
        if(r$route_search_string == 'air'){
          
          result <- merge(result, r$airports[c("id", "iata")], by.x = "origin", by.y = "id")
          result <- merge(result, r$airports[c("id", "iata")], by.x = "destination", by.y = "id")
          names(result)[names(result) == 'iata.x'] <- 'origin.code'
          names(result)[names(result) == 'iata.y'] <- 'destination.code'
          
          # -- sea
        } else if(r$route_search_string == 'sea'){
          
          result <- merge(result, r$seaports()[c("id", "name")], by.x = "origin", by.y = "id")
          result <- merge(result, r$seaports()[c("id", "name")], by.x = "destination", by.y = "id")
          names(result)[names(result) == 'name.x'] <- 'origin.code'
          names(result)[names(result) == 'name.y'] <- 'destination.code'
          
        }
        
        # -- return
        result
        
      }
    })
    
    
    # -------------------------------------
    # Select
    # -------------------------------------
    # query pattern:
    # - character 'all' keeps all items
    # - numeric vector to select by id's
    # - character string to select by column, type=air
    
    # -- declare trigger
    r$route_select <- NULL
    
    # -- observe
    r$selected_route <- eventReactive(r$route_select, {
      
      cat("[TRIGGER] Select route: \n")
      str(r$route_select)
      
      # -- init
      routes <- r[[r_items]]()
      query <- r$route_select
      
      # -- check: keep all items
      if(is.character(query) && query == 'all'){
        
        cat("-- select all \n")
        
        
      } else if(is.numeric(query)){
        
        # -- unselect: empty num
        if(identical(query, numeric(0)))
          routes <- data.frame(0)
        
        else {
          # -- apply selection
          cat("-- select by id \n")
          routes <- routes[routes$id %in% query, ]}
        
        
      } else {
        
        # -- split 'key=value' into c(key, value)
        query <- unlist(strsplit(query, split = "="))
        cat("-- select by key,", query[1], "=", query[2], "\n")
        
        # -- select
        routes <- routes[routes$query[1] %in% query[2], ]
        
      }
      
      cat("-- output dim =", dim(routes), "\n")
      
      # -- return
      routes
      
    })
    
    
    # -- observe: activity tab
    observeEvent(r$activity(), {
      
      if(r$activity() == "world_map")
        r$route_select <- numeric(0)
      
    }, ignoreInit = TRUE)
    
    
    # -------------------------------------
    # Update map
    # -------------------------------------
    
    observeEvent(r$selected_route(), {
      
      # -- init
      routes <- r$selected_route()
      cat("[route] Update map, selected routes =", length(routes$id), "\n")
      
      # -- clear map (group)
      r[[map_proxy]] %>%
        clearGroup(group_id)
      
      # -- check (otherwise unselect, so clearGroup is enough)
      if(length(routes$id) > 0){
        
        # -- Helper: add route to map
        addroute <- function(id){
          
          # -- get route & parameters
          route <- routes[routes$id == id, ]
          origin <- route$origin
          destination <- route$destination
          type  <- route$type
          
          cat("-- route origin", origin, "/ destination", destination, "/ type =", type, "\n")
          
          # -- mode: air
          if(type == 'air'){
            
            # -- get names
            origin_name <- r$airports[r$airports$id == origin, 'name']
            destination_name <- r$airports[r$airports$id == destination, 'name']
            
            # -- compute great circle route
            route <- gcIntermediate(p1 = airport_coord(r$airports, id = origin), 
                                    p2 = airport_coord(r$airports, id = destination), 
                                    n = 100, 
                                    addStartEnd = TRUE)
            
            # -- add fight route
            r[[map_proxy]] %>%
              addPolylines(data = route, group = group_id, color = "purple", weight = 2, popup = route_labels(origin_name, destination_name))
            
            
            # -- mode: sea
          } else if(type == 'sea'){
            
            # -- get names
            origin_name <- r$seaports()[r$seaports()$id == origin, 'name']
            destination_name <- r$seaports()[r$seaports()$id == destination, 'name']
            
            # -- compute route
            route <- data.frame(lng = c(r$seaports()[r$seaports()$id == origin, 'lng'], r$seaports()[r$seaports()$id == destination, 'lng']),
                                lat = c(r$seaports()[r$seaports()$id == origin, 'lat'], r$seaports()[r$seaports()$id == destination, 'lat']))
            
            # -- add sea route
            r[[map_proxy]] %>%
              addPolylines(lng = route$lng, lat = route$lat, group = group_id, color = "purple", weight = 2, popup = route_labels(origin_name, destination_name))
            
          }
          
        }
        
        # -- apply helper to routes df
        cat("[routes] Looping over route list... \n")
        lapply(routes$id, addroute)
        
      }
      
    })
    
    
    # -------------------------------------
    # Display routes
    # -------------------------------------

    # -- add path to map
    # observeEvent(
    #   {r[[r_items]]()
    #     input$transport_mode}, {
    #       
    #       cat("[routeS] Updating route segments \n")
    #       
    #       # -- clear map (group)
    #       r$proxymap %>%
    #         clearGroup(group_id)
    #       
    #       # -- get & filter data
    #       routes <- r[[r_items]]()
    #       routes <- routes[routes$type == input$transport_mode, ]
    #       
    #       # -- Helper: add route to map
    #       addroute <- function(routeid){
    #         
    #         # -- get route parameters
    #         origin <- routes[routes$id == routeid, 'origin']
    #         destination <- routes[routes$id == routeid, 'destination']
    #         
    #         cat("-- route origin", origin, "/ destination", destination, "\n")
    #         
    #         # -- mode: air
    #         if(input$transport_mode == 'air'){
    #           
    #           # -- get names
    #           origin_name <- r$airports[r$airports$id == origin, 'name']
    #           destination_name <- r$airports[r$airports$id == destination, 'name']
    #           
    #           # -- compute great circle route
    #           route <- gcIntermediate(p1 = airport_coord(r$airports, id = origin), 
    #                                   p2 = airport_coord(r$airports, id = destination), 
    #                                   n = 100, 
    #                                   addStartEnd = TRUE)
    #           
    #           # -- mode: sea
    #         } else if(input$transport_mode == 'sea'){
    #           
    #           # -- get names
    #           origin_name <- r$seaports()[r$seaports()$id == origin, 'name']
    #           destination_name <- r$seaports()[r$seaports()$id == destination, 'name']
    #           
    #           # -- compute route
    #           route <- data.frame(lng = c(r$seaports()[r$seaports()$id == origin, 'lng'], r$seaports()[r$seaports()$id == destination, 'lng']),
    #                               lat = c(r$seaports()[r$seaports()$id == origin, 'lat'], r$seaports()[r$seaports()$id == destination, 'lat']))
    #           
    #         }
    #         
    #         # -- add to proxy map
    #         if(input$transport_mode == "air")
    #           r$proxymap %>%
    #           
    #           # add fight route
    #           addPolylines(data = route, group = group_id, color = "purple", weight = 2, popup = route_labels(origin_name, destination_name))
    #         
    #         else if(input$transport_mode == "sea")
    #           
    #           r$proxymap %>%
    #           
    #           addPolylines(lng = route$lng, lat = route$lat, group = group_id, color = "purple", weight = 2, popup = route_labels(origin_name, destination_name))
    #         
    #       }
    #       
    #       # -- apply helper to routes df
    #       cat("[routes] Looping over route list... \n")
    #       lapply(routes$id, addroute)
    #       
    #     })
    
    
    # -------------------------------------
    # Hide / Show
    # -------------------------------------
    
    # -- Observe checkbox
    observeEvent(input$hide_show, 
      hide_show(proxy = r[[map_proxy]], id = group_id, show = input$hide_show), ignoreInit = TRUE)
    
  })
}

