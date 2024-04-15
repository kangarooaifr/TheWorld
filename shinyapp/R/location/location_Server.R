

# ------------------------------------------------------------------------------
# Server logic
# ------------------------------------------------------------------------------

location_Server <- function(id, r, path) {
  moduleServer(id, function(input, output, session) {
    
    # -- get namespace
    ns <- session$ns
    
    # -- load countries
    countries <- read.csv(file = file.path(path$resources, "countries.csv"), encoding = "UTF-8")
    
    # -- ids
    kitems_id <- "location"
    group_id <- "locations"
    
    # -- launch kitems sub module
    kitems::kitemsManager_Server(id = kitems_id, r, path$data)
    
    # -- items name
    r_items <- kitems::items_name(id = kitems_id)
    r_data_model <- kitems::dm_name(id = kitems_id)
    r_trigger_add <- kitems::trigger_add_name(id = kitems_id)
    r_trigger_delete <- kitems::trigger_delete_name(id = kitems_id)
    
    # -- icon set
    icons <- awesomeIconList(been.there = makeAwesomeIcon(icon = 'location-dot', iconColor = 'black', library = 'fa', markerColor = 'beige'),
                             wish.list = makeAwesomeIcon(icon = 'location-dot', iconColor = 'black', library = 'fa', markerColor = 'pink'),
                             default = makeAwesomeIcon(icon = 'location-dot', iconColor = 'black', library = 'fa', markerColor = 'blue'))
    
    
    # -------------------------------------
    # Filter: country >>> to be moved to country module!
    # -------------------------------------
    
    # -- update filter choices
    observeEvent(r[[r_items]](), {
      
      locations <- r[[r_items]]()
      choices <- unique(locations$country)
      
      r$filter_country_choices <- choices
      
    })
    
    
    # -- Observe map click
    observeEvent(r$map_click, {
      
      output$location_panel <- if(is.null(r$map_click))
        NULL
      else 
        renderUI(
            
          tagList(
            h4("Location"),
            p("Coordinates:"), 
            
            tags$ul(tags$li("long =", r$map_click[1]), 
                    tags$li("lat =", r$map_click[2])),
            
            # -- name
            textInput(inputId = ns("name"), 
                      label = "Name"),
            
            # -- type
            selectizeInput(inputId = ns("type"), 
                           label = "Type", 
                           choices = r[[r_items]]()$type, 
                           options = list(placeholder = 'Please select an option below',
                                          onInitialize = I('function() { this.setValue(""); }'),
                                          create = TRUE)),
            
            # -- country          
            selectizeInput(inputId = ns("country"), 
                           label = "Country", 
                           choices = countries$country.en, 
                           options = list(placeholder = 'Please select an option below',
                                          onInitialize = I('function() { this.setValue(""); }'),
                                          create = FALSE)),
            
            # -- state / region
            selectizeInput(inputId = ns("state"), 
                           label = "State", 
                           choices = r[[r_items]]()$state, 
                           options = list(placeholder = 'Please select an option below',
                                          onInitialize = I('function() { this.setValue(""); }'),
                                          create = TRUE)),
            
            # -- zip code
            textInput(inputId = ns("zip.code"), 
                      label = "Zip code"),
            
            # -- city
            selectizeInput(inputId = ns("city"), 
                           label = "City", 
                           choices = r[[r_items]]()$city, 
                           options = list(placeholder = 'Please select an option below',
                                          onInitialize = I('function() { this.setValue(""); }'),
                                          create = TRUE)),
            
            # -- address
            textInput(inputId = ns("address"), 
                      label = "Address"),
            
            # -- comment
            textInput(inputId = ns("comment"), 
                      label = "Comment"),
            
            # -- been.there
            checkboxInput(inputId = ns("been.there"), 
                          label = "Been there", 
                          value = FALSE),
            
            # -- wish.list
            checkboxInput(inputId = ns("wish.list"), 
                          label = "Wish list", 
                          value = FALSE),
            
            # -- action btn
            actionButton(inputId = ns("confirm_add_location"), 
                         label = "Create")))
      
    }, ignoreNULL = FALSE)
    

    # -- Observer add location button
    observeEvent(input$confirm_add_location, {

      # -- build values
      input_values <- data.frame(id = NA,
                                 name = input$name,
                                 type = input$type,
                                 lng = r$map_click[1],
                                 lat = r$map_click[2],
                                 country = input$country,
                                 state = input$state,
                                 zip.code = input$zip.code,
                                 city = input$city,
                                 address = input$address,
                                 comment = input$comment,
                                 been.there = input$been.there,
                                 wish.list = input$wish.list)
      
      # -- create item
      item <- kitems::item_create(values = input_values, data.model = r[[r_data_model]]())
    
      # -- call trigger
      r[[r_trigger_add]](item)
      
    })
    
    
    # -- Observe: display button
    # dependencies on button, items and filter
    observeEvent({
      input$display_btn
      r[[r_items]]()
      r$filter_country
    }, {
      
      # -- option
      cat("[location] Show location, option =", input$display_options, "\n")
      
      # -- get the data & apply option
      locations <- r[[r_items]]()
      if(input$display_options == "been-there")
        locations <- locations[locations$been.there, ]
      else if(input$display_options == "wish-list")
        locations <- locations[locations$wish.list, ]
      
      cat("-- apply type filter output dim =", dim(locations)[1], "obs. \n")
      
      # -- filter by country
      if(!is.null(r$filter_country)){
        locations <- locations[locations$country %in% r$filter_country, ]
        cat("-- apply country filter output dim =", dim(locations)[1], "obs. \n")}
      
      # -- check dim
      if(dim(locations)[1] > 0){
        
        # -- get bounding box (args for flyToBounds)
        lng_min <- min(locations$lng)
        lng_max <- max(locations$lng)
        lat_min <- min(locations$lat)
        lat_max <- max(locations$lat)

        # -- prepare marker icon
        locations <- transform(locations, icon = ifelse(been.there, 'been.there', ifelse(wish.list, 'wish.list', 'default')))
        
        # -- update map (proxy)
        r$proxymap %>%
          
          # -- cleanup
          clearGroup(group_id) %>%
          
          # -- Add markers
          addAwesomeMarkers(data = locations,
                            lng = ~lng,
                            lat = ~lat,
                            group = group_id,
                            icon = ~icons[icon],
                            label = ~city,
                            popup = ~sprintf(
                              paste0(
                                "Name:", city,
                                br(),
                                "lng = ", lng,
                                br(),
                                "lat = ", lat,
                                br(),
                                actionLink(inputId = "delete_%s", 
                                           label =  "Delete", 
                                           onclick = sprintf(
                                             'Shiny.setInputValue(\"%s\", this.id, {priority: \"event\"})',
                                             ns("button_click")))), id),
                            #clusterOptions = markerClusterOptions(),
                            clusterOptions = NULL) %>%
          
          # -- set view
          flyToBounds(lng1 = lng_min, lat1 = lat_min, lng2 = lng_max, lat2 = lat_max, options = list(padding = c(50, 50)))
      }
      
    }, ignoreNULL = FALSE)
    
    
    # -------------------------------------
    # Hide / show
    # -------------------------------------
    
    # -- Observe: hide_show
    observeEvent(input$hide_show, {
      
      # -- test input
      if(input$hide_show){
        
        cat("Show group:", group_id, "\n")
        
        # -- update proxy
        r$proxymap %>%
          showGroup(group_id)
        
      } else {
        
        cat("Hide group:", group_id, "\n")
        
        # -- update proxy
        r$proxymap %>%
          hideGroup(group_id)
      }
      
    })
    
    
    # -------------------------------------
    
    # -- Observe: button click from marker popup
    observeEvent(input$button_click, {
      
      # -- get id from input value
      id <- unlist(strsplit(input$button_click, split = "_"))[2]
      cat("Marker popup click, id =", id, "\n")
      
      # -- call trigger
      r[[r_trigger_delete]](id)
      
    })
    
  })
}
