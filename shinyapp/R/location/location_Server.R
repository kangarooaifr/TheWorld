

# ------------------------------------------------------------------------------
# Server logic
# ------------------------------------------------------------------------------

location_Server <- function(id, r, path) {
  moduleServer(id, function(input, output, session) {
    
    # -- get namespace
    ns <- session$ns
    
    # -- load countries
    countries <- read.csv(file = file.path(path$resources, "countries.csv"), encoding = "UTF-8")
    
    # -- id
    kitems_id <- "location"
    
    # -- launch kitems sub module
    kitems::kitemsManager_Server(id = kitems_id, r, path$data)
    
    # -- items name
    r_items <- kitems::items_name(id = kitems_id)
    r_data_model <- kitems::dm_name(id = kitems_id)
    r_trigger_add <- kitems::trigger_add_name(id = kitems_id)
    r_trigger_delete <- kitems::trigger_delete_name(id = kitems_id)
    
    
    # -- Observe map click
    observeEvent(r$map_click, {
      
      output$location_panel <- if(is.null(r$map_click))
        NULL
      else 
        renderUI(
          
          wellPanel(
            h4("Location"),
            p("Coordinates:"), 
            tags$ul(tags$li("long =", r$map_click[1]), 
                    tags$li("lat =", r$map_click[2])),
            actionButton(inputId = ns("add_location"), label = "", icon = icon("plus"))))
      
    }, ignoreNULL = FALSE)
    
    
    # -- Observer add location button
    observeEvent(input$add_location, {
      
      # -- location form
      output$add_location_form <- renderUI(
        
        wellPanel(
          
          textInput(inputId = ns("name"), label = "Name"),
          selectizeInput(inputId = ns("type"), label = "Type", choices = r[[r_items]]()$type, options = list(create = TRUE)),
          selectizeInput(inputId = ns("country"), label = "Country", choices = countries$country.en, options = list(create = FALSE)),
          selectizeInput(inputId = ns("state"), label = "State", choices = r[[r_items]]()$state, options = list(create = TRUE)),
          textInput(inputId = ns("zip.code"), label = "Zip code"),
          selectizeInput(inputId = ns("city"), label = "City", choices = r[[r_items]]()$city, options = list(create = TRUE)),
          textInput(inputId = ns("address"), label = "Address"),
          textInput(inputId = ns("comment"), label = "Comment"),
          checkboxInput(inputId = ns("been.there"), label = "Been there", value = FALSE),
          checkboxInput(inputId = ns("wish.list"), label = "Wish list", value = FALSE),
          
          actionButton(inputId = ns("confirm_add_location"), label = "Create")))
      
    })
    
    
    # -- Observer add location button
    observeEvent(input$confirm_add_location, {
      
      # -- update output
      output$add_location_form <- NULL
      
      cat(">>> DEBUG input \n")
      
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
      
      str(input_values)
      
      # -- create item
      item <- kitems::item_create(values = input_values, data.model = r[[r_data_model]]())
    
      # -- call trigger
      r[[r_trigger_add]](item)
      
    })
    
    
    # -- Observer show location button
    observeEvent({
      input$show_location
      r[[r_items]]()
    }, {
      
      # -- option
      cat("[location] Show location, option =", input$show_location_option, "\n")
      
      # -- get the data & apply option
      locations <- r[[r_items]]()
      if(input$show_location_option == "been-there")
        locations <- locations[locations$been.there, ]
      else if(input$show_location_option == "wish-list")
        locations <- locations[locations$wish.list, ]
      
      cat("-- apply filter output dim =", dim(locations)[1], "obs. \n")
      
      # -- check dim
      if(dim(locations)[1] > 0){
        
        # -- get area bounds
        lng_min <- min(locations$lng)
        lng_max <- max(locations$lng)
        lat_min <- min(locations$lat)
        lat_max <- max(locations$lat)
        
        # -- get proxy map
        r$proxymap %>%
          
          # -- cleanup
          clearGroup("location") %>%
          
          # -- Add markers
          addMarkers(data = locations,
                     lng = ~lng,
                     lat = ~lat,
                     group = "location",
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
                     clusterOptions = NULL
          ) %>%
          
          # -- set view
          flyToBounds(lng1 = lng_min, lat1 = lat_min, lng2 = lng_max, lat2 = lat_max, options = list(padding = c(50, 50)))
      }
      
    })
    
    
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
