

# ------------------------------------------------------------------------------
# Server logic
# ------------------------------------------------------------------------------

trip_Server <- function(id, r, path) {
  moduleServer(id, function(input, output, session) {
    
    # -- get namespace
    ns <- session$ns
    
    # -- id
    kitems_id <- "trip"
    
    # id, trip.id, name, status, description, comment
    # date.start, date.end, cities, countries >> computed from other tables based on trip.id
    # status = draft, planned, inwork, done
    
    # -- launch kitems sub module
    kitems::kitemsManager_Server(id = kitems_id, r, path$data)
    
    # -- items name
    r_items <- kitems::items_name(id = kitems_id)
    r_data_model <- kitems::dm_name(id = kitems_id)
    r_trigger_add <- kitems::trigger_add_name(id = kitems_id)
    r_trigger_delete <- kitems::trigger_delete_name(id = kitems_id)
    
    
    # -------------------------------------
    # Transport management
    # -------------------------------------
    
    # -- id
    transport_kitems_id <- "transport"
    
    # -- launch kitems sub module
    kitems::kitemsManager_Server(id = transport_kitems_id, r, path$data)
    
    # -- to be defined: functions
    # cache_trip_id
    # cache_route_id
    
    
    # -------------------------------------
    # -- create tables flight, sea, road to instantiate in route!
    
    
    # -- accommodation
    # id, trip.id, location.id, date.start, date.end, checkin, checkout, breakfast, comment

    
  })
}
