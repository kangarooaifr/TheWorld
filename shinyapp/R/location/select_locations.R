
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
  
  
  # -- check result
  if("locations" %in% result){
    
    # -- init (return value)
    select <- NULL
    
    # -- id
    if("id" %in% names(pattern))
      select <- location_items[location_items$id %in% pattern['id'], ]
    
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
        select <- location_items[eval(parse(text = query)), ]

    }
    
  }
  
  # -- return
  cat("[select_locations] output dim =", dim(select),"\n")
  select
  
}