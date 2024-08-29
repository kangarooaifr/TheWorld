
# location: NULL will end up in all location$foo to be NULL as well (create mode)

# choices: list of 'type', 'country', 'state', 'city'


# -- function definition
location_modal <- function(location = NULL, lng = NULL, lat = NULL, choices, ns){

  # -- init
  update <- FALSE

  # -- check param (update)
  if(!is.null(location)){
    
    # -- update coord from location
    lng <- location$lng
    lat <- location$lat
    
    # -- return
    update <- TRUE
    
  }
  
  # -- init onInitialize option
  onInitialize <- if(update)
    NULL
  else 
    I('function() { this.setValue(""); }')
  
  cat("[location_modal] update =", update, "\n")
  
  
  # -- return modal
  modalDialog(
    
    p("Coordinates:"), 
    
    tags$ul(tags$li("long =", lng), 
            tags$li("lat =", lat)),
    
    # -- name
    textInput(inputId = ns("name"), 
              label = "Name",
              value = location$name),
    
    # -- type
    selectizeInput(inputId = ns("type"), 
                   label = "Type", 
                   choices = choices['type'],
                   selected = location$type,
                   options = list(placeholder = 'Please select an option below',
                                  onInitialize = onInitialize,
                                  create = TRUE)),
    
    # -- country          
    selectizeInput(inputId = ns("country"), 
                   label = "Country", 
                   choices = choices['country'], 
                   selected = location$country,
                   options = list(placeholder = 'Please select an option below',
                                  onInitialize = onInitialize,
                                  create = FALSE)),
    
    # -- state / region
    selectizeInput(inputId = ns("state"), 
                   label = "State", 
                   choices = choices['state'], 
                   selected = location$state,
                   options = list(placeholder = 'Please select an option below',
                                  onInitialize = onInitialize,
                                  create = TRUE)),
    
    # -- zip code
    textInput(inputId = ns("zip.code"), 
              label = "Zip code",
              value = location$zip.code),
    
    # -- city
    selectizeInput(inputId = ns("city"), 
                   label = "City", 
                   choices = choices['city'], 
                   selected = location$city,
                   options = list(placeholder = 'Please select an option below',
                                  onInitialize = onInitialize,
                                  create = TRUE)),
    
    # -- address
    textInput(inputId = ns("address"), 
              label = "Address",
              value = location$address),
    
    # -- comment
    textInput(inputId = ns("comment"), 
              label = "Comment",
              value = location$comment),
    
    # -- been.there
    checkboxInput(inputId = ns("been.there"), 
                  label = "Been there", 
                  value = location$been.there),
    
    # -- wish.list
    checkboxInput(inputId = ns("wish.list"), 
                  label = "Wish list", 
                  value = location$wish.list),
    
    # -- title
    title = paste(ifelse(update, "Update", "Create"), "location"),
    
    # -- actions
    footer = tagList(
      modalButton("Cancel"),
      actionButton(inputId = ns(paste("confirm", ifelse(update, "update", "add"), "location", sep = "_")), 
                   label = ifelse(update, "Update", "Create"))))
  
}
