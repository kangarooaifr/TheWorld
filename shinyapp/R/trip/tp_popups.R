

# -- function definition
tp_popups <- function(locations, ns, type = 'selected'){
  
  # -- return
  sprintf(
    paste0(

      if(type == 'selected')

        # -- remove from trip
        actionLink(inputId = "remove_%s",
                   label =  "Remove from trip",
                   onclick = sprintf('Shiny.setInputValue(\"%s\", this.id, {priority: \"event\"})',
                                     ns("remove_from_trip")))

      else

        # -- add to trip
        actionLink(inputId = "add_%s",
                   label =  "Add to trip",
                   onclick = sprintf('Shiny.setInputValue(\"%s\", this.id, {priority: \"event\"})',
                                     ns("add_to_trip")))),

    locations$id)
  
}
