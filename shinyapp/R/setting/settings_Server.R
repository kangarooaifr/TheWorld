

# ------------------------------------------------------------------------------
# Shiny module: Server logic
# ------------------------------------------------------------------------------

settings_Server <- function(id, path) {
  moduleServer(id, function(input, output, session) {
    
    # -- namespace
    ns <- NS(id)
    
    # --------------------------------------------------------------------------
    # Parameters
    # --------------------------------------------------------------------------
    
    # -- trace
    MODULE <- paste0("[", id, "]")
    cat(MODULE, "Starting module server... \n")
    
    # -- file
    filename <- "settings.rds"
    
    
    # --------------------------------------------------------------------------
    # Init
    # --------------------------------------------------------------------------
    
    # -- Init settings
    settings <<- init_settings(path = path$settings, filename)
    
    
    # --------------------------------------------------------------------------
    # Auto save
    # --------------------------------------------------------------------------
    
    # -- Event: settings
    observe({
      
      saveRDS(settings, file = file.path(path$settings, filename))
      cat(MODULE, "Settings have been (auto) saved. \n")
      
    }) %>% bindEvent(settings, ignoreNULL = TRUE,  ignoreInit = TRUE)
    
    
    # --------------------------------------------------------------------------
    # Output
    # --------------------------------------------------------------------------
    
    # -- Admin UI
    output$settings_admin <- renderUI({
      
      # -- Generate observers
      obs <- lapply(settings$name, setting_observer, input)
      
      # -- Generate inputs
      apply(settings, 1, setting_input, ns)
      
    })
    
  })
}
