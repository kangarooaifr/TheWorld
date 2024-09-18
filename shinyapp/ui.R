

# ------------------------------------------------------------------------------
# User-interface definition of the Shiny web application
# ------------------------------------------------------------------------------

# -- Define sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(id = "selected_tab",
    menuItem("World map", tabName = "world_map", icon = icon("earth-americas"), selected = TRUE),
    menuItem("Trip planner", tabName = "trip_planner", icon = icon("earth-americas")),
    menuItem("Options", tabName = "settings", icon = icon("gear"))),
  
  # -- Add dynamic section
  sidebarMenu(tabName = "kitems", sidebarMenuOutput("menu")),
  
  collapsed = TRUE)


# -- Define body
body <- dashboardBody(
  
  # -- include custom CSS
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
  
  # -- body
  tabItems(
    
    # --------------------------------------------------------------------------
    # World map section
    # --------------------------------------------------------------------------
    
    # -- World map
    tabItem(tabName = "world_map",
            
            fluidRow(
              
              # -- sidebar left
              column(width = 3,
                     
                     # -- search
                     map_search_Input("worldmap-world"),
                     
                     # -- locations & countries
                     worldmap_INPUT("worldmap")),
                     
              
              # -- main area (map)
              column(width = 9,
                     map_freeze_INPUT("worldmap-world"),
                     map_UI("worldmap-world")))),
    
    
    # --------------------------------------------------------------------------
    # Trip planner section
    # --------------------------------------------------------------------------
    
    # -- main
    tabItem(tabName = "trip_planner",
            
            fluidRow(
              
              # -- sidebar left
              column(width = 3,
                     
                     # -- search
                     map_search_Input("tripmngr-trip"),
                     trip_panel_UI("tripmngr"),
                     
                     #route_UI("routemngr"),
                     
                     ),
              
                     
              # -- main area (trip)
              column(width = 9,
                     map_freeze_INPUT("tripmngr-trip"),
                     map_UI("tripmngr-trip")))),
    
    
    # --------------------------------------------------------------------------
    # Settings section
    # --------------------------------------------------------------------------
    
    # -- main
    tabItem(tabName = "settings",
            
            settingsAdmin_UI("setting")
            
    ),
    
    
    # --------------------------------------------------------------------------
    # kitems
    # --------------------------------------------------------------------------
    
    # -- kitems admin (location)
    tabItem(tabName = "location",
            
            # -- Admin UI
            fluidRow(
              column(width = 12,
                     kitems::admin_ui("locationmngr-location")))),
    
    # -- kitems admin (route)
    tabItem(tabName = "route",
            
            # -- Admin UI
            fluidRow(
              column(width = 12,
                     kitems::admin_ui("routemngr-route")))),
    
    # -- kitems admin (trip)
    tabItem(tabName = "trip",
            
            # -- Admin UI
            fluidRow(
              column(width = 12,
                     kitems::admin_ui("tripmngr-trip")))),
    
    # -- kitems admin (step)
    tabItem(tabName = "step",
            
            # -- Admin UI
            fluidRow(
              column(width = 12,
                     kitems::admin_ui("tripmngr-step")))),
    
    # -- kitems admin (transport)
    tabItem(tabName = "transport",
            
            # -- Admin UI
            fluidRow(
              column(width = 12,
                     kitems::admin_ui("tripmngr-transport")))),
    
    # -- kitems admin (accommodation)
    tabItem(tabName = "accommodation",
            
            # -- Admin UI
            fluidRow(
              column(width = 12,
                     kitems::admin_ui("tripmngr-accommodation"))))
    
  )
)


# -- Put them together into a dashboard
dashboardPage(
  dashboardHeader(title = "TheWorld"),
  sidebar,
  body)
