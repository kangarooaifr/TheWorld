

# ------------------------------------------------------------------------------
# User-interface definition of the Shiny web application
# ------------------------------------------------------------------------------

# -- Define sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("World map", tabName = "worldmap", icon = icon("earth-americas"), selected = TRUE),
    menuItem("Trip planner", tabName = "tripplanner", icon = icon("earth-americas"))),
  
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
    tabItem(tabName = "worldmap",
            
            fluidRow(
              
              # -- sidebar left
              column(width = 3,
                     
                     # -- search
                     search_Input("map"),
                     
                     # -- main
                     tabsetPanel(id = "selected_tab",
                              
                                 # -- world map tab
                                 tabPanel("World map", value = "world_map",
                                          country_INPUT("map"),
                                          location_panel_UI("locationmngr"),
                                          # -- Commented #90
                                          # Keep until moved to a different code location
                                          route_UI("routemngr"),
                                          country_UI("country"),
                                          # -- Commented #91
                                          # Keep until moved to a different code location
                                          # track_UI("track")
                                          ),
                                 
                                 # -- trip planner tab
                                 tabPanel("Trip planner", value = "trip_planner",
                                          trip_panel_UI("tripmngr")))),
              
              # -- main area (map)
              column(width = 9,
                     freeze_INPUT("map"),
                     map_UI("map")))),
    
    
    # --------------------------------------------------------------------------
    # Trip planner section
    # --------------------------------------------------------------------------
    
    # -- main
    tabItem(tabName = "tripplanner",
            
            fluidRow(
              
              # -- sidebar left
              column(width = 3,
                     
                     # -- search
                     search_Input("xxx")),
                     
              # -- main area (xxx)
              column(width = 9,
                     freeze_INPUT("xxx"),
                     map_UI("xxx")))),
    
    
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
