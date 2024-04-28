

# --------------------------------------------------------------------------------
# This is the user-interface definition of the Shiny web application
# --------------------------------------------------------------------------------

# -- Define Sidebar UI

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("World map", tabName = "worldmap", icon = icon("earth-americas"), selected = TRUE),
        menuItem("Trip planner", tabName = "trip-planner", icon = icon("compass"))),
    
    
    # -- add dynamic section
    sidebarMenu(tabName = "kitems", sidebarMenuOutput("menu")),
    
    collapsed = TRUE)


# -- Define Body UI

body <- dashboardBody(
    
    tabItems(
    
        tabItem(tabName = "worldmap",
                
                fluidRow(
                    column(width = 3,
                           search_Input("map"),
                           location_panel_UI("locationmngr"),
                           route_UI("routemngr"),
                           country_UI("country"),
                           track_UI("track")),
                    
                    column(width = 9,
                           country_INPUT("map"),
                           map_UI("map")))),
        
        # -- Trip planner
        tabItem(tabName = "trip-planner",
                
                fluidRow(
                  column(width = 3,
                         trip_panel_UI("tripmngr")),
                  
                  column(width = 9,
                         p("map here"),
                         tmp_trip_ui("tripmngr")))),
        
        
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
        
        # -- kitems admin (transport)
        tabItem(tabName = "transport",
                
                # -- Admin UI
                fluidRow(
                  column(width = 12,
                         kitems::admin_ui("tripmngr-transport")))),
        
        # -- kitems admin (accomodation)
        tabItem(tabName = "accomodation",
                
                # -- Admin UI
                fluidRow(
                  column(width = 12,
                         kitems::admin_ui("tripmngr-accomodation"))))
        
    )
)


# -- Put them together into a dashboard

dashboardPage(
    
    dashboardHeader(title = "TheWorld"),
    sidebar,
    body
    
)

