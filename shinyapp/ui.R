

# --------------------------------------------------------------------------------
# This is the user-interface definition of the Shiny web application
# --------------------------------------------------------------------------------

# -- Define Sidebar UI

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"), selected = TRUE)),
    
    # -- add dynamic section
    sidebarMenu(tabName = "kitems", sidebarMenuOutput("menu")),
    
    collapsed = TRUE)


# -- Define Body UI

body <- dashboardBody(
    
    tabItems(
    
        tabItem(tabName = "dashboard",
                
                fluidRow(
                    column(width = 2,
                           search_Input("map"),
                           location_panel_UI("locationmngr"),
                           route_UI("routemngr"),
                           country_UI("country"),
                           track_UI("tracks")),
                    
                    column(width = 10,
                           country_INPUT("map"),
                           map_UI("map")))),
        
        
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
        
        # -- kitems admin (route)
        tabItem(tabName = "trip",
                
                # -- Admin UI
                fluidRow(
                  column(width = 12,
                         kitems::admin_ui("tripmngr-trip"))))
        
    )
)


# -- Put them together into a dashboard

dashboardPage(
    
    dashboardHeader(title = "TheWorld"),
    sidebar,
    body
    
)

