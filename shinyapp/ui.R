

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
                h2("Dashboard"),
                
                fluidRow(
                    column(width = 2,
                           search_Input("map"),
                           location_panel_UI("locationmngr"),
                           show_location_BTN("locationmngr"),
                           #whereGone_UI("wheregone"),
                           flights_UI("flights"),
                           countries_UI("countries"),
                           tracks_UI("tracks")),
                    
                    column(width = 10,
                           map_UI("map")))),
        
        
        # -- kitems admin (location)
        tabItem(tabName = "location",
                
                # -- Admin UI
                fluidRow(
                  column(width = 12,
                         kitems::admin_ui("locationmngr-location")))),
        
        # -- kitems admin (transport)
        tabItem(tabName = "transport",
                
                # -- Admin UI
                fluidRow(
                  column(width = 12,
                         kitems::admin_ui("transportmngr-transport"))))
        
    )
)


# -- Put them together into a dashboard

dashboardPage(
    
    dashboardHeader(title = "TheWorld"),
    sidebar,
    body
    
)

