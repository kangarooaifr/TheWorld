

# --------------------------------------------------------------------------------
# This is the user-interface definition of the Shiny web application
# --------------------------------------------------------------------------------

# -- Define Sidebar UI

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"), selected = TRUE)),
    collapsed = TRUE)


# -- Define Body UI

body <- dashboardBody(
    
    tabItems(
    
        tabItem(tabName = "dashboard",
                h2("Dashboard"),
                
                fluidRow(
                    column(width = 2,
                           search_Input("map"),
                           whereGone_UI("wheregone"),
                           flights_UI("flights"),
                           countries_UI("countries"),
                           tracks_UI("tracks")
                           ),
                    column(width = 10,
                           map_UI("map")
                           )
                )
                
        )
    )
)


# -- Put them together into a dashboard

dashboardPage(
    
    dashboardHeader(title = "TheWorld"),
    sidebar,
    body
    
)

