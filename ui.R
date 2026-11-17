dashboardPage(
    dashboardHeader(title = "DIG Trial Explorer"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("About",   tabName = "about",   icon = icon("info-circle")),
            menuItem("Explore", tabName = "explore", icon = icon("chart-bar"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem("about",   h2("About the DIG trial")),
            tabItem("explore", plotOutput("age_hist"))
        )
    )
)

