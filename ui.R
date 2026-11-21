ui <- dashboardPage(skin='black',
    dashboardHeader(title = "DIG Trial Explorer",titleWidth = 250),
    dashboardSidebar(
        width = 170,
        sidebarMenu(
            menuItem("Overview", tabName = "overview",
                     icon = icon("dashboard")),
            menuItem("Patient Profile", tabName = "patients",
                     icon = icon("users")),
            menuItem("Treatment Outcomes", tabName = "outcomes",
                     icon = icon("chart-bar")),
            menuItem("Survival Analysis", tabName = "survival",
                     icon = icon("heartbeat")),
            menuItem("About & Data", tabName = "about",
                     icon = icon("info-circle"))
        )
    ),
    
    dashboardBody(
        tabItems(
            tabItem(tabName = "overview", h2("Overview")),
            tabItem(tabName = "patients", h2("Patient Profile")),
            tabItem(tabName = "outcomes", h2("Treatment Outcomes")),
            tabItem(tabName = "survival", h2("Survival Analysis")),
            tabItem(tabName="about",aboutUI())
        )
    )
)

