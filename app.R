library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)

dig <- read.csv("data/DIG.csv")

ui     <- source("ui.R",     local = TRUE)$value
server <- source("server.R", local = TRUE)$value

shinyApp(ui, server)