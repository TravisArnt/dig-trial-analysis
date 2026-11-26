library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(reactable)
library(plotly)

# 1. Load data preparation function
source("R/data_preparation.R")

# 2. Read and prepare dataset
raw_dig <- read.csv("data/DIG.csv")
cleaned_dig <- prepare_dig_data(raw_dig)

# 3. Load modules
source("R/about.R")
source("R/overview.R")
source("R/outcomes.R")
# Later:

# source("R/patients.R")
# source("R/outcomes.R")
# source("R/survival.R")

# 4. Load main UI and server
ui <- source("ui.R", local = TRUE)$value
server <- source("server.R", local = TRUE)$value

# 5. Run Shiny app
shinyApp(ui, server)

