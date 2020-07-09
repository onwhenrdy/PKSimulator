# packages
library(stringr)  
library(shiny)
library(magrittr)
library(plotly)
library(glue)
library(scales)
library(shinyWidgets)
library(shiny.i18n)
library(dplyr)
library(shinythemes)
library(data.table)
library(shinydashboard)
library(shinyjs)
library(colorspace)
library(dashboardthemes)
library(shinycssloaders)
library(waiter)
library(FSA)
library(shinyalert)
library(msm)  

# Source
source(file = "app_settings.R", local = FALSE)
source(file = "app_data.R", local = FALSE)
source(file = "model.R", local = FALSE)
source(file = "app_ui.R", local = FALSE)
source(file = "app_server.R", local = FALSE)

# show
shiny::shinyApp(ui, server)

#a <- 1 - exp(-0.1 * 8)
#b <- 1 - exp(-0.5 * 8)

#1000/ (a * b)