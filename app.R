library(shiny)
library(plotly)
library(tidyverse)
library(readxl)
library(lubridate)
library(shinydashboard)
library(shinyWidgets)
library(ggh4x)
library(ggridges)
library(fresh)
library(shinydashboardPlus)
library(shinyjs)
library(fontawesome)


source(file = 'R/funciones_visualizacion.R')
source(file = "R/ui_visualizacion_data.R")
source(file = "R/server_visualizacion_data.R")
source(file = "R/general_functions.R")

shinyApp(ui = ui_visualizacion_data, server = server_visualizacion_data,
         options = list(
           width = "100%", height = 8000
         )
)
