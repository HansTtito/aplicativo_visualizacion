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

CargaDescargaMenu <- function(){

  menuItem(
    "Datos",
    tabName = "datos",
    icon = icon("upload"),
    menuSubItem(
      fileInput(inputId = "datos", label = HTML('<i class="fas fa-upload"></i> Subir Archivo'), multiple = TRUE, accept = ".xlsx", buttonLabel = "Sube excel")
    ),
    menuSubItem(
      downloadButton(outputId = "download", label = "Descarga reporte", icon = icon("pen-to-square"), class = "btn btn-default btn-block",
                     style = "color: #2E3440;")
    )
  )

}


MenuSiderBad <- function(){

  menuItems <- list(
    menuItem("Biomasa y Densidad", tabName = "biomasa_densidad", icon = fa_i(name = 'chart-line')),
    menuItem("Estructura de Tallas", tabName = "tallas", icon = fa_i(name = 'chart-simple')),
    menuItem("Reproducción", tabName = "reproduccion", icon = fa_i(name = 'venus-mars')),
    menuItem("Cefalópodos", tabName = "pulpos", icon = fa_i(name = 'octopus-deploy'), badgeLabel = 'new'),
    menuItem("Mapas", tabName = "mapas", icon = fa_i(name = 'earth-americas'))
  )

  do.call(tagList, menuItems)

}

