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

mytheme <- fresh::create_theme(
  adminlte_color(
    light_blue = '#2C7873'
  ),
  adminlte_sidebar(
    width = "250px",
    dark_bg = "#004445",
    dark_hover_bg = "#6FB98F",
    dark_color = "white",
    dark_hover_color = "black" #2C7873
  ),
  adminlte_global(
    content_bg = "#e2e9de",  # Ajustamos la transparencia aquí
    box_bg = "#D8DEE9AA",
    info_box_bg = "#D8DEE9AA"
  )
)


source('R/tabs/tabs.R')
source('R/tabs/Menu.R')
source('R/tabs/HeaderInfoBoxFn.R')

ui_visualizacion_data <- dashboardPage(

  dashboardHeader(
    title = "Visualización de datos",
    tags$li(class = "dropdown",
            tags$a(href = "https://heartfelt-lollipop-1b4a75.netlify.app/", icon("home"), "Home")
    )
    ),

  dashboardSidebar(

    sidebarMenu(

      CargaDescargaMenu(),

      br(),
      br(),

      MenuSiderBad()

    )
  ),

  dashboardBody(

    use_theme(mytheme),

    includeCSS(file.path("www/", "style_vis.css")),

    tags$div(
      style =
        'background-image: url("https://github.com/HansTtito/randomThings/blob/main/img/fondo_tnc.png?raw=true");
      position: fixed;
      height: 4500px;
      width: 100%;
      background-attachment: fixed;
      background-position: center top;
      background-size: cover; /* Cubrir todo el contenedor */
      top: 0;
      left: 0;'
    ),

    InfoBoxFn(),

    tabItems(

      biomasaDensidadTab(),

      EstructuraTallasTab(),

      ReproductionTab(),

      CefalpodosTab(),

      MapasTab()

      )
    )
)


