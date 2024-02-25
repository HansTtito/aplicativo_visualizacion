
mytheme <- create_theme(
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
    title = ""
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

    # tags$div(
    #   style =
    #     'background-image: url("https://i0.wp.com/laderasur.com/wp-content/uploads/2020/11/pulpo-del-sur-enteroctopus-megalocyathus-en-isla-guafo-ceduardo-sorensen-frontera-azul-1.jpg?fit=1000%2C539&ssl=1");
    #   position: absolute;
    #   height: 4500px;
    #   width: 100%;
    #   background-attachment: fixed;
    #   background-position: center top;
    #   top: 0;
    #   left: 0;'
    # ),

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


