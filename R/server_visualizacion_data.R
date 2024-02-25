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

server_visualizacion_data <- function(input, output, session) {

  rv <- reactiveValues(data = NULL)
  rv_especies <- reactiveValues(general = NULL, biometrico = NULL)
  meses_espanol <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Set", "Oct", "Nov", "Dic")

  # Lectura de archivos
  observeEvent(input$datos,{

    rv <- procesarDatos(rv, input$datos)

    if(!is.null(rv$info_general)){
      rv_especies$general <- unique(rv$info_general$Especie[!is.na(rv$info_general$Especie)])
    }
    if(!is.null(rv$biometrico)){
      rv_especies$biometrico <- unique(rv$biometrico$Especie[(!is.na(rv$biometrico$Tallas) & rv$biometrico$Tallas > 0)])
    }

  })

  # Render de las especies para el gráfico de densidades
  observeEvent(rv$info_general,{
      output$box_especie_biomasa_densidad_output <- renderUI({
        generarCheckboxGroupButtons('Especie', p('Especies'), rv_especies$general, rv_especies$general)
      })
    })

  # Plot gráfico de líneas de densidades y biomasa
  observeEvent(rv$info_general,{

    output$plot_densidad_biomasa <- renderPlot({

      especie_seleccionada <- input$Especie
      variable_seleccionada <- input$Variable

      biomasa_densidad_long <- reactive({
        generarDatosBiomasaDensidad(rv$info_general, variable_seleccionada, especie_seleccionada, meses_espanol)
      })

      colores_sp <- reactive({
        generarColoresEspecie(biomasa_densidad_long(), 'Especie')
      })

      colores_especie <- colores_sp()
      plot_data <- biomasa_densidad_long()

      generarGraficoDensidadBiomasa(plot_data, colores_especie, variable_seleccionada, input$etiquetas_line, meses_espanol, variables = c('Especie', 'variable'))

    })

  })

  # Render de las especies para el gráfico boxplot de densidades
  observeEvent(rv$info_general,{
    output$boxplot_especie_biomasa_densidad_output <- renderUI({
      generarCheckboxGroupButtons('Especie_boxplot', p('Especies'), rv_especies$general, rv_especies$general)
    })
  })

  # Box plot de densidades y biomasa
  observeEvent(rv$info_general,{

    output$boxplot_densidad_biomasa <- renderPlot({

      bxplt_data = rv$info_general

      especie_seleccionada_bx <- input$Especie_boxplot
      variable_seleccionada_bx <- input$Variable_bx
      variable_x_box_density <- input$axis_x_box_density

      biomasa_densidad_bx <- reactive({
        isolate(generarDatosBiomasaDensidadBoxplot(bxplt_data, especie_seleccionada_bx, variable_seleccionada_bx, variable_x_box_density))
      })

      colores_sp_bx <- reactive({
        isolate(generarColoresEspecie(biomasa_densidad_bx(), 'Especie'))
      })

      bx_data <- biomasa_densidad_bx()
      bx_colors <- colores_sp_bx()

      generarBoxplotBiomasaDensidad(bx_data, bx_colors)

      })
  })

  # Render de las especies para el gráfico de tallas global
  observeEvent(rv$biometrico,{

    output$especie_talla_global = renderUI({
      selectizeInput(
        inputId = "Especie_tallas_global",
        label = p("Especie"),
        choices = rv_especies$biometrico,
        selected = rv_especies$biometrico[1]
       )
    })
  })

  # Estrucura de tallas
  observeEvent(rv$biometrico,{

    output$plot_tallas_global <- renderPlot({

      tallas_plot <- reactive({
        procesarTallas(rv$biometrico, input$Especie_tallas_global, input$talla_minima)
      })

      generarGraficoTallasGeneral(tallas_plot(), input$Especie_tallas_global, input$talla_minima)

    })

  })

  # Render de las especies para el gráfico de tallas
  observeEvent(rv$biometrico,{
    output$biometrico_especie_output <- renderUI({
      generarCheckboxGroupButtons('Especie_biometrico', p('Especies'), rv_especies$biometrico, rv_especies$biometrico)
    })
  })

  # Estrucura de tallas
  observeEvent(rv$biometrico,{

    output$plot_estructura_tallas <- renderPlot({

      estructura_tallas <- reactive({
        procesamiento_data_by_var(rv$biometrico, input$Especie_biometrico, input$axis_y_grafico_tallas, FALSE)
      })

      plt_talla = estructura_tallas() %>%
        filter(Tallas > 0, !is.na(Tallas))
      print(input$Especie_biometrico)

      plot_tallas_by_variable(estructura_tallas(), input$tipo_grafico_tallas, input$axis_y_grafico_tallas, 'Especie')

    })
  })

  # Render de las especies para el gráfico de talla por sexo
  observeEvent(rv$biometrico,{

    output$especie_talla_sexo_ru <- renderUI({

      sps = rv$biometrico %>%
        dplyr::filter(!is.na(Tallas), Tallas > 0, Sexo %in% c("Macho","Hembra"))

      selectizeInput(inputId = "Especie_tallas_sexo",
                  label = p("Especies"),
                  choices = unique(sps$Especie),
                  selected = unique(sps$Especie)[1])
    })
  })

  # Estrucura de tallas por sexo
  observeEvent(rv$biometrico,{

    output$plot_sexo_tallas <- renderPlot({

      talla_sexo <- reactive({
        procesamiento_data_by_var(rv$biometrico, input$Especie_tallas_sexo, input$axis_y_grafico_tallas_sexo)
      })

      plt_talla_sexo = talla_sexo() %>%
        filter(Sexo %in% c('Macho','Hembra'))
      print(input$Especie_tallas_sexo)

      plot_tallas_by_variable(plt_talla_sexo, input$tipo_grafico_tallas_sexo, input$axis_y_grafico_tallas_sexo, 'Sexo')

    })

  })

  # Render de las especies para el gráfico de talla - peso
  observeEvent(rv$biometrico,{

    output$especie_talla_peso_ru <- renderUI({

      sps_talla_peso = rv$biometrico %>%
        dplyr::filter(!is.na(Tallas), Tallas > 0, !is.na(Peso), Peso > 0)

      selectizeInput(inputId = "Especie_talla_peso",
                     label = p("Especies"),
                     choices = unique(sps_talla_peso$Especie),
                     selected = unique(sps_talla_peso$Especie)[1])
    })
  })

  # Relacion Longitud - Peso
  observeEvent(rv$biometrico,{

    output$plot_talla_peso_ru <- renderPlot({

      talla_peso <- reactive({
        rv$biometrico %>%
          filter(Especie %in% input$Especie_talla_peso, Tallas > 0, !is.na(Tallas), Peso > 0, !is.na(Peso)) %>%
          mutate(Peso = Peso * 1000)
      })

      print(input$Especie_talla_peso)

      data_talla_peso <- talla_peso()

      modelo_L_w = modelo_talla_peso(data_talla_peso, 0.001, 3)

      plot_talla_peso(data_talla_peso, modelo_L_w, input$ecuacion_talla_peso)

    })

  })

  # Render de las especies para el gráfico de juveniles
  observeEvent(rv$biometrico,{

    output$especie_juveniles_ru <- renderUI({

      sps_juveniles = rv$biometrico %>%
        mutate(mes = month(Fecha),
               year = year(Fecha)) %>%
        filter(!is.na(Tallas), Tallas > 0)

      selectizeInput(inputId = "especie_juveniles",
                     label = p("Especies"),
                     choices = unique(sps_juveniles$Especie),
                     selected = unique(sps_juveniles$Especie)[1])
    })

  })

  # plot de juveniles
  observeEvent(input$talla_minima_juveniles,{

    output$plot_juveniles_tiempo <- renderPlotly({

      print(input$talla_minima_juveniles)
      print(input$group_plot_juveniles)
      print(input$especie_juveniles)

      tryCatch({

        data_juveniles <- reactive({
          GetDataJuveniles(rv$biometrico, input$especie_juveniles, input$group_plot_juveniles, meses_espanol, input$talla_minima_juveniles)
        })

        print(data_juveniles())

        getPlotJuveniles(data_juveniles(), input$group_plot_juveniles, input$especie_juveniles, input$etiquetas_juveniles_tallas)

      }, error = function(e){
        return('Ingresa datos')
      })

    })

  })

  # Render de las especies para el gráfico sexual maturity
  observeEvent(rv$biometrico,{

    output$Especie_sex_maturity_ru <- renderUI({

      sps_maturity = rv$biometrico %>%
        dplyr::filter(!is.na(Tallas), Tallas > 0, Madurez %in% c("Maduro","Inmaduro"))

      selectizeInput(inputId = "Especie_sex_maturity",
                     label = p("Especies"),
                     choices = unique(sps_maturity$Especie),
                     selected = unique(sps_maturity$Especie)[1])
    })
  })

  # madurez a la talla
  observeEvent(rv$biometrico,{

    output$plot_sex_maturity <- renderPlot({

      data_sex_maturity = reactive({
        rv$biometrico %>%
          dplyr::filter(Tallas > 0, !is.na(Tallas), Madurez %in% c("Maduro",'Inmaduro'), Especie %in% input$Especie_sex_maturity)
        })

      print(input$Especie_sex_maturity)

      tryCatch({

        data_prop_madurez = getDataTallaMadurez(data_sex_maturity())

        modelo_madurez = getModelMadurez(data_sex_maturity(), 5, 0.1)

        plotMadurez(data_sex_maturity(), data_prop_madurez, modelo_madurez, input$ecuacion_madurez)


      }, error = function(e) {
        par(bg = "#e2e9de")
        plot(1, type = "n", main = "La cantidad de datos no es suficiente para estimar Talla de Madurez", cex.main = 1.5, axes = FALSE, xlab = "", ylab = "")
        rect(par("usr")[1], par("usr")[3],
             par("usr")[2], par("usr")[4],
             col = "#e2e9de") # Color

        box()

      })

    })
    })


  # Render de las especies para el gráfico sexual ratio
  observeEvent(rv$biometrico,{

    output$Especie_sexual_ratio_ru <- renderUI({

      sps_sex_ratio = rv$biometrico %>%
        dplyr::filter(Sexo %in% c("Macho","Hembra"))

      selectizeInput(inputId = "Especie_sexual_ratio",
                     label = p("Especies"),
                     choices = unique(sps_sex_ratio$Especie),
                     selected = unique(sps_sex_ratio$Especie)[1])
    })
  })

  # plot sex ratio
  observeEvent(rv$biometrico,{

    output$plot_sexual_ratio <- renderPlot({

      sexual_ration <- reactive({
        procesamiento_data_by_var(rv$biometrico, input$Especie_sexual_ratio, input$axis_y_grafico_sexual_ratio)
      })

      angle_x_sr <- ifelse(input$axis_y_grafico_sexual_ratio %in% c("Tiempo","Zona", "Cat_Profundidad"), 90, 0)

      print(input$Especie_sexual_ratio)

      plt_sx_rt = sexual_ration() %>%
        filter(Sexo %in% c('Macho','Hembra')) %>%
        reframe(var = length(Sexo), .by = c('year','id_var', 'Sexo'))

      generar_grafico_sx_ratio(plt_sx_rt, 'id_var', 'var', 'Sexo', input$tipo_grafico_sx_ratio, input$axis_y_grafico_sexual_ratio , etiquetas = input$etiquetas_sex_ratio, angle_x_sr, c('Macho','Hembra'),  c('♂','♀'))

    })

  })

  # Render de las especies para ovigeras CANGREJOS
  observeEvent(rv$biometrico,{

    output$cangrejos_especie_ovigeras <- renderUI({

      sps_ovigeras = rv$biometrico %>%
        filter(Grupos %in% 'Cangrejos', Sexo %in% 'Hembra', Ovigeras %in% c('No','Si'))

      selectizeInput(inputId = "Especie_cangrejos_ovigera",
                     label = p("Especies"),
                     choices = unique(sps_ovigeras$Especie),
                     selected = unique(sps_ovigeras$Especie)[1])
    })
  })

  # plot ovigeras CANGREJOS
  observeEvent(rv$biometrico,{

    output$plot_cangrejo_ovigeras <- renderPlot({

      print(input$Especie_cangrejos_ovigera)

      ovigeras_cangrejos <- reactive({
        procesamiento_data_by_var(rv$biometrico, input$Especie_cangrejos_ovigera, input$axis_x_cangrejo_ovigeras)
      })

      plt_ovigeras = ovigeras_cangrejos() %>%
        filter(Grupos %in% 'Cangrejos', Sexo %in% 'Hembra', Ovigeras %in% c('No','Si')) %>%
        reframe(var = length(Ovigeras), .by = c('year','id_var', 'Ovigeras'))

      angle_ovigeras <- ifelse(input$axis_x_cangrejo_ovigeras %in% c("Tiempo","Zona", "Cat_Profundidad"), 90, 0)

      generar_grafico_sx_ratio(plt_ovigeras, 'id_var', 'var', 'Ovigeras', input$tipo_grafico_ovigeras, input$axis_x_cangrejo_ovigeras , input$etiquetas_cangrejo_ovigeras, angle_ovigeras, c('No','Si'), c('No','Si'))

    })

  })

  # Render de las especies para reproductoras CANGREJOS
  observeEvent(rv$info_general,{

    output$cangrejo_especie_reproductores <- renderUI({

      sps_reproductoras = rv$info_general %>%
        filter(Grupos %in% 'Cangrejos', Parejas_reproductoras_Cangrejo > 0, !is.na(Parejas_reproductoras_Cangrejo))

      selectizeInput(inputId = "Especie_cangrejos_reproductores",
                     label = p("Especies"),
                     choices = unique(sps_reproductoras$Especie),
                     selected = unique(sps_reproductoras$Especie)[1])
    })
  })

  # plot reproductores CANGREJOS
  observeEvent(rv$info_general,{

    output$plot_cangrejo_reproductores <- renderPlot({

      print(input$Especie_cangrejos_reproductores)

      cangrejos_reproductores <- reactive({
        getDataEspecieReproductores(rv$info_general, 'Cangrejos', 'Parejas_reproductoras_Cangrejo' , input$Especie_cangrejos_reproductores, input$axis_x_cangrejo_reproductores)
      })

      angle_reproductoras <- ifelse(input$axis_x_cangrejo_reproductores %in% c("Tiempo","Zona", "Cat_Profundidad"), 90, 0)

      generar_grafico_sx_ratio(cangrejos_reproductores(), 'id_var', 'n', 'Especie', 'dodge', input$axis_x_cangrejo_reproductores, input$etiquetas_cangrejo_reproductores, angle_reproductoras, input$Especie_cangrejos_reproductores, c('Reproductores',''))

    })

  })

  # Render de las especies para reproductoras CARACOLES
  observeEvent(rv$info_general,{

    output$caracoles_especie_reproductor <- renderUI({

      sps_reproductoras_caracol = rv$info_general %>%
        filter(Grupos %in% 'Caracoles', Agregacion_reproductor_Caracol > 0, !is.na(Agregacion_reproductor_Caracol))

      selectizeInput(inputId = "Especie_caracoles_reproductores",
                     label = p("Especies"),
                     choices = unique(sps_reproductoras_caracol$Especie),
                     selected = unique(sps_reproductoras_caracol$Especie)[1])
    })
  })

  # plot reproductores CARACOLES
  observeEvent(rv$info_general,{

    output$plot_caracol_reproductor <- renderPlot({


      cangrejos_reproductores <- reactive({
        getDataEspecieReproductores(rv$info_general, 'Caracoles', 'Agregacion_reproductor_Caracol', input$Especie_caracoles_reproductores, input$axis_x_caracoles_reproductor)
      })

      angle_agregaciones_caracoles <- ifelse(input$axis_x_caracoles_reproductor %in% c("Tiempo","Zona", "Cat_Profundidad"), 90, 0)

      generar_grafico_sx_ratio(cangrejos_reproductores(), 'id_var', 'n', 'Especie', 'dodge', input$axis_x_caracoles_reproductor, input$etiquetas_caracole_reproductor, angle_agregaciones_caracoles, input$Especie_caracoles_reproductores, c('Agregaciones',''))

    })

  })

  # Render de las las maduras GENERAL
  observeEvent(rv$biometrico,{

    output$general_especie_maduras <- renderUI({

      sps_maduras_general = rv$biometrico %>%
        filter(Madurez %in% c('Maduro', 'Inmaduro'))

      selectizeInput(inputId = "Especie_maduras_general",
                     label = p("Especies"),
                     choices = unique(sps_maduras_general$Especie),
                     selected = unique(sps_maduras_general$Especie)[1])
    })
  })

  # plot de las las maduras GENERAL
  observeEvent(rv$biometrico,{

    output$plot_general_maduras <- renderPlot({

      general_maduras <- reactive({
        procesamiento_data_by_var(rv$biometrico, input$Especie_maduras_general, input$axis_x_general_maduras)
      })

      angle_x_general_maduros <- ifelse(input$axis_x_general_maduras %in% c("Tiempo","Zona", "Cat_Profundidad"), 90, 0)

      plt_maduras_general = general_maduras() %>%
        filter(Madurez %in% c('Maduro','Inmaduro')) %>%
        reframe(var = length(Madurez), .by = c('year','id_var', 'Madurez'))

      print(input$Especie_maduras_general)

      generar_grafico_sx_ratio(plt_maduras_general, 'id_var', 'var', 'Madurez',input$tipo_grafico_maduras, input$axis_x_general_maduras, input$etiquetas_general_maduras, angle_x_general_maduros, c('Maduro','Inmaduro'), c('Mad','Inm'))

    })

  })

  # Render de las especies para PULPOS
  observeEvent(rv$biometrico,{

    output$especie_cefalopodo_size <- renderUI({

      sps_cefalopodos_size = rv$biometrico %>%
        dplyr::filter(Grupos %in% 'Pulpos', Size_aparente_final %in% c('Pequenho','Mediano','Grande'))

      selectizeInput(inputId = "Especie_cefalopodo_size",
                     label = p("Especies"),
                     choices = unique(sps_cefalopodos_size$Especie),
                     selected = unique(sps_cefalopodos_size$Especie)[1])
    })
  })

  # plot size aparente PULPOS
  observeEvent(rv$biometrico,{

    output$plot_pulpo_size <- renderPlot({



      size_aparente_data <- reactive({
        procesamiento_data_by_var(rv$biometrico, input$Especie_cefalopodo_size, input$axis_x_cefalopodos_size)
      })

      print(input$Especie_cefalopodo_size)

      angle_size_ap <- ifelse(input$axis_x_cefalopodos_size %in% c("Tiempo","Zona", "Cat_Profundidad"), 90, 0)

      plt_size_aparente = size_aparente_data() %>%
        filter(Size_aparente_final %in% c('Pequenho','Mediano','Grande')) %>%
        reframe(var = length(Size_aparente_final), .by = c('year','id_var', 'Size_aparente_final'))

      generar_grafico_sx_ratio(plt_size_aparente, 'id_var', 'var', 'Size_aparente_final', input$tipo_grafico_cefalopodos_size, input$axis_x_cefalopodos_size, input$etiquetas_cefalopodos_size , angle_size_ap, c('Pequenho','Mediano','Grande'), c('P','M','G'))


    })

  })

  # Render de las especies para reproductoras PULPOS
  observeEvent(rv$info_general,{

    output$especie_cefalopodo_reproductores <- renderUI({

      sps_reproductoras_pulpos = rv$info_general %>%
        filter(Grupos %in% 'Pulpos', Hembras_puesta_Pulpo > 0, !is.na(Hembras_puesta_Pulpo))

      selectizeInput(inputId = "Especie_pulpo_reproductores",
                     label = p("Especies"),
                     choices = unique(sps_reproductoras_pulpos$Especie),
                     selected = unique(sps_reproductoras_pulpos$Especie)[1])
    })
  })

  # plot reproductores PULPOS
  observeEvent(rv$info_general,{

    output$plot_cefalopodos_reproductores <- renderPlot({

      pulpos_reproductores <- reactive({
        getDataEspecieReproductores(rv$info_general, 'Pulpos', 'Hembras_puesta_Pulpo', input$Especie_pulpo_reproductores, input$axis_x_cefalopodo_reproductores)
      })

      print(input$Especie_pulpo_reproductoress)

      angle_enh_pulpos <- ifelse(input$axis_x_cefalopodo_reproductores %in% c("Tiempo","Zona", "Cat_Profundidad"), 90, 0)

      generar_grafico_sx_ratio(pulpos_reproductores(), 'id_var', 'n', 'Especie', 'dodge', input$axis_x_cefalopodo_reproductores, input$etiquetas_cefalopodo_reproductores, angle_enh_pulpos, input$Especie_pulpo_reproductores, c('Enhueverados',''))

      })

  })

  # Render de las PULPO con Huevos
  observeEvent(rv$biometrico,{

    output$especie_enhuevadera <- renderUI({

      sps_pulpos_huevos = rv$biometrico %>%
        filter(Grupos %in%'Pulpos', Enhueverado %in% c('Si', 'No'))

      print('MIRA ACA')
      print(sps_pulpos_huevos)

      selectizeInput(inputId = "Especie_pulpos_huevos",
                     label = p("Especies"),
                     choices = unique(sps_pulpos_huevos$Especie),
                     selected = unique(sps_pulpos_huevos$Especie)[1])
    })
  })

  # plot de las Pulpos Huevos
  observeEvent(rv$biometrico,{

    output$plot_pulpo_enhuevadera <- renderPlot({

      pulpos_huevos <- reactive({
        procesamiento_data_by_var(rv$biometrico, input$Especie_pulpos_huevos, input$axis_x_pulpo_enhuevadera, pulpos = TRUE)
      })

      angle_pulpo_huevos <- ifelse(input$axis_x_pulpo_enhuevadera %in% c("Tiempo","Zona", "Cat_Profundidad"), 90, 0)

      plt_pulpos_huevos = pulpos_huevos() %>%
        filter(Enhueverado %in% c('Si','No')) %>%
        reframe(var = length(Enhueverado), .by = c('year', 'id_var', 'Enhueverado'))

      print(input$Especie_pulpos_huevos)

      generar_grafico_sx_ratio(plt_pulpos_huevos, 'id_var', 'var', 'Enhueverado', input$tipo_grafico_pulpo_enhuevadera, input$axis_x_pulpo_enhuevadera , input$etiquetas_pulpo_enhuevadera, angle_pulpo_huevos, c('Si','No'), c('Si','No'))

    })

  })


  # Render de las especies para el mapa
  observeEvent(rv$info_general,{

    output$especie_mapa_ru <- renderUI({

      sps_mapa = limpiar_data_mapa(rv$info_general)

      selectizeInput(
        inputId = "especie_mapa",
        label = "Especie",
        choices = c("Todas",unique(sps_mapa$Especie)),
        selected = "Todas")
    })
  })

  ## Distribución de puntos
  observeEvent(rv$info_general,{

    output$plot_mapa <- renderPlotly({

      mapa_densidad_biomasa = limpiar_data_mapa(rv$info_general)

      data_plot <- reactive({
        filtrar_puntos_mapa(mapa_densidad_biomasa, input$especie_mapa)
      })

      print(input$especie_mapa)

      generar_mapa_plotly(data_plot(), input$Variable_mapa)

    })

  })

  output$download <- downloadHandler(
    filename =  function() {paste0("Reporte_", Sys.Date(), ".pdf")},
    content = function(file) {
      req(input$datos)
      rmarkdown::render(input = "reporte.Rmd",
                        output_format = "pdf_document",
                        output_file = "reporte.pdf")
      file.copy("reporte.pdf", file)
    }
  )

}
