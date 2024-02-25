# biomasa_densidad_tab.R

biomasaDensidadTab <- function() {
  tabItem(
    tabName = "biomasa_densidad",
    fluidRow(
      column(
        width = 12,
        h4("Biomasa y Densidad", class = "box-title"),
        box(
          title = "",
          width = 6,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          uiOutput('box_especie_biomasa_densidad_output')
        ),
        column(width = 6),
        box(
          title = "",
          width = 6,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          class  = "transparent-box",
          checkboxGroupButtons(
            inputId = 'Variable',
            label = p('Variables'),
            choices = c("Biomasa","Densidad"),
            selected = c('Biomasa','Densidad'),
            individual = TRUE,
            checkIcon = list(
              yes = tags$i(class = "fa fa-circle",
                           style = "color: steelblue"),
              no = tags$i(class = "fa fa-circle-o",
                          style = "color: steelblue")
            )
          )
        )
      )
    ),
    checkboxInput(inputId = "etiquetas_line", label = "Mostrar valores", value = TRUE),
    fluidRow(
      column(
        width = 12,
        plotOutput("plot_densidad_biomasa", height = "500px")
      )
    ),
    br(),

    fluidRow(
      column(
        width = 12,
        box(
          title = "",
          width = 6,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          uiOutput('boxplot_especie_biomasa_densidad_output')
        ),
        box(
          title = "",
          width = 3,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          selectizeInput(
            inputId = "axis_x_box_density",
            label = p("Eje x"),
            choices = c("Zona", "Cat_Profundidad","Sustrato"),
            selected = "Zona")
        ),
        box(
          title = "",
          width = 3,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          checkboxGroupButtons(
            inputId = 'Variable_bx',
            label = p('Variables'),
            choices = c("Biomasa","Densidad"),
            selected = c('Biomasa','Densidad'),
            individual = TRUE,
            checkIcon = list(
              yes = tags$i(class = "fa fa-circle",
                           style = "color: steelblue"),
              no = tags$i(class = "fa fa-circle-o",
                          style = "color: steelblue"))
          )
        )
      )
    ),

    plotOutput("boxplot_densidad_biomasa", height = "600px")

  )
}


EstructuraTallasTab = function(){

  tabItem(

    tabName = "tallas",

    fluidRow(
      column(
        width = 12,
        h4("Estructura de Tallas", class = "box-title"),
        box(
          title = "",
          width = 6,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          uiOutput('especie_talla_global')
        ),
        box(
          title = "",
          width = 6,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          numericInputIcon(inputId = "talla_minima", label = p("Ingresa una talla"), value = '', min = 0, icon = list('mm'))
        )
      )
    ),

    plotOutput("plot_tallas_global", height = "350px"),

    br(),

    fluidRow(
      column(
        width = 12,

        box(
          title = "",
          width = 6,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          uiOutput('biometrico_especie_output')
        ),

        box(
          title = "",
          width = 3,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          selectizeInput(inputId = "axis_y_grafico_tallas", label = p("Eje Y"),
                         choices = c("Tiempo", "Cat_Profundidad","Zona","Sustrato"),
                         selected = "Densidad")
        ),

        box(
          title = "",
          width = 3,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          selectizeInput(inputId = "tipo_grafico_tallas", label = p("Tipo gráfico"),
                         choices = c("Densidad", "Histograma"),
                         selected = "Densidad")
        )
      )
    ),

    plotOutput("plot_estructura_tallas", height = "700px"),

    br(),

    fluidRow(
      column(
        width = 12,
        box(
          title = "",
          width = 6,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          uiOutput('especie_talla_sexo_ru')
        ),
        box(
          title = "",
          width = 3,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          selectizeInput(inputId = "axis_y_grafico_tallas_sexo", label = p("Eje Y"),
                         choices = c("Tiempo", "Cat_Profundidad","Zona","Sustrato"),
                         selected = "Tiempo")
        ),
        box(
          title = "",
          width = 3,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          selectizeInput(inputId = "tipo_grafico_tallas_sexo", label = p("Tipo gráfico"),
                         choices = c("Densidad", "Histograma"),
                         selected = "Densidad")
        )
      )
    ),

    plotOutput("plot_sexo_tallas", height = "550px"),

    br(),

    fluidRow(
      column(
        width = 12,
        box(
          title = "",
          width = 6,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          uiOutput('especie_talla_peso_ru')
        ),
      )
    ),
    checkboxInput(inputId = "ecuacion_talla_peso", label = "Mostrar Ecuación", value = TRUE),

    plotOutput("plot_talla_peso_ru", height = "550px"),

    br(),

    fluidRow(
      column(
        h5("Es neceario ingresar una talla", class = "box-title-red"),
        width = 12,
        box(
          title = "",
          width = 6,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          uiOutput('especie_juveniles_ru')
        ),
        box(
          title = "",
          width = 3,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          numericInputIcon(inputId = 'talla_minima_juveniles',
                           label = p('Ingresa una talla'),
                           value = '',
                           min = 0,
                           icon = list('mm'))
        ),
        box(
          title = "",
          width = 3,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          selectizeInput(inputId = "group_plot_juveniles", label = p("Grupos"),
                         choices = c('Total', "Cat_Profundidad","Zona","Sustrato"),
                         selected = "Total")
        )
      )
    ),
    checkboxInput(inputId = "etiquetas_juveniles_tallas", label = "Mostrar valores", value = TRUE),

    plotlyOutput("plot_juveniles_tiempo", height = "550px"),

    br(),

    fluidRow(
      column(
        width = 12,
        box(
          title = "",
          width = 6,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          uiOutput('Especie_sex_maturity_ru')
        )
      )
    ),
    checkboxInput(inputId = "ecuacion_madurez", label = "Mostrar Ecuación", value = TRUE),

    fluidRow(
      column(
        width = 8,
        plotOutput("plot_sex_maturity", height = "350px")
      ),
      column(
        width = 4,
        p("Tener en consideración que la talla de madurez estimada es un valor preliminar.")
      )
    )
  )
}


ReproductionTab <- function(){

  tabItem(
    tabName = 'reproduccion',

    fluidRow(
      h4("Aspectos reproductivos", class = "box-title"),
      column(
        width = 12,
        box(
          title = "",
          width = 6,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          uiOutput('Especie_sexual_ratio_ru')
        ),
        box(
          title = "",
          width = 3,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          selectizeInput(inputId = "axis_y_grafico_sexual_ratio", label = p("Eje X"),
                         choices = c("Tiempo", "Cat_Profundidad","Zona","Sustrato"),
                         selected = "Tiempo")
        ),
        box(
          title = "",
          width = 3,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          selectizeInput(inputId = "tipo_grafico_sx_ratio", label = p("Tipo gráfico"),
                         choices = c("dodge", "stack","fill"),
                         selected = "dodge")
        )
      )
    ),

    checkboxInput(inputId = "etiquetas_sex_ratio", label = "Mostrar valores", value = TRUE),

    plotOutput("plot_sexual_ratio", height = "550px"),

    br(),

    fluidRow(
      h4("Cangrejos", class = "box-title"),
      br(),
      h6('  Ovígeras', class = 'box-title-left'),
      column(
        width = 12,
        box(
          title = "",
          width = 6,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          uiOutput('cangrejos_especie_ovigeras')
        ),
        box(
          title = "",
          width = 3,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          selectizeInput(inputId = "axis_x_cangrejo_ovigeras", label = p("Eje X"),
                         choices = c("Tiempo", "Cat_Profundidad","Zona","Sustrato"),
                         selected = "Tiempo")
        ),
        box(
          title = "",
          width = 3,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          selectizeInput(inputId = "tipo_grafico_ovigeras", label = p("Tipo gráfico"),
                         choices = c("dodge", "stack","fill"),
                         selected = "dodge")
        )
      )
    ),

    checkboxInput(inputId = "etiquetas_cangrejo_ovigeras", label = "Mostrar valores", value = TRUE),

    plotOutput("plot_cangrejo_ovigeras", height = "550px"),

    br(),

    fluidRow(
      h6('Parejas Reproductoras', class = 'box-title-left'),
      column(
        width = 12,
        box(
          title = "",
          width = 6,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          uiOutput('cangrejo_especie_reproductores')
        ),
        box(
          title = "",
          width = 3,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          selectizeInput(inputId = "axis_x_cangrejo_reproductores", label = p("Eje X"),
                         choices = c("Tiempo", "Cat_Profundidad","Zona","Sustrato"),
                         selected = "Tiempo")
        )
      )
    ),

    checkboxInput(inputId = "etiquetas_cangrejo_reproductores", label = "Mostrar valores", value = TRUE),

    plotOutput("plot_cangrejo_reproductores", height = "550px"),

    br(),

    fluidRow(
      h4("Caracoles", class = "box-title"),
      h6('Agregaciones de Reproductores', class = 'box-title-left'),
      column(
        width = 12,
        box(
          title = "",
          width = 6,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          uiOutput('caracoles_especie_reproductor')
        ),
        box(
          title = "",
          width = 3,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          selectizeInput(inputId = "axis_x_caracoles_reproductor", label = p("Eje X"),
                         choices = c("Tiempo", "Cat_Profundidad","Zona","Sustrato"),
                         selected = "Tiempo")
        )
      )
    ),

    checkboxInput(inputId = "etiquetas_caracole_reproductor", label = "Mostrar valores", value = TRUE),

    plotOutput("plot_caracol_reproductor", height = "550px"),

    br(),

    fluidRow(
      h4("General", class = "box-title"),
      column(
        width = 12,
        box(
          title = "",
          width = 6,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          uiOutput('general_especie_maduras')
        ),
        box(
          title = "",
          width = 3,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          selectizeInput(inputId = "axis_x_general_maduras", label = p("Eje X"),
                         choices = c("Tiempo", "Cat_Profundidad","Zona","Sustrato"),
                         selected = "Tiempo")
        ),
        box(
          title = "",
          width = 3,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          selectizeInput(inputId = "tipo_grafico_maduras", label = p("Tipo gráfico"),
                         choices = c("dodge", "stack","fill"),
                         selected = "dodge")
        )
      )
    ),

    checkboxInput(inputId = "etiquetas_general_maduras", label = "Mostrar valores", value = TRUE),

    plotOutput("plot_general_maduras", height = "550px")
    )
}


CefalpodosTab <- function(){

  tabItem(

    tabName = 'pulpos',

    fluidRow(
      h4("Estructura de Tamaños", class = "box-title"),
      column(
        width = 12,
        box(
          title = "",
          width = 6,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          uiOutput('especie_cefalopodo_size')
        ),
        box(
          title = "",
          width = 3,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          selectizeInput(inputId = "axis_x_cefalopodos_size", label = p("Eje X"),
                         choices = c("Tiempo", "Cat_Profundidad","Zona","Sustrato"),
                         selected = "Tiempo")
        ),
        box(
          title = "",
          width = 3,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          selectizeInput(inputId = "tipo_grafico_cefalopodos_size", label = p("Tipo gráfico"),
                         choices = c("dodge", "stack","fill"),
                         selected = "dodge")
        )
      ),

      checkboxInput(inputId = "etiquetas_cefalopodos_size", label = "Mostrar valores", value = TRUE),

      plotOutput("plot_pulpo_size", height = "550px")

    ),

    br(),

    fluidRow(
      h4("Aspectos Reproductivos", class = "box-title"),
      h6('Hembras con Huevos', class = 'box-title-left'),
      column(
        width = 12,
        box(
          title = "",
          width = 6,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          uiOutput('especie_cefalopodo_reproductores')
        ),
        box(
          title = "",
          width = 3,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          selectizeInput(inputId = "axis_x_cefalopodo_reproductores", label = p("Eje X"),
                         choices = c("Tiempo", "Cat_Profundidad","Zona","Sustrato"),
                         selected = "Tiempo")
        )
      ),

      checkboxInput(inputId = "etiquetas_cefalopodo_reproductores", label = "Mostrar valores", value = TRUE),

      plotOutput("plot_cefalopodos_reproductores", height = "550px")

    ),

    br(),

    fluidRow(
      # h6('Hembras con Huevos', class = 'box-title-left'),
      column(
        width = 12,
        box(
          title = "",
          width = 6,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          uiOutput('especie_enhuevadera')
        ),
        box(
          title = "",
          width = 3,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          selectizeInput(inputId = "axis_x_pulpo_enhuevadera", label = p("Eje X"),
                         choices = c('Size',"Tiempo", "Cat_Profundidad","Zona","Sustrato"),
                         selected = "Size")
        ),
        box(
          title = "",
          width = 3,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          selectizeInput(inputId = "tipo_grafico_pulpo_enhuevadera", label = p("Tipo gráfico"),
                         choices = c("dodge", "stack","fill"),
                         selected = "dodge")
        )
      ),

      checkboxInput(inputId = "etiquetas_pulpo_enhuevadera", label = "Mostrar valores", value = TRUE),

      plotOutput("plot_pulpo_enhuevadera", height = "550px")

    )
  )
}


MapasTab <- function(){

  tabItem(

    tabName = "mapas",

    fluidRow(
      column(
        width = 9,
        box(
          title = "",
          width = 6,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          uiOutput('especie_mapa_ru')
        ),
        box(
          title = "",
          width = 3,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          selectizeInput(inputId = "Variable_mapa", label = "Variable",
                         choices = c("Especie","Densidad","Biomasa"),
                         selected = "Especie")

        )
      )
    ),

    plotlyOutput("plot_mapa", height = "650px", width = "100%")

  )

}
