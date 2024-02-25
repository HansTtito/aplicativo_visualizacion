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
    )
  )
}
