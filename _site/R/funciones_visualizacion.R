generarCheckboxGroupButtons <- function(id, label, choices, selected) {
  checkboxGroupButtons(
    inputId = id,
    label = label,
    individual = TRUE,
    choices = choices,
    selected = selected,
    checkIcon = list(
      yes = tags$i(class = "fa fa-circle", style = "color: steelblue"),
      no = tags$i(class = "fa fa-circle-o", style = "color: steelblue")
    )
  )
}


procesarDatos <- function(reactive_obj, input_file) {

    info_general <- read_xlsx(input_file$datapath, sheet = "Tabla_informacion")
    biometrico <- read_xlsx(input_file$datapath, sheet = "biometrico")

    biometrico = biometrico %>%
      mutate(Tallas = as.numeric(Tallas),
             Peso = as.numeric(Peso),
             Peso_cuantitativo = as.numeric(Peso_cuantitativo))

    info_general = info_general %>%
      mutate(Densidad = as.numeric(Densidad),
             Biomasa = as.numeric(Biomasa),
             Profundidad = as.numeric(Profundidad),
             Parejas_reproductoras_Cangrejo = as.numeric(Parejas_reproductoras_Cangrejo),
             Agregacion_reproductor_Caracol = as.numeric(Agregacion_reproductor_Caracol),
             Hembras_puesta_Pulpo = as.numeric(Hembras_puesta_Pulpo))



    biometrico$Zona <- ifelse(biometrico$row_id %in% info_general$row_id,
                              info_general$Zona[match(biometrico$row_id, info_general$row_id,nomatch = 0, incomparables = NA)],
                              NA)

    biometrico$Profundidad <- ifelse(biometrico$row_id %in% info_general$row_id,
                                         info_general$Profundidad[match(biometrico$row_id, info_general$row_id,nomatch = 0, incomparables = NA)],
                                         NA)

    biometrico$Sustrato <- ifelse(biometrico$row_id %in% info_general$row_id,
                                  info_general$Sustrato[match(biometrico$row_id, info_general$row_id,nomatch = 0, incomparables = NA)],
                                  NA)

    biometrico$Grupos <- ifelse(biometrico$row_id %in% info_general$row_id,
                                info_general$Grupos[match(biometrico$row_id,info_general$row_id,nomatch = 0, incomparables = NA)],
                                NA)

    biometrico$Grupos <- ifelse(biometrico$Especie %in% info_general$Especie,
                                info_general$Grupos[match(biometrico$Especie,info_general$Especie,nomatch = 0, incomparables = NA)],
                                NA)


    info_general$Cat_Profundidad <- cut(x = info_general$Profundidad, breaks = c(-Inf, 5, 10, +Inf))

    biometrico$Cat_Profundidad <- cut(x = biometrico$Profundidad, breaks = c(-Inf, 5, 10, +Inf))

    biometrico = biometrico %>%
      mutate(Size_aparente_final =
               case_when(Grupos %in% 'Pulpos' & Peso_cuantitativo < 0.8 ~ 'Pequenho',
                         Grupos %in% 'Pulpos' & Peso_cuantitativo >= 0.8 & Peso_cuantitativo < 1.2 ~ 'Mediano',
                         Grupos %in% 'Pulpos' & Peso_cuantitativo >= 1.2 ~ 'Grande',
                         TRUE ~ as.character(Size_aparente)))


    reactive_obj$info_general <- info_general
    reactive_obj$biometrico <- biometrico


  return(reactive_obj)
}


generarDatosBiomasaDensidad <- function(info_general, input_variable, input_especie, meses_espanol) {

  biomasa_densidad_long <- info_general %>%
    mutate(year = year(Fecha),
           mes = month(Fecha)) %>%
    reframe(Biomasa = mean(Biomasa, na.rm = TRUE),
            Densidad = mean(Densidad, na.rm = TRUE), .by = c('year', 'mes', 'Especie')) %>%
    filter(Especie %in% input_especie) %>%
    gather(variable, valor, -c(year, mes, Especie)) %>%
    filter(variable %in% input_variable)

  biomasa_densidad_long$valor_scaled = biomasa_densidad_long$valor

  if ("Biomasa" %in% input_variable & "Densidad" %in% input_variable) {
    biomasa_densidad_long <- biomasa_densidad_long %>%
      mutate(valor_scaled = ifelse(variable == "Densidad", valor_scaled / 10, valor_scaled))
  }

  biomasa_densidad_long$mes <- meses_espanol[biomasa_densidad_long$mes]

  biomasa_densidad_long$mes <- factor(biomasa_densidad_long$mes, levels = meses_espanol)

  return(biomasa_densidad_long)

}

generarDatosBiomasaDensidadBoxplot <- function(data, input_especie, input_variable, input_axis_x_box_density) {

  data$axis_x_bxplt = switch(
    input_axis_x_box_density,
    "Zona" = data$Zona,
    "Sustrato" = data$Sustrato,
    "Cat_Profundidad" = data$Cat_Profundidad
  )

  new_data = data %>%
    filter(Especie %in% input_especie) %>%
    select(axis_x_bxplt, Especie, Densidad, Biomasa) %>%
    gather(variable, valor, -c(axis_x_bxplt, Especie)) %>%
    filter(variable %in% input_variable)

  return(new_data)

}

generarColoresEspecie <- function(biomasa_densidad_long, varaible) {
  spec_colors <- rainbow(length(unique(biomasa_densidad_long[[varaible]])))
  names(spec_colors) <- unique(biomasa_densidad_long[[varaible]])
  return(spec_colors)
}

complete_year_month <- function(data, meses_espanol, variables = c('Especie', 'variable')) {

  unique_combinations <- expand.grid(mes = meses_espanol, year = unique(data$year))

  if (!is.null(variables)) {
    all_combinations <- expand.grid(lapply(data[variables], unique))
    todos_los_meses_df <- merge(unique_combinations, all_combinations, by = NULL)
  } else {
    todos_los_meses_df <- unique_combinations
  }

  plot_data_completo <- merge(todos_los_meses_df, data, all.x = TRUE)

  plot_data_completo$mes <- factor(plot_data_completo$mes, levels = meses_espanol)

  plot_data_completo = plot_data_completo %>% arrange(year, mes)

  return(plot_data_completo)
}



generarGraficoDensidadBiomasa <- function(plot_data, colores_especie, input_variable, input_etiquetas, meses_espanol, variables = c('Especie', 'variable'), legend.text.size = 14, legend.title.size = 14, background.color = "#e2e9de", axis.text.sizee = 15, axis.title.size = 18, legend.position = 'right') {

  plot_data <- complete_year_month(plot_data, meses_espanol, variables)

  plot_data$Variable = plot_data$variable

  p <- ggplot(plot_data, aes(x = interaction(mes, year),
                             y = valor_scaled,
                             group = interaction(Variable, Especie),
                             color = Especie,
                             linetype = Variable)) +
    geom_line() +
    geom_point(size = 1.5) +
    labs(x = "",
         y = paste0(input_variable,'\n')) +
    scale_color_manual(values = colores_especie) +
    scale_x_discrete(guide = 'axis_nested') +
    theme(axis.text = element_text(color = "black", size = axis.text.sizee),
          axis.text.x = element_text(angle = 90),
          axis.title = element_text(size = axis.title.size, face = 'bold'),
          legend.text = element_text(size = legend.text.size),
          legend.title = element_text(size = legend.title.size, face = 'bold'),
          plot.title = element_text(size = 20, face = "bold"),
          panel.background = element_rect(fill = background.color),
          plot.background = element_rect(fill = background.color, color = 'black'),
          panel.grid.major = element_line(color = "gray60"),
          panel.grid.minor = element_line(color = "gray60"),
          strip.text = element_text(size = 20, face = "bold"),
          strip.background = element_blank(),
          panel.border = element_rect(color = "black", fill = NA, size = 1),
          legend.background = element_rect(fill = background.color, color = background.color),
          legend.position = legend.position)

  if (input_etiquetas) {
    p <- p + geom_text(aes(label = sprintf("%0.2f", valor), vjust = -0.5), show.legend = FALSE, size = 6, family = 'mono', fontface = 'bold')
  }

  if ("Biomasa" %in% input_variable & "Densidad" %in% input_variable) {
    p <- p + scale_y_continuous(
      sec.axis = sec_axis(~.*10, name = "Densidad\n", labels = scales::comma)
    ) +
      labs(y = "Biomasa\n")
  }

  return(p)
}


generarBoxplotBiomasaDensidad <- function(plot_data, colores_especie) {

  p <- ggplot(plot_data, aes(
    x = interaction(Especie, axis_x_bxplt),
    y = valor,
    fill = Especie
  )) +
    labs(y = "", x = "") +
    geom_boxplot() +
    facet_wrap(~variable, ncol = 1, scales = "free_y") +
    scale_color_manual(values = colores_especie) +
    scale_x_discrete(guide = 'axis_nested') +
    theme(axis.text = element_text(color = "black", size = 15),
          axis.text.x = element_text(angle = 90),
          axis.title = element_text(size = 18, face = 'bold'),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14, face = 'bold'),
          plot.title = element_text(size = 20, face = "bold"),
          panel.background = element_rect(fill = "#e2e9de"),
          plot.background = element_rect(fill = "#e2e9de", color = 'black'),
          panel.grid.major = element_line(color = "gray60"),
          panel.grid.minor = element_line(color = "gray60"),
          strip.text = element_text(size = 20, face = "bold"),
          strip.background = element_blank(),
          panel.border = element_rect(color = "black", fill = NA, size = 1),
          legend.background = element_rect(fill = "#e2e9de", color = "#e2e9de"))

  return(print(p))

}

# Función para procesar las tallas
procesarTallas <- function(biometrico, input_especie, talla_minima) {

  data <- biometrico %>%
    filter(Especie %in% input_especie, !is.na(Tallas), Tallas > 0)

  if (!is.na(talla_minima) && talla_minima < 0) {

    showModal(modalDialog(
      title = "Error",
      "La talla no puede ser negativa.",
      easyClose = TRUE,
      footer = modalButton("OK")
    ))

    updateNumericInput(session, "talla_minima", label = "Ingresa una talla", value = '', min = 0)

  } else if(!is.na(talla_minima) && talla_minima > 0) {

    porcentaje_bajo_minima <- mean(data$Tallas <= talla_minima) * 100
    porcentaje_sobre_minima <- mean(data$Tallas > talla_minima) * 100

    data$Porcentaje_bajo <- paste("\nPorcentaje por\ndebajo de ",talla_minima, 'mm:\n', round(porcentaje_bajo_minima, 2), "%")
    data$Porcentaje_sobre <- paste("\nPorcentaje por\nencima de",talla_minima, 'mm:\n', round(porcentaje_sobre_minima, 2), "%")

  }

  return(data)

}


# Función para generar el gráfico de tallas
generarGraficoTallasGeneral <- function(tallas_plot_general, input_especie, talla_minima) {

  gg <- ggplot(tallas_plot_general, aes(x = Tallas, y = Especie)) +
    geom_density_ridges(alpha = 0.5, scale = 1) +
    theme_minimal() +
    labs(y = input_especie, x = "\nLongitud (mm)") +
    theme(axis.text = element_text(color = "black", size = 15),
          axis.title = element_text(size = 18, face = 'bold'),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14, face = 'bold'),
          plot.title = element_text(size = 20, face = "bold"),
          panel.background = element_rect(fill = "#e2e9de"),
          plot.background = element_rect(fill = "#e2e9de"),
          panel.grid.major = element_line(color = "gray60"),
          panel.grid.minor = element_line(color = "gray60"),
          legend.background = element_rect(fill = "#e2e9de", color = "#e2e9de")) +
    scale_y_discrete(expand = c(0, 0))

  # Agregar línea vertical y etiquetas si talla_minima existe
  if (!is.na(talla_minima) && talla_minima > 0) {
    gg <- gg +
      geom_vline(xintercept = talla_minima, linetype = "dashed", color = "red", linewidth = 1.2) +
      annotate("text", x = min(tallas_plot_general$Tallas), y = Inf, vjust = 1, hjust = 0,
               label = unique(tallas_plot_general$Porcentaje_bajo), size = 5) +
      annotate("text", x = max(tallas_plot_general$Tallas), y = Inf, vjust = 1, hjust = 1,
               label = unique(tallas_plot_general$Porcentaje_sobre), size = 5)
  }

  return(gg)
}


procesamiento_data_by_var <- function(rv_biometrico, input_especie, input_axis_y_grafico_tallas, pulpos = FALSE){

  new_data <- rv_biometrico  %>%
    dplyr::filter(Especie %in% input_especie) %>%
    mutate(
      year = year(Fecha),
      mes = month(Fecha, label = TRUE, abbr = TRUE))

  if(pulpos){
    new_data$id_var = switch(
      input_axis_y_grafico_tallas,
      'Size' = factor(new_data$Size_aparente_final, levels = c('Pequenho','Mediano','Grande')),
      "Tiempo" = new_data$mes,
      "Zona" = new_data$Zona,
      "Sustrato" = as.character(new_data$Sustrato),
      "Cat_Profundidad" = new_data$Cat_Profundidad
    )

  } else {

    new_data$id_var = switch(
      input_axis_y_grafico_tallas,
      "Tiempo" = new_data$mes,
      "Zona" = new_data$Zona,
      "Sustrato" = as.character(new_data$Sustrato),
      "Cat_Profundidad" = new_data$Cat_Profundidad
    )

  }

  return(new_data)
}



plot_tallas_by_variable = function(data_plot, input_tipo_grafico_tallas, input_axis_y_grafico_tallas, fill = 'Especie'){

  # data_plot = complete_year_month(data_plot, meses_espanol, c(fill))

  fill_sym <- rlang::sym(fill)

  p = data_plot %>%
    filter(Tallas > 0, !is.na(Tallas)) %>%
    ggplot() +
    geom_density_ridges(aes(x = Tallas,y = interaction(id_var, year), fill = !!fill_sym),
                        alpha = 0.5,
                        scale = 1,
                        stat = ifelse(input_tipo_grafico_tallas == 'Densidad', "density_ridges", 'binline'),
                        bins = ifelse(input_tipo_grafico_tallas == 'Densidad', NA, 200)) +
    scale_y_discrete(guide = 'axis_nested', expand = c(0.05, 0)) +
    labs(y = paste0(input_axis_y_grafico_tallas,'\n'), x = "\nLongitud (mm)") +
    theme(axis.text = element_text(color = "black", size = 15),
          axis.title = element_text(size = 18, face = 'bold'),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14, face = 'bold'),
          plot.title = element_text(size = 20, face = "bold"),
          panel.background = element_rect(fill = "#e2e9de"),
          plot.background = element_rect(fill = "#e2e9de"),
          panel.grid.major = element_line(color = "gray60"),
          panel.grid.minor = element_line(color = "gray60"),
          legend.background = element_rect(fill = "#e2e9de", color = "#e2e9de"))

  return(p)

}


modelo_talla_peso <- function(data, a_initial, b_initial){

  modelo <- tryCatch(
    expr = {
      nls(Peso ~ a * Tallas^b, data = data, start = list(a = a_initial, b = b_initial))
    },
    error = function(e) {
      NULL
    }
  )

  if (!is.null(modelo)) {

    plot_data <- data.frame(Tallas = seq(min(data$Tallas), max(data$Tallas), length.out = 100))
    plot_data$Peso_predicho <- predict(modelo, newdata = plot_data)
    coeficientes <- coef(modelo)
    rm(modelo)
    a <- as.numeric(format(coeficientes["a"], scientific = FALSE, digits = 5))
    b <- as.numeric(format(coeficientes["b"], digits = 4))

    ecuacion <- as.expression(substitute(Peso == a * Tallas^b, list(a = a, b = b)))

    data_modelo <- list(ecuacion = ecuacion, plot_data = plot_data)

  } else {
    data_modelo <- list(ecuacion = 'Datos insuficientes para ajustar el modelo', plot_data = data.frame())
  }

  return(data_modelo)

}


plot_talla_peso <- function(data, ecuacion, input_ecuacion_talla_peso){

  p <- ggplot() +
    geom_point(data = data, aes(x = Tallas, y = Peso)) +
    labs(y = "Peso (gr)\n", x = "\nTalla (mm)") +
    theme_minimal() +
    theme(axis.text = element_text(color = "black", size = 15),
          axis.title = element_text(size = 18, face = 'bold'),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14, face = 'bold'),
          plot.title = element_text(size = 20, face = "bold"),
          panel.background = element_rect(fill = "#e2e9de"),
          plot.background = element_rect(fill = "#e2e9de"),
          panel.grid.major = element_line(color = "gray60"),
          panel.grid.minor = element_line(color = "gray60")) +
    annotate('text', x = Inf, y = Inf, label =
            ifelse(input_ecuacion_talla_peso, ecuacion[['ecuacion']], ''),
            hjust = 2, vjust = 2, size = 8, color = "blue")

  if (!is_empty(ecuacion[['plot_data']])) {
    p <- p + geom_line(data = ecuacion[['plot_data']],
                       aes(x = Tallas, y = Peso_predicho), color = "red")
  }

  return(p)

}

agregar_etiquetas <- function(plt, id_var, var, variable, tipo_grafico, etiquetas, conds = c('Macho', 'Hembra', 'Otra'), labs = c('♂', '♀', 'Otra')) {

  if (etiquetas) {
    if (tipo_grafico == "dodge") {
      plt <- plt +
        geom_text(aes(x = interaction(id_var, year), y = var, label = case_when(
          variable == conds[1] ~ paste(labs[1], "=", var),
          variable == conds[2] ~ paste(labs[2], "=", var),
          variable == conds[3] ~ paste(labs[3], "=", var)
        ), group = variable),
        position = position_dodge(width = 0.9),
        vjust = -0.5, size = 5, show.legend = FALSE)
    } else if (tipo_grafico == "stack") {
      plt <- plt +
        geom_text(aes(x = interaction(id_var, year), y = var - 0.5 * var, label = case_when(
          variable == conds[1] ~ paste(labs[1], "=", var),
          variable == conds[2] ~ paste(labs[2], "=", var),
          variable == conds[3] ~ paste(labs[3], "=", var)
        ), group = variable),
        vjust = -0.2, size = 5, show.legend = FALSE)
    } else if (tipo_grafico == "fill") {
      plt <- plt +
        geom_text(aes(x = interaction(id_var, year), y = cumsum(var) - 0.5 * var, label = case_when(
          variable == conds[1] ~ paste(labs[1], "=", var),
          variable == conds[2] ~ paste(labs[2], "=", var),
          variable == conds[3] ~ paste(labs[3], "=", var)
        ), group = variable),
        vjust = 0, size = 5, position = position_fill(vjust = 0.5), show.legend = FALSE)
    }
  }
  return(plt)
}



generar_grafico_sx_ratio <- function(data, id_var, var, variable_fill, tipo_grafico,input_axis_y_grafico_sexual_ratio, etiquetas, angle_x_sr, cond = c('Macho','Hembra',''), labs = c('♂','♀',''), legend.text.size = 14, legend.title.size = 14, background.color = "#e2e9de", axis.text.size = 15, axis.title.size = 18, legend.position = 'right') {

  labels_plot = if_else(tipo_grafico == 'fill',  "Proporción de individuos\n",  "Número de individuos\n")

  data = data %>%
    mutate(id_var = !!sym(id_var),
           var = !!sym(var),
           variable = !!sym(variable_fill))

  plt <- data %>%
    filter(!is.na(id_var)) %>%
    ggplot() +
    geom_bar(aes(x = interaction(id_var, year), y = var, fill = variable), stat = "identity", position = tipo_grafico, alpha = 0.7) +
    labs(y = labels_plot, x = paste0('\n',input_axis_y_grafico_sexual_ratio), fill = variable_fill)+
    scale_x_discrete(guide = 'axis_nested') +
    theme(axis.text = element_text(color = "black", size = axis.text.size),
          axis.text.x = element_text(angle = angle_x_sr),
          axis.title = element_text(size = axis.title.size, face = 'bold'),
          legend.text = element_text(size = legend.text.size),
          legend.title = element_text(size = legend.title.size, face = 'bold'),
          plot.title = element_text(size = 20, face = "bold"),
          panel.background = element_rect(fill = background.color),
          plot.background = element_rect(fill = background.color, color = 'black'),
          panel.grid.major = element_line(color = "gray60"),
          panel.grid.minor = element_line(color = "gray60"),
          strip.text = element_text(size = 20, face = "bold"),
          strip.background = element_blank(),
          panel.border = element_rect(color = "black", fill = NA, size = 1),
          legend.background = element_rect(fill = background.color, color = background.color),
          legend.position = legend.position) +
    scale_fill_manual(values = c('#3A6324','#48A3A7','#8BC34A'))

  plt <- agregar_etiquetas(plt, id_var, var, variable, tipo_grafico, etiquetas, cond , labs)

  return(plt)
}


getDataTallaMadurez <- function(data){

  base_logistica = table(data$Tallas, data$Madurez)

  data_logistica = data.frame(tallas = sort(unique(data$Tallas)),
                              inm = as.numeric(base_logistica[, 1]),
                              mad = as.numeric(base_logistica[, 2]))

  data_logistica$total <- apply(data_logistica[,c(2,3)], 1, sum)
  data_logistica$prop  <- data_logistica$mad/data_logistica$total

  return(data_logistica)

}


getModelMadurez <- function(data, a_input, b_input){

  data = data %>% mutate(madurez = ifelse(Madurez %in% 'Inmaduro',0 , 1))

  gonadmat_total_nls <- nls(madurez ~ 1/(1+exp(a-(b*Tallas))), data = data,
                            start = list(a = a_input, b = b_input))

  TPM_total_nls <- as.numeric(coef(gonadmat_total_nls)[1]/coef(gonadmat_total_nls)[2])
  predojiva_total_nls <- predict(gonadmat_total_nls)

  return(list(modelo = gonadmat_total_nls, L_50 = TPM_total_nls, predict_data = predojiva_total_nls))


}



limpiar_data_mapa <- function(data){
  new_data = data %>%
    mutate(Longitud = as.numeric(Longitud),
           Latitud = as.numeric(Latitud),
           Biomasa = as.numeric(Biomasa),
           Densidad = as.numeric(Densidad)) %>%
    filter(!is.na(Longitud),!is.na(Latitud), Latitud < 0, Longitud < 0, Biomasa > 0, Densidad > 0)
  return(new_data)
}


filtrar_puntos_mapa <- function(mapa_densidad_biomasa, especie_mapa) {
  if (especie_mapa == "Todas") {
    return(mapa_densidad_biomasa)
  } else {
    return(mapa_densidad_biomasa %>% filter(Especie == especie_mapa))
  }
}

generar_mapa_plotly <- function(data, variable_mapa) {
  p <- plot_ly(
    data = data,
    lat = ~Latitud,
    lon = ~Longitud
  )

  if (variable_mapa == "Especie") {
    p <- add_trace(
      p,
      data = data,
      mode = 'markers',
      type = 'scattermapbox',
      marker = list(size = 6, opacity = 0.8),
      color = ~as.character(Especie),
      legendgroup = ~Especie,
      name = ~Especie,
      hoverinfo = 'text',
      text = ~paste("Especie: ", Especie, "<br>Biomasa: ", Biomasa, "<br>Densidad: ", Densidad)
    )
  } else {
    variable_a_usar <- switch(
      variable_mapa,
      "Biomasa" = ~Biomasa,
      "Densidad" = ~Densidad,
      ~Especie  # Por defecto
    )

    p <- add_trace(
      p,
      z = variable_a_usar,
      mode = 'markers',
      type = 'scattermapbox',
      marker = list(size = 6, opacity = 0.8),
      color = variable_a_usar,
      hoverinfo = 'text',
      text = ~paste("Especie: ", Especie, "<br>Biomasa: ", Biomasa, "<br>Densidad: ", Densidad)
    )
  }

  p <- p %>%
    layout(
      mapbox = list(
        style = 'open-street-map',
        zoom = 5,
        center = list(lon = -77.20, lat = -11.77)
        ),
      legend = list(
        title = list(
          text = variable_mapa,
          font = list(size = 18, color = "black", family = "Arial", weight = "bold")
        ),
        font = list(size = 12, color = "black", family = "Arial", weight = "bold")
      ),
      hoverinfo = 'text'
    )

}


plotMadurez <- function(data_total, data_proporcion, lista_modelo, mostrar_etiqueta_madurez){

  ggplot(data_proporcion, aes(x = tallas, y = prop)) +
    geom_point(shape = 19, size = 3, color = "black") +
    labs(x = "\nTalla (mm)", y = "Proporción de maduros\n")+
    theme_minimal() +
    theme(axis.text = element_text(color = "black", size = 15),
          axis.title = element_text(size = 18, face = 'bold'),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14, face = 'bold'),
          plot.title = element_text(size = 20, face = "bold"),
          panel.background = element_rect(fill = "#e2e9de"),
          plot.background = element_rect(fill = "#e2e9de"),
          panel.grid.major = element_line(color = "gray60"),
          panel.grid.minor = element_line(color = "gray60"),
          legend.position = c(0.85, 0.2)) +
    geom_line(data = data.frame(length = sort(data_total$Tallas), pred = sort(lista_modelo$predict_data)),
              aes(x = length, y = pred), color = "grey10", size = 1.5) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "red", size = 1.5) +
    geom_vline(xintercept = lista_modelo$L_50, linetype = "dashed", color = "red", size = 1.5) +
    geom_point(data = data.frame(x = lista_modelo$L_50, y = 0.5), aes(x = x, y = y), shape = 17, color = "red", size = 3) +
    annotate("text", x = Inf, y = 0.7, label = ifelse(mostrar_etiqueta_madurez, paste("L[50] =", round(lista_modelo$L_50, 2), "mm"), ''),
             hjust = 2, vjust = 2, size = 6, col = 'blue') +
    ylim(c(0,1))

}


GetDataJuveniles <- function(data, input_especie, input_grupo, meses_espanol, input_talla_minima){


  if (input_grupo == 'Total') {
    new_data = data %>%
      mutate(mes = month(Fecha),
             year = year(Fecha)) %>%
      filter(!is.na(Tallas), Tallas > 0, Especie %in% input_especie) %>%
      summarise(juv = mean(Tallas <= input_talla_minima)*100, .by = c(year, mes)) %>%
      arrange(year, mes)
  } else {
    new_data = data %>%
      mutate(mes = month(Fecha),
             year = year(Fecha)) %>%
      filter(!is.na(Tallas), Tallas > 0, Especie %in% input_especie) %>%
      summarise(juv = mean(Tallas <= input_talla_minima)*100, .by = c(year, mes, {{input_grupo}})) %>%
      arrange(year, mes)
  }

  new_data$mes <- meses_espanol[new_data$mes]

  new_data$mes <- factor(new_data$mes, levels = meses_espanol)

  new_data = complete_year_month(new_data, meses_espanol,  NULL)

  return(new_data)

}

getPlotJuveniles <- function(data, input_grupo, input_especie, mostrar_etiqueta){

  if (input_grupo == 'Total') {

    data$variable = input_especie
    colores_juv = 'black'
    legend_title = 'Especie'

    p = data %>%
      ggplot(aes(x = interaction(mes, year), y = juv, group = 1, colour = factor(variable)))



  } else {

    colores_juv = generarColoresEspecie(data, input_grupo)
    data$variable = data[[input_grupo]]
    legend_title = 'Grupo'

    p = data %>%
      filter(!is.na(variable)) %>%
      ggplot(aes(x = interaction(mes, year), y = juv, group = variable, colour = factor(variable)))

  }

  p = p  +
    geom_point(size = 1.5)+
    geom_line()  +
    labs(x = "",
         y = 'Juveniles (%)\n', colour = legend_title) +
    scale_color_manual(values = colores_juv) +
    scale_x_discrete(guide = 'axis_nested') +
    theme(axis.text = element_text(color = "black", size = 10),
          axis.text.x = element_text(angle = 90),
          axis.title = element_text(size = 13, face = 'bold'),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14, face = 'bold'),
          plot.title = element_text(size = 20, face = "bold"),
          panel.background = element_rect(fill = "#e2e9de"),
          plot.background = element_rect(fill = "#e2e9de", color = 'black'),
          panel.grid.major = element_line(color = "gray60"),
          panel.grid.minor = element_line(color = "gray60"),
          strip.text = element_text(size = 20, face = "bold"),
          strip.background = element_blank(),
          panel.border = element_rect(color = "black", fill = NA, size = 1),
          legend.background = element_rect(fill = "#e2e9de", color = "#e2e9de"))

  if (mostrar_etiqueta) {
    p <- p + geom_text(aes(label = sprintf("%0.2f", juv), vjust = -0.5), show.legend = FALSE, size = 4, family = 'mono', fontface = 'bold')
  }

  return(ggplotly(p))
}


getDataCangrejosOvigeras <- function(data, input_especie, input_variable){

  plot_data = data %>%
    filter(Grupos %in% 'Cangrejos', Especie %in% input_especie, Sexo %in% 'Hembra') %>%
    mutate(
      year = year(Fecha),
      mes = month(Fecha, label = TRUE, abbr = TRUE))

  plot_data$id_var = switch(
    input_variable,
    "Tiempo" = plot_data$mes,
    "Zona" = plot_data$Zona,
    "Sustrato" = as.character(plot_data$Sustrato),
    "Cat_Profundidad" = plot_data$Cat_Profundidad
  )

  plot_data = plot_data %>%
    reframe(n = length(Ovigeras), .by = c(year, id_var , Ovigeras))

  return(plot_data)

}


getPlotCangrejos <- function(data, Ovigeras, input_axis_x_cangrejo_ovigeras, input_tipo_grafico, input_etiquetas, cond = c('No','Si',''), labs = c('No','Si','')){

  labels_plot = if_else(tipo_grafico == 'fill',  "Proporción de individuos\n",  "Número de individuos\n")

  p <- ggplot(data) +
    geom_bar(aes(x = factor(id_var) , y = n, fill = {{Ovigeras}}), stat = "identity", position = input_tipo_grafico, alpha = 0.7) +
    labs(y = labels_plot, x = paste0('\n',input_axis_x_cangrejo_ovigeras)) +
    theme_minimal() +
    theme(axis.text = element_text(color = "black", size = 15),
          axis.title = element_text(size = 18, face = 'bold'),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14, face = 'bold'),
          plot.title = element_text(size = 20, face = "bold"),
          panel.background = element_rect(fill = "#e2e9de"),
          plot.background = element_rect(fill = "#e2e9de"),
          panel.grid.major = element_line(color = "gray60"),
          panel.grid.minor = element_line(color = "gray60")) +
  scale_fill_manual(values = c('#3A6324','#48A3A7','#8BC34A'))

  p <- agregar_etiquetas(p, id_var, n, {{Ovigeras}}, input_tipo_grafico, input_etiquetas, cond, labs)


  return(p)

}


getDataEspecieReproductores <- function(data, grupo , variable, input_especie, input_variable){

  var = rlang::sym(variable)

  new_data = data %>%
    filter(Grupos %in% grupo,
           Especie %in% input_especie,
           !!var > 0,
           !is.na(!!var)) %>%
    mutate(
      year = year(Fecha),
      mes = month(Fecha, label = TRUE, abbr = TRUE))

  new_data$id_var = switch(
    input_variable,
    "Tiempo" = new_data$mes,
    "Zona" = new_data$Zona,
    "Sustrato" = as.character(new_data$Sustrato),
    "Cat_Profundidad" = new_data$Cat_Profundidad
  )

  new_data = new_data %>%
    reframe(n = sum(!!var), .by = c('year','id_var', 'Especie'))

  return(new_data)

}



