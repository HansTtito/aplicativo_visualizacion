InfoBoxFn <- function(){

  fluidRow(
    column(
      width = 12,
      valueBox(value = 47,
               subtitle = 'Moluscos',
               icon = fa_i(name = 'octopus-deploy'),
               color = 'green',
               width = 3),
      valueBox(value = 22,
               subtitle = 'Crustáceos',
               icon = fa_i(name = 'shrimp'),
               color = 'green',
               width = 3),
      valueBox(value = 3,
               subtitle = 'Equinodermos',
               color = 'green',
               width = 3),
      valueBox(value = 3,
               subtitle = 'Anémonas',
               color = 'green',
               width = 3)
    )
  )


}

