library(shiny)
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(lubridate)

data_sampled <- read.csv("Scripts/data_sampled.csv")

data_sampled <- data_sampled %>%
  mutate(DATE = mdy(DATE)) %>%
  filter(!is.na(LONGITUDE), !is.na(LATITUDE),
         LONGITUDE != 0, LATITUDE != 0)

ui <- navbarPage(
  title = p(strong('Visualización de Accidentes de Tráfico')),
  theme = shinytheme('darkly'),
  
  tabPanel("EVOLUCIÓN",
           sidebarLayout(
             sidebarPanel(
               actionButton("start", "Iniciar Animación", class = "btn-primary"),
               actionButton("reset", "Reiniciar", class = "btn-secondary")
             ),
             mainPanel(
               leafletOutput("mapa_evolucion", height = 500)
             )
           )
  )
)

server <- function(input, output, session) {
  
  evol_index <- reactiveVal(1)
  animando <- reactiveVal(FALSE)
  
  data_reciente <- data_sampled %>%
    arrange(desc(DATE)) %>%
    slice(1:500)
  
  observeEvent(input$start, {
    animando(TRUE)
  })
  
  observeEvent(input$reset, {
    animando(FALSE)
    evol_index(1)
    leafletProxy("mapa_evolucion") %>% clearGroup("heatmap")
  })
  
  observe({
    invalidateLater(500, session)
    if (animando()) {
      if (evol_index() < nrow(data_reciente)) {
        evol_index(evol_index() + 10)
      } else {
        animando(FALSE)
      }
    }
  })
  
  output$mapa_evolucion <- renderLeaflet({
    leaflet() %>%
      addTiles()
  })
  
  observe({
    req(animando())
    
    idx <- evol_index()
    df_evol <- data_reciente[1:idx, ]
    
    leafletProxy("mapa_evolucion", data = df_evol) %>%
      clearGroup("heatmap") %>%
      addHeatmap(
        lng = ~LONGITUDE,
        lat = ~LATITUDE,
        radius = 12,
        blur = 15,
        max = 0.05,
        group = "heatmap"
      )
  })
}

shinyApp(ui, server)

