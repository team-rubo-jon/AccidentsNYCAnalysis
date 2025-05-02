library(shiny)
library(shinythemes)
library(leaflet)
library(dplyr)
library(lubridate)

# Leer los datos
data_sampled <- read.csv("data_sampled.csv")

# Convertir la columna de fecha a formato adecuado
data_sampled <- data_sampled %>%
  mutate(DATE = mdy(DATE))

# Definir el número de subconjuntos y el tamaño del subconjunto
subconjunto_size <- 500
total_subconjuntos <- ceiling(nrow(data_sampled) / subconjunto_size)

ui <- navbarPage(
  title = p(strong('Visualización de Accidentes de Tráfico')),
  theme = shinytheme('darkly'),
  
  tabPanel("EVOLUCIÓN",
           sidebarLayout(
             sidebarPanel(
               actionButton("start", "SIGUIENTE", class = "btn-primary")
             ),
             mainPanel(
               leafletOutput("mapa_evolucion", height = 500)
             )
           )
  )
)

server <- function(input, output, session) {
  
  # Índice reactivo para saber qué subconjunto mostrar
  current_index <- reactiveVal(1)
  
  # Función para obtener el subconjunto actual
  get_subset <- function(index) {
    start_row <- (index - 1) * subconjunto_size + 1
    end_row <- min(index * subconjunto_size, nrow(data_sampled))
    data_sampled[start_row:end_row, ]
  }
  
  observeEvent(input$start, {
    # Obtener el subconjunto actual de 500 accidentes
    subset_data <- get_subset(current_index())
    
    # Mostrar el subconjunto en el mapa de calor
    output$mapa_evolucion <- renderLeaflet({
      leaflet(subset_data) %>%
        addTiles() %>%
        addHeatmap(
          lng = ~LONGITUDE,
          lat = ~LATITUDE,
          intensity = ~NUM_PERSONS_KILLED + NUM_PERSONS_INJURED,
          radius = 15,
          blur = 20,
          max = 0.05
        )
    })
    
    # Incrementar el índice para el siguiente subconjunto
    if (current_index() < total_subconjuntos) {
      current_index(current_index() + 1)
    } else {
      current_index(1)  # Reiniciar el índice cuando se alcanza el final
    }
  })
}

shinyApp(ui, server)

