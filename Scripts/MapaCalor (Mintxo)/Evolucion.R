library(shiny)
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(lubridate)






# AUN LO TENGO QUE TERMINAR









data_sampled <- read.csv("data_sampled.csv")

data_sampled <- data_sampled %>%
  mutate(DATE = mdy(DATE)) %>%
  arrange(DATE)

subconjunto_size <- 500
total_subconjuntos <- ceiling(nrow(data_sampled) / subconjunto_size)

ui <- navbarPage(
  title = p(strong('Visualización de Accidentes de Tráfico')),
  theme = shinytheme('darkly'),
  
  tabPanel("EVOLUCIÓN",
           sidebarLayout(
             sidebarPanel(
               actionButton("start", "Iniciar", class = "btn-success"),
               actionButton("pause", "Pausar", class = "btn-warning"),
               br(), br()
             ),
             mainPanel(
               leafletOutput("mapa_evolucion", height = 500),
               uiOutput("rango_fechas")
             )
           )
  )
)

server <- function(input, output, session) {
  
  current_index <- reactiveVal(1)
  
  get_subset <- function(index) {
    start_row <- (index - 1) * subconjunto_size + 1
    end_row <- min(index * subconjunto_size, nrow(data_sampled))
    data_sampled[start_row:end_row, ]
  }
  
  # Renderizar el primer subconjunto al iniciar la app
  observe({
    subset_data <- get_subset(current_index())
    
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
    
    output$rango_fechas <- renderUI({
      fecha_min <- min(subset_data$DATE, na.rm = TRUE)
      fecha_max <- max(subset_data$DATE, na.rm = TRUE)
      rango <- paste0(format(fecha_min, "%Y/%m/%d"), 
                      " - ", format(fecha_max, "%Y/%m/%d"))
      
      div(
        style = "margin-top: 15px; padding: 10px; background-color: #222;
                 color: #fff; font-size: 20px; text-align: center;
                 border-radius: 8px; border: 1px solid #444;",
        rango
      )
    })
  })
  
  # Al presionar el botón, avanzar al siguiente subconjunto
  observeEvent(input$start, {
    new_index <- if (current_index() < total_subconjuntos) {
      current_index() + 1
    } else {
      1
    }
    current_index(new_index)
  })
}

shinyApp(ui, server)

