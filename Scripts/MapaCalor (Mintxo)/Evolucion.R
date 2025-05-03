library(shiny)
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(lubridate)

data_sampled <- read.csv("data_sampled.csv")
data_sampled <- data_sampled %>%
  mutate(DATE = mdy(DATE)) %>%
  arrange(DATE)

window_size <- 2500
step_size <- 20
interval_ms <- 200

ui <- navbarPage(
  title = p(strong('Visualización de Accidentes')),
  theme = shinytheme('darkly'),
  
  tabPanel("EVOLUCIÓN",
           sidebarLayout(
             sidebarPanel(
               tags$div(
                 style = "background-color: #333; padding: 10px; border-radius: 8px; 
                   color: #fff; font-weight: bold; font-size: 16px; 
                   border: 1px solid #444; margin-bottom: 15px; text-align: center;", 
                 "CONTROLES"
               ),
               tags$div(
                 style = "display: flex; justify-content: center; gap: 10px; margin-top: 20px;",
                 actionButton("start", "Comenzar", class = "btn-success", 
                              style = "border: none; background-color: #28a745;"),
                 actionButton("pause", "Pausar", class = "btn-warning", 
                              style = "border: none; background-color: #ffc107;")
               ),
               br(), br(),
               uiOutput("timeline_bar"),
               br(), br(),
               div(
                 style = "background-color: #333; padding: 15px; border-radius: 8px; 
                   color: #fff; font-size: 14px; border: 1px solid #444; margin-top: 20px;",
                 tags$p(style = "font-weight: bold;", "Descripción"),
                 p("Este mapa muestra la evolución temporal de accidentes de tráfico en Nueva York.",br(),br(),
                   
                   "Las zonas más calientes indican una mayor concentración de accidentes que hayan causado heridos o fallecidos")
               )
             ),
             mainPanel(
               leafletOutput("mapa_evolucion", height = 600),
               br()
             )
           )
  )
)

server <- function(input, output, session) {
  index <- reactiveVal(1)
  running <- reactiveVal(FALSE)
  
  observeEvent(input$start, {
    running(TRUE)
  })
  
  observeEvent(input$pause, {
    running(FALSE)
  })
  
  output$mapa_evolucion <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -73.9, lat = 40.7128, zoom = 11)
  })
  
  observe({
    invalidateLater(interval_ms, session)
    req(running())
    
    i <- isolate(index())
    
    if ((i + window_size) <= nrow(data_sampled)) {
      subset <- data_sampled[i:(i + window_size), ]
      index(i + step_size)
    } else {
      subset <- data_sampled[1:window_size, ]
      index(1)
    }
    
    leafletProxy("mapa_evolucion", data = subset) %>%
      clearGroup("heat") %>%
      addHeatmap(
        lng = ~LONGITUDE,
        lat = ~LATITUDE,
        intensity = ~5 * NUM_PERSONS_KILLED + NUM_PERSONS_INJURED,
        radius = 15,
        blur = 20,
        max = 0.05,
        group = "heat"
      )
    
    output$timeline_bar <- renderUI({
      fecha_min <- min(data_sampled$DATE, na.rm = TRUE)
      fecha_max <- max(data_sampled$DATE, na.rm = TRUE)
      current_min <- min(subset$DATE, na.rm = TRUE)
      
      porcentaje <- round(
        as.numeric(difftime(current_min, fecha_min, units = "days")) /
          as.numeric(difftime(fecha_max, fecha_min, units = "days")) * 100
      )
      
      tagList(
        div("Progreso temporal:", style = "color: #fff; font-weight: bold; margin-bottom: 5px;"),
        tags$div(style = "background-color: #444; height: 20px; border-radius: 10px; overflow: hidden;",
                 tags$div(style = paste0(
                   "height: 100%; width: ", porcentaje, "%; background-color: #00c8ff;
             transition: width 0.2s; border-radius: 10px;"
                 ))
        ),
        div(
          paste0(format(current_min, "%Y/%m/%d"), " → ", 
                 format(max(subset$DATE, na.rm = TRUE), "%Y/%m/%d")),
          style = "color: #ccc; font-size: 14px; margin-top: 5px;"
        )
      )
    })
  })
}

shinyApp(ui, server)

