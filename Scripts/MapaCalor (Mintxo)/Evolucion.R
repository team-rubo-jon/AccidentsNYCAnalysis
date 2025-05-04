library(shiny)
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(lubridate)

data_sampled <- read.csv("data_sampled.csv")
data_sampled <- data_sampled %>%
  mutate(DATE = as.Date(DATE, format = "%Y-%m-%d")) %>%
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
               
               dateInput(
                 "fecha_inicio",
                 label = "Selecciona fecha de inicio:",
                 value = min(data_sampled$DATE, na.rm = TRUE),
                 min = min(data_sampled$DATE, na.rm = TRUE),
                 max = max(data_sampled$DATE, na.rm = TRUE),
                 format = "yyyy-mm-dd"
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
               br(),
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
    
    fecha_inicio <- input$fecha_inicio
    i <- isolate(index())
    
    if (!is.null(fecha_inicio)) {
      pos_inicio <- which.max(data_sampled$DATE >= fecha_inicio)
      if (i < pos_inicio) {
        index(pos_inicio)
        return()
      }
    }
    
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
      current_start <- min(subset$DATE, na.rm = TRUE)
      current_end <- max(subset$DATE, na.rm = TRUE)
      
      # Convert to numeric for consistent difference
      total_days <- as.numeric(fecha_max - fecha_min)
      start_days <- as.numeric(current_start - fecha_min)
      end_days <- as.numeric(current_end - fecha_min)
      
      start_pct <- (start_days / total_days) * 100
      end_pct <- (end_days / total_days) * 100
      width_pct <- end_pct - start_pct
      
      años <- seq(year(fecha_min), year(fecha_max), by = 1)
      
      tagList(
        div("Línea temporal:", style = "color: #fff; font-weight: bold; margin-bottom: 10px;"),
        
        div(style = "position: relative; height: 60px; background-color: transparent; width: 100%;",
            
            # Línea base
            div(style = "position: absolute; top: 30px; left: -4%; width: 103%; height: 2px; background-color: #888;"),
            
            # Ticks y etiquetas
            lapply(seq_along(años), function(i) {
              left_pos <- paste0((as.numeric(as.Date(paste0(años[i], "-01-01")) - fecha_min) / total_days) * 100, "%")
              label <- if (años[i] %% 2 == 0) as.character(años[i]) else NULL
              
              tagList(
                div(style = paste0("position: absolute; left: ", left_pos, 
                                   "; top: 25px; width: 1px; height: 10px; background-color: #ccc;")),
                if (!is.null(label)) {
                  div(label,
                      style = paste0(
                        "position: absolute; left: ", left_pos, 
                        "; top: 0px; transform: rotate(-45deg); transform-origin: left bottom; ",
                        "font-size: 10px; color: #ccc;"
                      )
                  )
                }
              )
            }),
            
            # Indicador azul
            div(
              style = paste0(
                "position: absolute; top: 26px; left: ", start_pct, "%; width: ", width_pct, "%; ",
                "height: 8px; background-color: #00c8ff; border-radius: 5px; ",
                "box-shadow: 0 0 5px #00c8ff; transition: left 0.2s, width 0.2s;"
              )
            )
        ),
        
        div(
          paste0(format(current_start, "%Y/%m/%d"), " → ", format(current_end, "%Y/%m/%d")),
          style = "color: #ccc; font-size: 14px; margin-top: 10px; text-align: center;"
        )
      )
    })
    
  })
}

shinyApp(ui, server)

