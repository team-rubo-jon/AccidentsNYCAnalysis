library(shiny)
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(lubridate)

data_sampled <- read.csv("data_sampled.csv")

#Cambiar formato de la columna DATE
data_sampled <- data_sampled %>%
  mutate(DATE = mdy(DATE))


ui <- navbarPage(
  p(strong('Visualización de Accidentes de Tráfico')),
  theme = shinytheme('darkly'),
  
  tabPanel("MAPA",
           sidebarLayout(
             sidebarPanel(
               checkboxGroupInput("tipo_usuario", "Víctimas implicadas:",
                                  choices = c("Sin heridos", "Peatones", "Ciclistas", "Motoristas"),
                                  selected = c("Sin heridos","Peatones", "Ciclistas", "Motoristas")),
               
               selectInput("causa", "Causa del accidente:",
                           choices = c("Todas", unique(data_sampled$CAUSE)),
                           selected = "Todas"),
               br(),
               uiOutput("num_accidentes")
             ),
             
             mainPanel(
               leafletOutput("mapa_accidentes", height=500),
               style = "margin-bottom: 10px;"
             )
           ),
           
           div(
             style = "position: fixed; bottom: 0; left: 0; right: 0; background-color: #2c3e50; padding: 10px; z-index: 9999; 
                      margin-bottom: 10px; margin-left: 15px; margin-right: 15px; border-radius: 5px;",
             sliderInput("Fecha", "RANGO DE FECHAS:",
                         min = as.Date(min(data_sampled$DATE)),
                         max = as.Date(max(data_sampled$DATE)),
                         value = c(as.Date(min(data_sampled$DATE)), as.Date(max(data_sampled$DATE))),
                         timeFormat = "%Y-%m-%d",
                         width = "100%")
           )
  )
)

server <- function(input, output, session) {
  
  data_filtrada <- reactive({
    df <- data_sampled %>%
      mutate(DATE = as.Date(DATE))
    
    # Filtro por fechas
    df <- df %>%
      filter(DATE >= input$Fecha[1], DATE <= input$Fecha[2])
    
    # Filtro por causa del accidente
    if (input$causa != "Todas") {
      df <- df %>% filter(CAUSE == input$causa)
    }
    
    # Filtro por tipo de usuario
    if (length(input$tipo_usuario) == 0) {
      df <- df[0, ]
    } else {
      condiciones <- list()
      
      if ("Sin heridos" %in% input$tipo_usuario) {
        condiciones[[length(condiciones) + 1]] <- (
          df$NUM_PERSONS_INJURED + df$NUM_PERSONS_KILLED == 0
        )
      }
      
      if ("Peatones" %in% input$tipo_usuario) {
        condiciones[[length(condiciones) + 1]] <- (
          df$NUM_PEDESTRIANS_INJURED + df$NUM_PEDESTRIANS_KILLED > 0
        )
      }
      
      if ("Ciclistas" %in% input$tipo_usuario) {
        condiciones[[length(condiciones) + 1]] <- (
          df$NUM_CYCLIST_INJURED + df$NUM_CYCLIST_KILLED > 0
        )
      }
      
      if ("Motoristas" %in% input$tipo_usuario) {
        condiciones[[length(condiciones) + 1]] <- (
          df$NUM_MOTORIST_INJURED + df$NUM_MOTORIST_KILLED > 0
        )
      }
      
      df <- df[Reduce("|", condiciones), ]
    }
    
    df
  })
  
  output$num_accidentes <- renderUI({
    n <- nrow(data_filtrada())
    
    div(style = "padding: 8px; background-color: #3c3c3c; border: 1px solid #555; border-radius: 4px; color: white;",
        HTML(paste0("<strong>Nº de accidentes:</strong> ", n))
    )
  })
  
  output$mapa_accidentes <- renderLeaflet({
    df <- data_filtrada()
    
    df_fallecidos <- df %>%
      filter(NUM_PERSONS_KILLED > 0)
    
    leaflet(df) %>%
      addTiles() %>%
      addHeatmap(
        lng = ~LONGITUDE,
        lat = ~LATITUDE,
        radius = 12,
        blur = 15,
        max = 0.05
      ) %>%
      addLabelOnlyMarkers(
        data = df_fallecidos,
        lng = ~LONGITUDE,
        lat = ~LATITUDE,
        label = "†",
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = "top",
          textOnly = TRUE,
          style = list(
            "color" = "black",
            "font-size" = "16px",
            "font-weight" = "bold"
          )
        )
      ) %>%
      addControl(
        html = HTML("<div style='background-color: rgba(255,255,255,0.8); padding: 6px; border-radius: 4px; font-size: 14px;'>
                  <b>†: Fallecidos</b>
                  </div>"),
        position = "bottomright"
      )
  })
  
}

shinyApp(ui, server)

