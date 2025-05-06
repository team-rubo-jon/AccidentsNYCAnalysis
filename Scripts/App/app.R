library(shiny)
library(shinythemes)
library(bslib)
library(tidyverse)
library(lubridate)
library(factoextra)
library(cluster)
library(dendextend)
library(zoo)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(tsibble)
library(DT)
library(stringdist)
library(plotly)
library(ggiraph)
library(gdtools)
library(gfonts)

data_sampled <- read.csv("data_sampled.csv")

data_sampled <- data_sampled |> 
  mutate(DATE = as.Date(DATE, format = "%Y-%m-%d")) |> 
  arrange(DATE)

# Procesamiento de datos
data_time <- data_sampled |>
  mutate(DATE = as.Date(DATE, format = "%Y-%m-%d")) |>
  mutate(DATE_MONTH = yearmonth(DATE),
         DATE_YEAR = year(DATE)) |>
  add_count(DATE, name = "FREQ_DAY") |>
  add_count(DATE_MONTH, name = "FREQ_MONTH") |>
  add_count(DATE_YEAR, name = "FREQ_YEAR") |>
  dplyr::select(DATE, DATE_MONTH, DATE_YEAR, FREQ_DAY, FREQ_MONTH, FREQ_YEAR, BOROUGH) |>
  arrange(DATE)

min_date <- min(data_time$DATE)
max_date <- max(data_time$DATE)

borough_choices <- unique(na.omit(data_sampled$BOROUGH))

window_size <- 2500
step_size <- 20
interval_ms <- 200

# Variables HOUR y DAYS_OF_WEEK para el análisis de correspondencia:
data_sampled <- data_sampled |> 
  mutate(HOUR = as.numeric(format(strptime(TIME, format = "%H:%M"), "%H")),
         DAY_OF_WEEK = weekdays(as.Date(DATE, format = "%Y-%m-%d")))
data_sampled$HOUR <- as.factor(data_sampled$HOUR)
data_sampled$DAY_OF_WEEK <- toupper(data_sampled$DAY_OF_WEEK)
data_sampled$DAY_OF_WEEK <- factor(trimws(data_sampled$DAY_OF_WEEK))

# Crear tema personalizado
ny_theme <- bs_theme(
  version = 5,
  primary = "#FFD100", # Amarillo taxi
  secondary = "#2E2E2E",     
  success = "#1F3B73",       
  danger = "#FF4C4C",        
  bg = "#F4F4F4",            
  fg = "#2E2E2E",            
  base_font = font_google("Roboto Condensed"),
  heading_font = font_google("Bebas Neue"),
  code_font = font_google("Fira Code")
)

theme_nyc <- function() {
  theme_minimal(base_family = "Roboto Condensed") +
    theme(
      plot.background = element_rect(fill = "#F4F4F4", color = NA),
      panel.background = element_rect(fill = "#F4F4F4", color = NA),
      panel.grid.major = element_line(color = "#DADADA"),
      panel.grid.minor = element_blank(),
      axis.title = element_text(color = "#2E2E2E", size = 12, face = "bold"),
      axis.text = element_text(color = "#2E2E2E"),
      plot.title = element_text(color = "#1F3B73", size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(color = "#2E2E2E", hjust = 0.5),
      legend.background = element_rect(fill = "#F4F4F4"),
      legend.key = element_rect(fill = "#F4F4F4"),
      legend.text = element_text(color = "#2E2E2E"),
      strip.background = element_rect(fill = "#FFD100"),
      strip.text = element_text(color = "#2E2E2E", face = "bold")
    )
}

ui <- tagList(
  tags$head(
    tags$style(HTML("
    /* Estilo del título */
    .main-title {
      font-family: 'Bebas Neue', sans-serif;
      font-size: 40px;
      color: #FFD100;
      background-color: #333;
      border-radius: 8px;
      padding: 10px 20px;
      margin: 10px 20px;
      border: 2px solid #FFD100;
      display: flex;
      align-items: center;
      gap: 10px;
    }

    /* Negrita en todas las pestañas */
    .navbar-nav > li > a {
      font-weight: bold !important;
      font-size: 17px;
    }
  "))
  ),
  div(
    class = "main-title",
    icon("car-crash"),
    "VISUALIZACIÓN ACCIDENTES TRÁFICO NY"
  ),
  navbarPage(
    title = NULL,
    theme = ny_theme,
    
    # Panel de Visualización
    tabPanel("Visualización de datos 📊",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   "main_view_viz",
                   "Seleccionar vista:",
                   choices = c(
                     "Frecuencia de Accidentes 🚦" = "freq",
                     "Frecuencia de Accidentes por Distrito 🏙️" = "freq_bar",
                     "Heridos vs Muertos 💀 " = "var",
                     "Frecuencia de Causas 🚑" = "causes"
                   ),
                   selected = "freq"
                 ),
                 
                 # Paneles Condicionales
                 conditionalPanel(
                   condition = "input.main_view_viz == 'freq'",
                   h4("Configuración de Frecuencia Temporal"),
                   selectInput("freq_type", "Frecuencia temporal:",
                               choices = c("Diario" = "daily", "Mensual" = "monthly", "Anual" = "yearly"),
                               selected = "yearly"),
                   radioButtons("graph_type", "Tipo de gráfico:",
                                choices = c("Barras" = "bar", "Líneas" = "line", "Combinado" = "combo"),
                                selected = "combo"),
                   dateRangeInput('date_range_freq', 'Rango de fechas:',
                                  start = min_date, end = max_date,
                                  min = min_date, max = max_date,
                                  format = 'yyyy-mm-dd', startview = 'year', language = 'es',
                                  separator = " a ")
                 ),
                 
                 conditionalPanel(
                   condition = "input.main_view_viz == 'freq_bar'",
                   h4("Configuración de Gráficos por Distrito"),
                   checkboxGroupInput("borough_freq", "Seleccionar Distrito(s):",
                                      choices = borough_choices, selected = borough_choices),
                   actionButton("select_all_freq", "Seleccionar Todos"),
                   actionButton("deselect_all_freq", "Deseleccionar Todos"),
                   dateRangeInput('date_range_freq', 'Rango de fechas:',
                                  start = min_date, end = max_date,
                                  min = min_date, max = max_date,
                                  format = 'yyyy-mm-dd', startview = 'year', language = 'es',
                                  separator = " a ")
                 ),
                 
                 conditionalPanel(
                   condition = "input.main_view_viz == 'var' || input.main_view_viz == 'causes'",
                   h4("Configuración de Variables"),
                   selectInput("borough", "Seleccionar Distrito:",
                               choices = c("Todos", unique(data_sampled$BOROUGH)), selected = "Todos"),
                   dateRangeInput('date_range_var', 'Filtrar por rango de fechas',
                                  start = min_date, end = max_date,
                                  min = min_date, max = max_date,
                                  format = 'yyyy-mm-dd', startview = 'year', language = 'es',
                                  separator = " a ")
                 ),
                 
                 conditionalPanel(
                   condition = "input.main_view_viz == 'causes'",
                   sliderInput("top_causes", "Número de causas más frecuentes:",
                               min = 5, max = length(unique(data_sampled$CAUSE)), value = 10, step = 1)
                 )
               ),
               
               mainPanel(
                 conditionalPanel(
                   condition = "input.main_view_viz == 'freq'",
                   tabsetPanel(
                     tabPanel('Gráfico Temporal Combinado', plotOutput('combined_plot'), textOutput("summary_text"))
                   )
                 ),
                 
                 conditionalPanel(
                   condition = "input.main_view_viz == 'freq_bar'",
                   tabsetPanel(
                     tabPanel('Distribución por Distrito', plotOutput('accident_borough')),
                     tabPanel('Evolución por Distrito', plotlyOutput("accident_evolution"))
                   )
                 ),
                 
                 conditionalPanel(
                   condition = "input.main_view_viz == 'var'",
                   tabsetPanel(
                     tabPanel('Heridos y Fallecidos', plotOutput('injury_death_plot'))
                   )
                 ),
                 
                 conditionalPanel(
                   condition = "input.main_view_viz == 'causes'",
                   tabsetPanel(
                     tabPanel('Causas de Accidentes', plotOutput('frequency_of_causes'))
                   )
                 )
               )
             )
    ),
    
    # Panel de Mapas
    tabPanel("Mapas 🗺️",
             sidebarLayout(
               sidebarPanel(
                 selectInput("main_view_map", "Seleccionar vista:",
                             choices = c("Mapa de calor 🌡️" = "hot_map", "️Mapa evolutivo 📈"= "evolution_map"),
                             selected = "hot_map"),
                 
                 conditionalPanel(
                   condition = "input.main_view_map == 'hot_map'",
                   h4("Configuración del mapa"),
                   checkboxGroupInput("tipo_usuario", "Víctimas implicadas:",
                                      choices = c("Incluir heridos", "Peatones", "Ciclistas", "Motoristas"),
                                      selected = c("Incluir heridos", "Peatones", "Ciclistas", "Motoristas")),
                   selectInput("causa", "Causa del accidente:",
                               choices = c("Todas", unique(data_sampled$CAUSE)), selected = "Todas"),
                   br(),
                   uiOutput("num_accidentes"),
                   div(style = "position: fixed; bottom: 0; left: 0; right: 0; background-color: #cccccc; padding: 10px; z-index: 9999; border: 1px solid #000000; border-radius: 5px; margin: 10px;",
                       sliderInput("Fecha",
                                   label = strong("RANGO DE FECHAS"),
                                   min = as.Date(min(data_sampled$DATE)), max = as.Date(max(data_sampled$DATE)),
                                   value = c(as.Date(min(data_sampled$DATE)), as.Date(max(data_sampled$DATE))),
                                   timeFormat = "%Y-%m-%d", width = "100%"))
                 ),
                 
                 conditionalPanel(
                   condition = "input.main_view_map == 'evolution_map'",
                   h4("Botones de control"),
                   dateInput("fecha_inicio", "Selecciona fecha de inicio:",
                             value = min(data_sampled$DATE),
                             min = min(data_sampled$DATE), max = max(data_sampled$DATE),
                             format = "yyyy-mm-dd"),
                   div(style = "display: flex; justify-content: center; gap: 10px; margin-top: 20px;",
                       actionButton("start", "Comenzar", class = "btn-success"),
                       actionButton("pause", "Pausar", class = "btn-warning")),
                   br(),
                   uiOutput("timeline_bar"),
                   br(),
                   div(style = "background-color: #e0e0e0; padding: 15px; border-radius: 8px;",
                       tags$p(style = "font-weight: bold;", "Descripción"),
                       p("Este mapa muestra la evolución temporal de accidentes de tráfico en Nueva York.", br(),
                         "Las zonas más calientes indican una mayor concentración de accidentes que hayan causado heridos o fallecidos"))
                 )
               ),
               
               mainPanel(
                 conditionalPanel(
                   condition = "input.main_view_map == 'hot_map'",
                   leafletOutput("mapa_accidentes", height = 500)
                 ),
                 conditionalPanel(
                   condition = "input.main_view_map == 'evolution_map'",
                   leafletOutput("evolution_map", height = 500)
                 )
               )
             )
    ),
    
    # Panel analisis de interes
    tabPanel(
      tags$div("Análisis de interés 💡️", style = "font-size: 18px; font-weight: bold;"),
      
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "main_view_analysis",
            "Seleccionar vista:",
            choices = c(
              "Análisis Cluster Jerárquico (Distritos) 💠️" = "cluster_dis",
              "Análisis Cluster Jerárquico (Causas) 💠️" = "cluster_cau",
              "Análisis de Correspondencia 👥" = "corresp"
            ),
            selected = "cluster_dis"
          ),
          
          # Widgets Análisis Jerárquico Distritos
          conditionalPanel(
            condition = "input.main_view_analysis == 'cluster_dis'",
            checkboxGroupInput("plot_choices_dis", 
                               "Selecciona las opciones para ver:",
                               choices = c("Dendrograma", "Silueta", "Tabla"),
                               selected = c("Dendrograma", "Silueta", "Tabla")),
            sliderInput("k_dis", 
                        "Número de grupos (k):", 
                        min = 2, max = 4, value = 3),
            checkboxInput("show_rect", "Mostrar rectángulos en el dendrograma", value = TRUE)
          ),
          
          # Widgets Análisis Jerárquico causas
          conditionalPanel(
            condition = "input.main_view_analysis == 'cluster_cau'",
            checkboxGroupInput("plot_choices_cau", 
                               "Selecciona las opciones para ver:",
                               choices = c("Dendrograma", "Silueta", "Tabla"),
                               selected = c("Dendrograma", "Silueta", "Tabla")),
            sliderInput("k_cau", 
                        "Número de grupos (k):", 
                        min = 2, max = 20, value = 8)
          ),
          
          # Widgets Análisis de Correspondencia
          conditionalPanel(
            condition = "input.main_view_analysis == 'corresp'",
            h4("Seleccione dos variables categóricas"),
            radioButtons("var1", "Elige Variable 1:", 
                         choices = c("VEHICLE_1", "CAUSE", "HOUR", "DAY_OF_WEEK"),
                         selected = "VEHICLE_1"),
            radioButtons("var2", "Elige Variable 2:", 
                         choices = c("VEHICLE_1", "CAUSE", "HOUR", "DAY_OF_WEEK"),
                         selected = "CAUSE"),
            tags$script(HTML("
          Shiny.addCustomMessageHandler('disable_radio_option', function(data) {
            var inputId = data.inputId;
            var optionToDisable = data.option;
            var radios = document.getElementsByName(inputId);
            for (var i = 0; i < radios.length; i++) {
              if (radios[i].value === optionToDisable) {
                radios[i].disabled = true;
                radios[i].parentElement.style.color = '#999999';
              } else {
                radios[i].disabled = false;
                radios[i].parentElement.style.color = '';
              }
            }
          });
        "))
          )
        ),
        
        mainPanel(
          # Panel principal Análisis Jerárquico Distritos
          conditionalPanel(
            condition = "input.main_view_analysis == 'cluster_dis'",
            conditionalPanel(
              condition = "input.plot_choices_dis.indexOf('Dendrograma') > -1",
              plotOutput("dendrogram_plot_borough", height = "500px")
            ),
            conditionalPanel(
              condition = "input.plot_choices_dis.indexOf('Silueta') > -1",
              plotOutput("silhouette_plot_borough")
            ),
            conditionalPanel(
              condition = "input.plot_choices_dis.indexOf('Tabla') > -1",
              DTOutput("borough_table")
            )
          ),
          
          # Panel principal Análisis Jerárquico Causas
          conditionalPanel(
            condition = "input.main_view_analysis == 'cluster_cau'",
            conditionalPanel(
              condition = "input.plot_choices_cau.indexOf('Dendrograma') > -1",
              plotOutput("dendrogram_plot_causes", height = "500px")
            ),
            conditionalPanel(
              condition = "input.plot_choices_cau.indexOf('Silueta') > -1",
              plotOutput("silhouette_plot_causes")
            ),
            conditionalPanel(
              condition = "input.plot_choices_cau.indexOf('Tabla') > -1",
              DTOutput("causes_table")
            )
          ),
          
          # Panel principal Análisis de Correspondencia
          conditionalPanel(
            condition = "input.main_view_analysis == 'corresp'",
            h4("Interpretación del análisis"),
            verbatimTextOutput("ca_interpretation"),
            tags$style(HTML("
          #ca_interpretation {
            white-space: normal;
            overflow-y: visible !important;
            height: auto !important;
          }
        ")),
            fluidRow(
              column(6, plotlyOutput("ca_biplot", height = "500px")),
              column(6, plotOutput("ca_contrib", height = "500px"))
            )
          )
        )
      )
    )
  )
)


server <- function(input, output, session) {
  
  observeEvent(input$select_all_freq, {
    updateCheckboxGroupInput(session, "borough_freq", selected = borough_choices)
  })
  
  observeEvent(input$deselect_all_freq, {
    updateCheckboxGroupInput(session, "borough_freq", selected = character(0))
  })
  
  filtered_data_freq <- reactive({
    req(input$date_range_freq)
    
    df <- data_time |>
      filter(DATE >= input$date_range_freq[1], 
             DATE <= input$date_range_freq[2])
    
    if (length(input$borough_freq) > 0) {
      df <- df |> 
        filter(BOROUGH %in% input$borough_freq)
    } else {
      return(NULL)
    }
    
    return(df)
  })
  
  filtered_data_var <- reactive({
    req(input$date_range_var)
    df <- data_sampled |> 
      filter(DATE >= input$date_range_var[1], DATE <= input$date_range_var[2])

    if (input$borough != "Todos") {
      df <- df |> 
        filter(BOROUGH == input$borough)
    }
    return(df)
  })
  
  # Datos filtrados para el mapa
  filtered_data_map <- reactive({
    df <- data_sampled |> 
      mutate(DATE = as.Date(DATE))
    
    df <- df |> 
      filter(DATE >= input$Fecha[1], DATE <= input$Fecha[2])
    
    if (input$causa != "Todas") {
      df <- df |> filter(CAUSE == input$causa)
    }

    if (length(input$tipo_usuario) == 0) {
      df <- df[0, ]
    } else {
      condiciones <- list()
      
      if ("Incluir heridos" %in% input$tipo_usuario) {
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
  
  # Mapa evolucion
  index <- reactiveVal(1)
  running <- reactiveVal(FALSE)
  
  observeEvent(input$start, {
    running(TRUE)
  })
  
  observeEvent(input$pause, {
    running(FALSE)
  })
  
  output$evolution_map <- renderLeaflet({
    leaflet() |> 
      addTiles() |> 
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
    
    leafletProxy("evolution_map", data = subset) %>%
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
      
      total_days <- as.numeric(fecha_max - fecha_min)
      start_days <- as.numeric(current_start - fecha_min)
      end_days <- as.numeric(current_end - fecha_min)
      
      start_pct <- (start_days / total_days) * 100
      end_pct <- (end_days / total_days) * 100
      width_pct <- end_pct - start_pct
      
      años <- seq(year(fecha_min), year(fecha_max), by = 1)
      
      tagList(
        div("Línea temporal:",
            style = "color: #FFD100; font-weight: bold; font-size: 14px; margin-bottom: 10px; background-color: #666666; padding: 5px; width: fit-content; border-radius: 5px;"
        ),
        
        div(style = "position: relative; height: 60px; background-color: transparent; width: 100%;",
            
            div(style = "position: absolute; top: 30px; left: -1%; width: 97%; height: 2px; background-color: #000000;"),

            lapply(seq_along(años), function(i) {
              pct_pos <- (as.numeric(as.Date(paste0(años[i], "-01-01")) - fecha_min) / total_days) * 100
              left_pos <- paste0(3 + pct_pos * 0.94, "%")
              label <- if (años[i] %% 2 == 0) as.character(años[i]) else NULL
              
              tagList(
                div(style = paste0("position: absolute; left: ", left_pos, 
                                   "; top: 25px; width: 1px; height: 10px; background-color: #000000;")),
                if (!is.null(label)) {
                  div(label,
                      style = paste0(
                        "position: absolute; left: ", left_pos, 
                        "; top: 0px; transform: rotate(-45deg); transform-origin: left bottom; ",
                        "font-size: 10px; color: #000000;"
                      )
                  )
                }
              )
            }),
            
            div(
              style = paste0(
                "position: absolute; top: 26px; left: ", 3 + start_pct * 0.94, "%; width: ", width_pct * 0.94, "%; ",
                "height: 8px; background-color: #00c8ff; border-radius: 5px; ",
                "box-shadow: 0 0 6px #00c8ff, 0 0 10px #00c8ff; transition: left 0.3s, width 0.3s;"
              )
            )
        ),
        
        div(
          paste0(format(current_start, "%Y/%m/%d"), " → ", format(current_end, "%Y/%m/%d")),
          style = "color: #000000; font-size: 13px; margin-top: 8px; text-align: center; font-style: italic;"
        )
      )
      
    })
  })
  
  #Grafico barra y lineas
  output$combined_plot <- renderPlot({
    df <- filtered_data_freq()
    if (is.null(df)) return()
    
    if (input$freq_type == "daily") {
      df_agg <- df |>
        group_by(DATE) |>
        summarise(y = sum(FREQ_DAY, na.rm = TRUE), .groups = "drop") |>
        rename(x = DATE)
      title <- "Frecuencia de Accidentes por Día"
      xlab <- "Fecha"
    } else if (input$freq_type == "monthly") {
      df_agg <- df |>
        group_by(DATE_MONTH) |>
        summarise(y = sum(FREQ_MONTH, na.rm = TRUE), .groups = "drop") |>
        rename(x = DATE_MONTH)
      title <- "Frecuencia de Accidentes por Mes"
      xlab <- "Mes"
    } else {
      df_agg <- df |>
        group_by(DATE_YEAR) |>
        summarise(y = sum(FREQ_YEAR, na.rm = TRUE), .groups = "drop") |>
        rename(x = DATE_YEAR)
      title <- "Frecuencia de Accidentes por Año"
      xlab <- "Año"
    }

    p <- ggplot(df_agg, aes(x = x, y = y))

    if (input$graph_type == "bar") {
      p <- p + geom_bar(stat = "identity", fill = "#1F3B73", alpha = 0.8)
    } else if (input$graph_type == "line") {
      p <- p + geom_line(color = "#1F3B73", size = 1.2) +
        geom_point(color = "#FF4C4C", size = 2)
    } else {
      p <- p + geom_bar(stat = "identity", fill = "#1F3B73", alpha = 0.5) +
        geom_line(color = "#1F3B73", size = 1.2) +
        geom_point(color = "#FF4C4C", size = 2)
    }

    p + labs(title = title, x = xlab, y = "Frecuencia") +
      theme_nyc()
  })
  
  # Gráfico de distribución por barrio
  output$accident_borough <- renderPlot({
    df <- filtered_data_freq()
    if (is.null(df)) return()
    
    ggplot(df, aes(x = BOROUGH)) +
      geom_bar(fill = "#1F3B73") +
      geom_text(aes(label = after_stat(count)), stat = "count", vjust = -0.5, size = 3) +
      labs(title = "Distribución de Accidentes por Distrito",
           x = "Barrio", y = "Número de Accidentes") +
      theme_nyc()
  })
  
  output$accident_evolution <- renderPlotly({
    df <- filtered_data_freq()
    if (is.null(df)) return(NULL)
    
    df_agg <- df |>
      group_by(DATE = floor_date(DATE, "month"), BOROUGH) |>
      summarise(accidents = sum(FREQ_DAY, na.rm = TRUE), .groups = "drop")
    
    p <- plot_ly(df_agg,
                 x = ~DATE,
                 y = ~accidents,
                 color = ~BOROUGH,
                 colors = "Dark2",
                 type = 'scatter',
                 mode = 'lines+markers',
                 line = list(width = 2),
                 marker = list(size = 6)) |>
      layout(
        title = list(
          text = "<b>Evolución Temporal de Accidentes por Distrito</b>",
          font = list(color = "#1F3B73", size = 16, family = "Roboto Condensed"),
          x = 0.5
        ),
        xaxis = list(
          title = list(text = "<b>Fecha<b>", font = list(color = "#2E2E2E", family = "Roboto Condensed", size = 12)),
          tickfont = list(color = "#2E2E2E", family = "Roboto Condensed"),
          gridcolor = "#DADADA"
        ),
        yaxis = list(
          title = list(text = "<b>Número de Accidentes<b>", font = list(color = "#2E2E2E", family = "Roboto Condensed", size = 12)),
          tickfont = list(color = "#2E2E2E", family = "Roboto Condensed"),
          gridcolor = "#DADADA"
        ),
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = -0.2,
          title = list(text = "Distrito", font = list(color = "#2E2E2E", family = "Roboto Condensed")),
          font = list(color = "#2E2E2E", family = "Roboto Condensed"),
          bgcolor = "#F4F4F4"
        ),
        plot_bgcolor = "#F4F4F4",
        paper_bgcolor = "#F4F4F4"
      )
    
    p
  })

  output$summary_text <- renderText({
    df <- filtered_data_freq()
    if (is.null(df)) return("Seleccione al menos un distrito para ver los datos.")
    
    total <- sum(df$FREQ_DAY, na.rm = TRUE)
    paste("Número total de accidentes en el período seleccionado:", total)
  })
  
  # Grafico de heridos y muertos
  output$injury_death_plot <- renderPlot({
    df <- filtered_data_var()
    
    datos_resumen <- df |> 
      summarise(
        Total_Heridos = sum(NUM_PERSONS_INJURED, na.rm = TRUE),
        Heridos_Pedestrians = sum(NUM_PEDESTRIANS_INJURED, na.rm = TRUE),
        Heridos_Cyclists = sum(NUM_CYCLIST_INJURED, na.rm = TRUE),
        Heridos_Motorists = sum(NUM_MOTORIST_INJURED, na.rm = TRUE),
        Total_Muertos = sum(NUM_PERSONS_KILLED, na.rm = TRUE),
        Muertos_Pedestrians = sum(NUM_PEDESTRIANS_KILLED, na.rm = TRUE),
        Muertos_Cyclists = sum(NUM_CYCLIST_KILLED, na.rm = TRUE),
        Muertos_Motorists = sum(NUM_MOTORIST_KILLED, na.rm = TRUE)
      )
    
    datos_long <- datos_resumen |> 
      pivot_longer(cols = everything(),
                   names_to = c("Estado", "Tipo"),
                   names_sep = "_",
                   values_to = "Total")
    
    ggplot(datos_long, aes(x = Tipo, y = Total, fill = Tipo)) +
      geom_col(show.legend = TRUE) +
      geom_text(aes(label = Total), vjust = -0.5, size = 4, color = "#2E2E2E") +
      facet_wrap(~Estado, scales = "free_y") +
      labs(
        title = "Total de personas heridas y muertas por tipo",
        x = NULL,
        y = "Total",
        fill = "Tipo de persona"
      ) +
      theme_nyc() +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom"
      )
  })
  
  # Gráfico de frecuencia de causas
  output$frequency_of_causes <- renderPlot({
    df <- filtered_data_var()
    
    df <- df |> 
      count(CAUSE, name = "FREQUENCY") |>  
      arrange(desc(FREQUENCY)) |>
      head(input$top_causes)
    
    ggplot(df, aes(x = reorder(CAUSE, FREQUENCY), y = FREQUENCY)) + 
      geom_bar(stat = "identity", fill = "#1F3B73") + 
      coord_flip() +
      labs(title = paste("Frecuencia de las", input$top_causes, "causas más comunes"),
           x = "Causa", y = "Frecuencia") +
      theme_nyc() + 
      theme(axis.title = element_text(size = 10),
            title = element_text(size = 12),
            legend.title = element_blank())
  })
  
  # Mapa de accidentes
  output$num_accidentes <- renderUI({
    n <- nrow(filtered_data_map())
    
    div(style = "padding: 8px; background-color: #e0e0e0; border: 1px solid; border-radius: 4px; color: black;",
        HTML(paste0("<strong>Nº de accidentes:</strong> ", n))
    )
  })
  
  output$mapa_accidentes <- renderLeaflet({
    df <- filtered_data_map()
    
    df_fallecidos <- df |> 
      filter(NUM_PERSONS_KILLED > 0)
    
    leaflet(df) |> 
      addTiles() |> 
      addHeatmap(
        lng = ~LONGITUDE,
        lat = ~LATITUDE,
        radius = 12,
        blur = 15,
        max = 0.05
      ) |> 
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
      ) |> 
      addControl(
        html = HTML("<div style='background-color: rgba(255,255,255,0.8); padding: 6px; border-radius: 4px; font-size: 14px;'>
                  <b>†: Fallecidos</b>
                  </div>"),
        position = "bottomright"
      )
  })
  
  
  # Analisis jerarquico distritos
  dend_data <- reactive({
    data_borough_sum <- data_sampled |> 
      group_by(BOROUGH) |> 
      summarise(
        total_injured = sum(NUM_PERSONS_INJURED, na.rm = TRUE),
        total_killed = sum(NUM_PERSONS_KILLED, na.rm = TRUE)
      ) |> 
      na.omit()
    
    data_scaled <- data_borough_sum |> 
      dplyr::select(-BOROUGH) |> 
      scale()
    
    dist_matrix <- dist(data_scaled)
    hc <- hclust(dist_matrix, method = 'complete')
    
    list(
      data = data_borough_sum,
      hc = hc
    )
  })
  
  output$dendrogram_plot_borough <- renderPlot({
    data_list <- dend_data()
    hc <- data_list$hc
    data_borough_sum <- data_list$data
    
    dend <- as.dendrogram(hc)
    dend <- color_branches(dend, k = input$k_dis)
    dend <- set(dend, "labels", data_borough_sum$BOROUGH[hc$order])

    factoextra::fviz_dend(
      dend,
      k = input$k_dis,
      horiz = TRUE,
      rect = input$show_rect,
      rect_border = "#1F3B73",
      rect_fill = FALSE,
      main = "Dendrograma de los Distritos",
      cex = 0.7,
      color_labels_by_k = TRUE,
    ) + 
    theme_nyc()
  })
  
  
  output$borough_table <- renderDT({
    k <- input$k_dis
    data_list <- dend_data()
    hc <- data_list$hc
    data_borough_sum <- data_list$data
    
    clusters <- cutree(hc, k = k)

    result_table <- data_borough_sum |>
      mutate(Grupo = as.factor(clusters)) |>
      arrange(Grupo)
    
    datatable(
      result_table,
      options = list(
        dom = 't',
        pageLength = nrow(result_table)
      ),
      rownames = FALSE
    )
  })
  
  output$silhouette_plot_borough <- renderPlot({
    data_list <- dend_data()
    hc <- data_list$hc
    data_borough_sum <- data_list$data
    
    k <- input$k_dis
    clusters <- cutree(hc, k = k)

    dist_matrix <- dist(data_borough_sum[, c("total_injured", "total_killed")])

    fviz_silhouette(silhouette(clusters, dist_matrix)) + 
      ggtitle(paste("Índice de Silueta para k =", k)) +
      theme_nyc()
  })

  causes_cleaned <- data_sampled |>
    count(CAUSE, name = "FREQUENCY") |>
    arrange(desc(FREQUENCY))
  
  output$dendrogram_plot_causes <- renderPlot({
    dist_matrix <- stringdistmatrix(causes_cleaned$CAUSE, causes_cleaned$CAUSE, method = "jw")
    rownames(dist_matrix) <- causes_cleaned$CAUSE
    colnames(dist_matrix) <- causes_cleaned$CAUSE

    hc <- hclust(as.dist(dist_matrix), method = "ward.D2")

    fviz_dend(
      x = hc,
      k = input$k_cau,
      k_colors = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728",
                   "#9467bd", "#8c564b", "#e377c2", "#7f7f7f"),
      color_labels_by_k = TRUE,
      rect = TRUE,
      rect_fill = TRUE,
      cex = 0.5,
      main = "Dendrograma de las Causas",
      xlab = "Causas",
      ylab = "Distancia",
      sub = ""
    ) +
      theme_nyc()
  })
  
  output$silhouette_plot_causes <- renderPlot({
    dist_matrix <- stringdistmatrix(causes_cleaned$CAUSE, causes_cleaned$CAUSE, method = "jw")
    rownames(dist_matrix) <- causes_cleaned$CAUSE
    colnames(dist_matrix) <- causes_cleaned$CAUSE
    hc <- hclust(as.dist(dist_matrix), method = "ward.D2")
    
    k <- input$k_cau
    clusters <- cutree(hc, k = k)

    sil <- silhouette(clusters, dist(as.dist(dist_matrix)))

    fviz_silhouette(sil) +
      ggtitle(paste("Índice de Silueta para k =", k)) +
      theme_nyc()
  })
  
  
  output$causes_table <- renderDT({
    dist_matrix <- stringdistmatrix(causes_cleaned$CAUSE, causes_cleaned$CAUSE, method = "jw")
    rownames(dist_matrix) <- causes_cleaned$CAUSE
    colnames(dist_matrix) <- causes_cleaned$CAUSE
    hc <- hclust(as.dist(dist_matrix), method = "ward.D2")
    k <- input$k_cau
    clusters <- cutree(hc, k = k)

    causes_clustered <- data.frame(
      CAUSA = causes_cleaned$CAUSE,
      Grupo = as.factor(clusters)
    ) |> 
      arrange(Grupo)
    
    datatable(
      causes_clustered,
      options = list(
        dom = 't',
        pageLength = nrow(causes_clustered)
      ),
      rownames = FALSE
    )
  })
  
  
  # Analisis de correspondencia
  observeEvent(c(input$var1, input$var2), {
    req(input$var1 != input$var2)
  })
  
  ca_data <- reactive({
    req(input$var1, input$var2, input$var1 != input$var2)
    
    var1 <- input$var1
    var2 <- input$var2
    df <- data_sampled
    
    if (var1 %in% c("CAUSE", "VEHICLE_1")) {
      top10_var1 <- names(sort(table(df[[var1]]), decreasing = TRUE))[1:10]
      df <- df[df[[var1]] %in% top10_var1, ]
    }
    if (var2 %in% c("CAUSE", "VEHICLE_1")) {
      top10_var2 <- names(sort(table(df[[var2]]), decreasing = TRUE))[1:10]
      df <- df[df[[var2]] %in% top10_var2, ]
    }
    
    df <- df %>%
      filter(!is.na(.data[[var1]]), !is.na(.data[[var2]]))
    
    tab <- table(df[[var1]], df[[var2]])
    
    tab <- tab[rowSums(tab) > 0, colSums(tab) > 0]
    
    if (nrow(tab) < 2 || ncol(tab) < 2) return(NULL)
    
    tab
  })
  
  ca_result <- reactive({
    tab <- ca_data()
    if (is.null(tab)) return(NULL)
    if (nrow(tab) < 2 || ncol(tab) < 2) return(NULL)
    
    FactoMineR::CA(tab, graph = FALSE)
  })
  
  output$ca_biplot <- plotly::renderPlotly({
    req(ca_result())
    
    ca_obj <- ca_result()
    row_coord <- as.data.frame(ca_obj$row$coord)
    col_coord <- as.data.frame(ca_obj$col$coord)
    
    row_coord$label <- rownames(row_coord)
    col_coord$label <- rownames(col_coord)
    
    row_coord$type <- "Fila"
    col_coord$type <- "Columna"
    
    biplot_data <- rbind(
      data.frame(Dim1 = row_coord[,1], Dim2 = row_coord[,2], label = row_coord$label, type = row_coord$type),
      data.frame(Dim1 = col_coord[,1], Dim2 = col_coord[,2], label = col_coord$label, type = col_coord$type)
    )
    
    biplot_data$text_color <- ifelse(grepl("^\\d+$", biplot_data$label), "black",
                                     ifelse(biplot_data$type == "Fila", "#1F3B73", "#FF4C4C"))
    
    plot_ly(
      data = biplot_data,
      x = ~Dim1, y = ~Dim2,
      type = 'scatter',
      mode = 'markers+text',
      text = ~label,
      textposition = 'bottom center',
      marker = list(size = 7),
      color = ~type,
      colors = c("Fila" = "#1F3B73", "Columna" = "#FF4C4C"),
      textfont = list(size = 12)
    ) %>%
      layout(
        title = list(
          text = "<b>Biplot del Análisis de Correspondencias<b>",
          x = 0.5,
          font = list(size = 16, color = "#1F3B73", family = "Roboto Condensed")
        ),
        xaxis = list(
          title = list(text = "<b>Dim 1<b>", font = list(size = 12, color = "#2E2E2E", family = "Roboto Condensed")),
          tickfont = list(color = "#2E2E2E", family = "Roboto Condensed"),
          gridcolor = "#DADADA"
        ),
        yaxis = list(
          title = list(text = "<b>Dim 2<b>", font = list(size = 12, color = "#2E2E2E", family = "Roboto Condensed")),
          tickfont = list(color = "#2E2E2E", family = "Roboto Condensed"),
          gridcolor = "#DADADA"
        ),
        plot_bgcolor = "#F4F4F4",
        paper_bgcolor = "#F4F4F4",
        legend = list(
          bgcolor = "#F4F4F4",
          bordercolor = "#F4F4F4",
          font = list(color = "#2E2E2E", family = "Roboto Condensed")
        )
      )
  })
  
  output$ca_contrib <- renderPlot({
    ca_res <- ca_result()
    req(!is.null(ca_res))
    
    eig_vals <- ca_res$eig
    if (is.null(eig_vals) || nrow(eig_vals) == 0) return()
    
    df <- data.frame(
      Dimension = factor(paste0("Dim", seq_along(eig_vals[, 2])), levels = paste0("Dim", seq_along(eig_vals[, 2]))),
      Porcentaje = eig_vals[, 2]
    )
    
    ggplot(df, aes(x = Dimension, y = Porcentaje)) +
      geom_bar(stat = "identity", fill = "#1F3B73") +
      labs(title = "Porcentaje de varianza explicada", y = "Porcentaje", x = NULL) +
      theme_nyc()
  })
  
  
  output$ca_interpretation <- renderText({
    ca_res <- ca_result()
    req(!is.null(ca_res))
    
    eig_vals <- ca_res$eig
    if (is.null(eig_vals) || nrow(eig_vals) < 2) {
      return("No se puede interpretar porque los datos no generan dimensiones suficientes.")
    }
    
    var1 <- input$var1
    var2 <- input$var2
    
    dim1 <- round(eig_vals[1, 2], 2)
    dim2 <- round(eig_vals[2, 2], 2)
    
    paste0("El análisis de correspondencias entre ", var1, " y ", var2,
           " revela que las dos primeras dimensiones explican aproximadamente el ",
           dim1 + dim2, "% de la varianza total. Esto sugiere una relación estructurada entre ambas variables.")
  })
  
  variables <- c("CAUSE", "HOUR", "DAY_OF_WEEK", "VEHICLE_1")
  
  observeEvent(input$var1, {
    session$sendCustomMessage("disable_radio_option", list(
      inputId = "var2",
      option = input$var1
    ))
  })
  
  observeEvent(input$var2, {
    session$sendCustomMessage("disable_radio_option", list(
      inputId = "var1",
      option = input$var2
    ))
  })
}

#EJECUTAR
shinyApp(ui = ui, server = server)