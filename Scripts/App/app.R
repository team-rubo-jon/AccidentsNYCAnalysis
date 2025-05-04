library(shiny)
library(shinythemes)
library(bslib)
library(tidyverse)
library(lubridate)
library(zoo)
library(leaflet)
library(leaflet.extras)
library(dplyr)

data_sampled <- read.csv("data_sampled.csv")

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

# Obtener fechas mínimas y máximas para el rango
min_date <- min(data_time$DATE)
max_date <- max(data_time$DATE)

# Obtener lista única de barrios
borough_choices <- unique(na.omit(data_sampled$BOROUGH))

# para el mapa evolutivo
subconjunto_size <- 500
total_subconjuntos <- ceiling(nrow(data_sampled) / subconjunto_size)

# Variables HOUR y DAYS_OF_WEEK para el análisis de correspondencia:
data_sampled <- data_sampled |> 
  mutate(HOUR = as.numeric(format(strptime(TIME, format = "%H:%M"), "%H")),
         DAY_OF_WEEK = weekdays(as.Date(DATE, format = "%Y-%m-%d")))
data_sampled$HOUR <- as.factor(data_sampled$HOUR)
data_sampled$DAY_OF_WEEK <- toupper(data_sampled$DAY_OF_WEEK)
data_sampled$DAY_OF_WEEK <- factor(trimws(data_sampled$DAY_OF_WEEK))  # Elimina espacios

# Crear tema personalizado
ny_theme <- bs_theme(
  version = 5,
  primary = "#FFD100",       # Amarillo taxi
  secondary = "#2E2E2E",     # Gris asfalto
  success = "#1F3B73",       # Azul NYPD
  danger = "#FF4C4C",        # Rojo alerta
  bg = "#F4F4F4",            # Fondo claro
  fg = "#2E2E2E",            # Texto
  base_font = font_google("Roboto Condensed"),
  heading_font = font_google("Bebas Neue"),
  code_font = font_google("Fira Code")
)

# Tema personalizado para gráficos
theme_nyc <- function() {
  theme_minimal(base_family = "Roboto Condensed") +
    theme(
      plot.background = element_rect(fill = "#F4F4F4", color = NA),
      panel.background = element_rect(fill = "#F4F4F4", color = NA),
      panel.grid.major = element_line(color = "#DADADA"),
      panel.grid.minor = element_blank(),
      axis.title = element_text(color = "#2E2E2E", face = "bold"),
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

# UI
ui <- navbarPage(
  title = div(
    icon("car-crash"),
    tags$h1("NYC Accident Dashboard", class = "text-secondary", style = "margin: 0; font-family: 'Bebas Neue';")
  ),
  theme = ny_theme,
  
  # Usar un desplegable para el "Menú Principal"
  tabPanel(
    tags$div("Visualización de datos 📊", style = "font-size: 18px; font-weight: bold;"),
    sidebarLayout(
      sidebarPanel(
        # Selector desplegable en lugar de radioButtons
        selectInput(
          "main_view",
          "Seleccionar vista:",
          choices = c(
            "Frecuencia de Accidentes 🚦" = "freq",
            "Frecuencia de Accidentes por barrio 🏙️"= "freq_bar",
            "Heridos vs Muertos 💀 " = "var", 
            "Frecuencia de Causas 🚑" = "causes"
          ),
          selected = "freq"
        ),
        
        # Panel condicional para frecuencia de accidentes
        conditionalPanel(
          condition = "input.main_view == 'freq'",
          h4("Configuración de Frecuencia Temporal"),
          selectInput(
            "freq_type", 
            "Frecuencia temporal:",
            choices = c("Diario" = "daily", "Mensual" = "monthly", "Anual" = "yearly"),
            selected = "daily"
          ),
          radioButtons(
            "graph_type",
            "Tipo de gráfico:",
            choices = c("Barras" = "bar", "Líneas" = "line", "Combinado" = "combo"),
            selected = "combo"
          ),
          dateRangeInput(
            'date_range_freq', 
            'Rango de fechas:',
            start = min_date, 
            end = max_date,
            min = min_date, 
            max = max_date,
            format = 'yyyy-mm-dd', 
            startview = 'year',
            language = 'es', 
            separator = " a "
          )
        ),
        
        # Panel condicional para frecuencia de barrios
        conditionalPanel(
          condition = "input.main_view == 'freq_bar'",
          h4("Configuración de Gráficos por Barrio"),
          
          checkboxGroupInput(
            "borough_freq", 
            "Seleccionar Barrio(s):",
            choices = borough_choices,
            selected = borough_choices
          ),
          
          actionButton("select_all_freq", "Seleccionar Todos"),
          actionButton("deselect_all_freq", "Deseleccionar Todos"),
          
          dateRangeInput(
            'date_range_freq', 
            'Rango de fechas:',
            start = min_date, 
            end = max_date,
            min = min_date, 
            max = max_date,
            format = 'yyyy-mm-dd', 
            startview = 'year',
            language = 'es', 
            separator = " a "
          )
        ),
        
        # Panel condicional para heridos y muertos
        conditionalPanel(
          condition = "input.main_view == 'var'",
          h4("Configuración de Variables"),
          selectInput("borough", "Seleccionar Barrio:",
                      choices = c("Todos", unique(data_sampled$BOROUGH)),
                      selected = "Todos"),
          dateRangeInput(
            'date_range_var', 
            'Filtrar por rango de fechas',
            start = min_date, 
            end = max_date,
            min = min_date, 
            max = max_date,
            format = 'yyyy-mm-dd', 
            startview = 'year',
            language = 'es', 
            separator = " a "
          )
        ),
        
        # Panel para seleccionar el número de causas a mostrar
        conditionalPanel(
          condition = "input.main_view == 'causes'",
          h4("Configuración de Variables"),
          selectInput("borough", "Seleccionar Barrio:",
                      choices = c("Todos", unique(data_sampled$BOROUGH)),
                      selected = "Todos"),
          dateRangeInput(
            'date_range_var', 
            'Filtrar por rango de fechas',
            start = min_date, 
            end = max_date,
            min = min_date, 
            max = max_date,
            format = 'yyyy-mm-dd', 
            startview = 'year',
            language = 'es', 
            separator = " a "
          ),
          
          # Slider para elegir el número de causas a mostrar
          sliderInput(
            "top_causes", 
            "Número de causas más frecuentes:",
            min = 5, 
            max = length(unique(data_sampled$CAUSE)), 
            value = 10,
            step = 1
          )
        )
      ),
      
      mainPanel(
        conditionalPanel(
          condition = "input.main_view == 'freq'",
          tabsetPanel(
            tabPanel('Gráfico Temporal Combinado', 
                     plotOutput(outputId = 'combined_plot'), 
                     textOutput("summary_text")),
          )
        ),
        
        conditionalPanel(
          condition = "input.main_view == 'freq_bar'",
          tabsetPanel(
            tabPanel('Distribución por Barrio', plotOutput(outputId = 'accident_borough')),
            tabPanel('Evolución por Barrio', plotOutput(outputId = 'accident_evolution'))
          )
        ),
        
        conditionalPanel(
          condition = "input.main_view == 'var'",
          tabsetPanel(
            tabPanel('Heridos y Fallecidos', plotOutput(outputId = 'injury_death_plot')),
          )
        ),
        
        conditionalPanel(
          condition = "input.main_view == 'causes'",
          tabsetPanel(
            tabPanel('Causas de Accidentes', plotOutput(outputId = 'frequency_of_causes'))
          )
        )
      )
    )
  ),
  
  # Panel para los mapas
  tabPanel(
    tags$div("Mapas 🗺️", style = "font-size: 18px; font-weight: bold;"),
    sidebarLayout(
      sidebarPanel(
        # Selector desplegable en lugar de radioButtons
        selectInput(
          "main_view",
          "Seleccionar vista:",
          choices = c(
            "Mapa de calor 🌡️" = "hot_map",
            "️Mapa evolutivo 📈"= "evolution_map"
          ),
          selected = "hot_map"
        ),
        
        # Panel condicional para el mapa de calor
        conditionalPanel(
          condition = "input.main_view == 'hot_map'",
          h4("Configuración del mapa"),
          
          checkboxGroupInput("tipo_usuario", "Víctimas implicadas:",
                             choices = c("Sin heridos", "Peatones", "Ciclistas", "Motoristas"),
                             selected = c("Sin heridos","Peatones", "Ciclistas", "Motoristas")),
          
          selectInput("causa", "Causa del accidente:",
                      choices = c("Todas", unique(data_sampled$CAUSE)),
                      selected = "Todas"),
          br(),
          uiOutput("num_accidentes")
        ),
        
        # Panel condicional para el mapa evolutivo
        conditionalPanel(
          condition = "input.main_view == 'evolution_map'",
          h4("Botones de control"),
          
          actionButton("start", "Iniciar", class = "btn-success"),
          actionButton("pause", "Pausar", class = "btn-warning"),
          br(), br()
        )
      ),
    
      mainPanel(
        conditionalPanel(
          condition = "input.main_view == 'hot_map'",
            leafletOutput("mapa_accidentes", height=500),
            style = "margin-bottom: 10px;"
        ),
        
        conditionalPanel(
          condition = "input.main_view == 'evolution_map'",
            leafletOutput("mapa_evolutivo", height = 500),
            uiOutput("rango_fechas")
        )
      ),
    ),
    
    tags$div(
      style = "position: fixed; bottom: 0; left: 0; right: 0; background-color: #F4F4F4; padding: 10px; z-index: 9999; 
           margin-bottom: 10px; margin-left: 15px; margin-right: 15px; border-radius: 5px;",
      tags$style(HTML("
        #Fecha-label { color: #FFD100; font-weight: bold; }
        .irs-grid-text { color: #FFD100 !important; }
      ")),
      sliderInput("Fecha", "RANGO DE FECHAS:",
                  min = as.Date(min(data_sampled$DATE)),
                  max = as.Date(max(data_sampled$DATE)),
                  value = c(as.Date(min(data_sampled$DATE)), as.Date(max(data_sampled$DATE))),
                  timeFormat = "%Y-%m-%d",
                  width = "100%")
    )
  ),
  
  # Panel para los análisis
  tabPanel(
    tags$div("Análisis de interés 💡️", style = "font-size: 18px; font-weight: bold;"),
    
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "main_view",
          "Seleccionar vista:",
          choices = c(
            "Análisis Cluster Jerárquico 💠️" = "cluster",
            "Análisis de Correspondencia 👥" = "corresp"
          ),
          selected = "cluster"
        ),
        
        # Widgets Análisis Jerárquico
        conditionalPanel(
          condition = "input.main_view == 'cluster'",
          checkboxGroupInput("plot_choices", 
                             "Selecciona las opciones para ver:",
                             choices = c("Dendrograma", "Silueta", "Tabla"),
                             selected = c("Dendrograma", "Silueta", "Tabla")),
          sliderInput("k", 
                      "Número de grupos (k):", 
                      min = 2, max = 4, value = 3),
          checkboxInput("show_rect", "Mostrar rectángulos en el dendrograma", value = TRUE)
        ),
        
        # Widgets Análisis de Correspondencia
        conditionalPanel(
          condition = "input.main_view == 'corresp'",
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
        # Panel principal Análisis Jerárquico
        conditionalPanel(
          condition = "input.main_view == 'cluster'",
          conditionalPanel(
            condition = "input.plot_choices.indexOf('Dendrograma') > -1",
            plotOutput("dendrogramPlot", height = "500px")
          ),
          conditionalPanel(
            condition = "input.plot_choices.indexOf('Silueta') > -1",
            plotOutput("silhouettePlot")
          ),
          conditionalPanel(
            condition = "input.plot_choices.indexOf('Tabla') > -1",
            DTOutput("boroughTable")
          )
        ),
        
        # Panel principal Análisis de Correspondencia
        conditionalPanel(
          condition = "input.main_view == 'corresp'",
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
            column(6, plotOutput("ca_biplot", height = "500px")),
            column(6, plotOutput("ca_contrib", height = "500px"))
          )
        )
      )
    )
  )
)

  
# SERVER
server <- function(input, output, session) {
  
  # Observadores para los botones de selección/deselección (frecuencia)
  observeEvent(input$select_all_freq, {
    updateCheckboxGroupInput(session, "borough_freq", selected = borough_choices)
  })
  
  observeEvent(input$deselect_all_freq, {
    updateCheckboxGroupInput(session, "borough_freq", selected = character(0))
  })
  
  # Datos filtrados para la pestaña de frecuencia
  filtered_data_freq <- reactive({
    req(input$date_range_freq)
    
    df <- data_time |>
      filter(DATE >= input$date_range_freq[1], 
             DATE <= input$date_range_freq[2])
    
    # Filtrar por barrios seleccionados (si hay alguno seleccionado)
    if (length(input$borough_freq) > 0) {
      df <- df |> 
        filter(BOROUGH %in% input$borough_freq)
    } else {
      return(NULL)  # No mostrar datos si no hay barrios seleccionados
    }
    
    return(df)
  })
  
  # Datos filtrados para la pestaña de variables
  filtered_data_var <- reactive({
    req(input$date_range_var)
    df <- data_sampled |> 
      filter(DATE >= input$date_range_var[1], DATE <= input$date_range_var[2])
    
    # Filtrar por barrio si no es "Todos"
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
    
    # Filtro por fechas
    df <- df |> 
      filter(DATE >= input$Fecha[1], DATE <= input$Fecha[2])
    
    # Filtro por causa del accidente
    if (input$causa != "Todas") {
      df <- df |> filter(CAUSE == input$causa)
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
  
  # Para el mapa evolutivo
  current_index <- reactiveVal(1)
  
  get_subset <- function(index) {
    start_row <- (index - 1) * subconjunto_size + 1
    end_row <- min(index * subconjunto_size, nrow(data_sampled))
    data_sampled[start_row:end_row, ]
  }
  
  
  # Gráfico combinado (barras + líneas)
  output$combined_plot <- renderPlot({
    df <- filtered_data_freq()
    if (is.null(df)) return()  # Salir si no hay datos
    
    # Agregar por frecuencia temporal seleccionada
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
    
    # Crear gráfico base
    p <- ggplot(df_agg, aes(x = x, y = y))
    
    # Añadir elementos según el tipo de gráfico seleccionado
    if (input$graph_type == "bar") {
      p <- p + geom_bar(stat = "identity", fill = "#1F3B73", alpha = 0.8)
    } else if (input$graph_type == "line") {
      p <- p + geom_line(color = "#1F3B73", size = 1.2) +
        geom_point(color = "#FF4C4C", size = 2)
    } else { # Combinado
      p <- p + geom_bar(stat = "identity", fill = "#1F3B73", alpha = 0.5) +
        geom_line(color = "#1F3B73", size = 1.2) +
        geom_point(color = "#FF4C4C", size = 2)
    }
    
    # Añadir etiquetas y tema
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
      labs(title = "Distribución de Accidentes por Barrio",
           x = "Barrio", y = "Número de Accidentes") +
      theme_nyc()
  })
  
  # Gráfico de evolución por barrio
  output$accident_evolution <- renderPlot({
    df <- filtered_data_freq()
    if (is.null(df)) return()
    
    df_agg <- df |>
      group_by(DATE = floor_date(DATE, "month"), BOROUGH) |>
      summarise(accidents = sum(FREQ_DAY, na.rm = TRUE), .groups = "drop")
    
    ggplot(df_agg, aes(x = DATE, y = accidents, color = BOROUGH)) +
      geom_line(size = 1.1) +
      geom_point(size = 1.5) +
      labs(title = "Evolución Temporal de Accidentes por Barrio",
           x = "Fecha", y = "Número de Accidentes", color = "Barrio") +
      theme_nyc() +
      scale_color_brewer(palette = "Set1")
  })

  
  # Texto resumen
  output$summary_text <- renderText({
    df <- filtered_data_freq()
    if (is.null(df)) return("Seleccione al menos un barrio para ver los datos.")
    
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
      head(input$top_causes)  # Filtrar por las causas más frecuentes
    
    ggplot(df, aes(x = reorder(CAUSE, FREQUENCY), y = FREQUENCY)) + 
      geom_bar(stat = "identity", fill = "#1F3B73") + 
      coord_flip() +  # Rota el gráfico para mejor visualización
      labs(title = paste("Frecuencia de las", input$top_causes, "sausas más comunes"),
           x = "Causa", y = "Frecuencia") +
      theme_nyc() + 
      theme(axis.title = element_text(size = 10),
            title = element_text(size = 12),
            legend.title = element_blank())
  })
  
  # Numero de accidentes
  output$num_accidentes <- renderUI({
    n <- nrow(filtered_data_map())
    
    div(style = "padding: 8px; background-color: #3c3c3c; border: 1px solid #555; border-radius: 4px; color: white;",
        HTML(paste0("<strong>Nº de accidentes:</strong> ", n))
    )
  })
  
  # Mapa de accidentes
  output$num_accidentes <- renderUI({
    n <- nrow(filtered_data_map())
    
    div(style = "padding: 8px; background-color: #3c3c3c; border: 1px solid #555; border-radius: 4px; color: white;",
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
  
  # Mapa evolutivo
  
  # Renderizar el primer subconjunto al iniciar la app
  output$mapa_evolutivo <- renderLeaflet({
    subset_data <- get_subset(current_index())
    
    leaflet(subset_data) |> 
      addTiles() |> 
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
    subset_data <- get_subset(current_index())
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
  
  # Al presionar el botón, avanzar al siguiente subconjunto
  observeEvent(input$start, {
    new_index <- if (current_index() < total_subconjuntos) {
      current_index() + 1
    } else {
      1
    }
    current_index(new_index)
  })
  
  
  # ELEMENTOS PARA EL ANÁLISIS JERÁRQUICO:
  # Reactivo para almacenar los datos del dendrograma
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
  
  output$dendrogramPlot <- renderPlot({
    data_list <- dend_data()
    hc <- data_list$hc
    data_borough_sum <- data_list$data
    
    # Crear y configurar dendrograma
    dend <- as.dendrogram(hc)
    dend <- color_branches(dend, k = input$k)
    dend <- set(dend, "hang", -1)
    dend <- set(dend, "labels", data_borough_sum$BOROUGH[hc$order])  # Etiquetas ordenadas
    
    # Ajustar márgenes para que las etiquetas no se corten
    op <- par(no.readonly = TRUE)  
    par(mar = c(8, 4, 4, 2))       
    
    # Graficar dendrograma
    plot(dend, main = "Dendrograma de distritos", horiz = FALSE, axes = FALSE)
    
    # Dibujar rectángulos de clúster si se selecciona
    if (input$show_rect) rect.dendrogram(dend, k = input$k, border = "blue")
    
    par(op)  # Restaurar márgenes originales
  })
  
  
  
  
  output$boroughTable <- renderDT({
    k <- input$k
    data_list <- dend_data()
    hc <- data_list$hc
    data_borough_sum <- data_list$data
    
    # Asignar clusters
    clusters <- cutree(hc, k = k)
    
    # Crear tabla con la columna Grupo
    result_table <- data_borough_sum |>
      mutate(Grupo = as.factor(clusters)) |>  # Convertir a factor para el coloreado
      arrange(Grupo)
    
    datatable(
      result_table,
      options = list(
        dom = 't',  # Elimina controles de búsqueda y paginación
        pageLength = nrow(result_table)  # Muestra todas las filas
      ),
      rownames = FALSE
    )
  })
  
  output$silhouettePlot <- renderPlot({
    data_list <- dend_data()
    hc <- data_list$hc
    data_borough_sum <- data_list$data
    
    # Obtener el número de clústeres (k) desde la entrada
    k <- input$k
    
    # Asignar los clústeres usando cutree
    clusters <- cutree(hc, k = k)
    
    # Calcular la silueta usando la distancia
    dist_matrix <- dist(data_borough_sum[, c("total_injured", "total_killed")])
    
    # Visualizar el índice de silueta con fviz_silhouette
    fviz_silhouette(silhouette(clusters, dist_matrix)) + 
      ggtitle(paste("Índice de Silueta para k =", k)) +
      theme_nyc()  # Usar el tema 'ny_theme' también en los gráficos
  })
  
  # ELEMENTOS PARA EL ANÁLISIS DE CORRESPONDENCIA
  
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
    
    # Eliminar filas/columnas vacías
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
  
  output$ca_biplot <- renderPlot({
    req(ca_result())
    plot(ca_result(), main = "Biplot del Análisis de Correspondencias", col.row = "blue", col.col = "red")
  })
  
  
  output$ca_contrib <- renderPlot({
    ca_res <- ca_result()
    req(!is.null(ca_res))
    
    eig_vals <- ca_res$eig
    if (is.null(eig_vals) || nrow(eig_vals) == 0) return()
    
    barplot(eig_vals[, 2],
            names.arg = paste0("Dim", seq_along(eig_vals[, 2])),
            main = "Porcentaje de varianza explicada",
            ylab = "Porcentaje",
            col = "skyblue")
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
  
  
  # Botones análisis de correspondencia:
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

# Run the application
shinyApp(ui = ui, server = server)