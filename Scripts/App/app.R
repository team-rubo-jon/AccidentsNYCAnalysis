library(shiny)
library(shinythemes)
library(bslib)
library(tidyverse)
library(lubridate)
library(zoo)

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
          wellPanel(
            h4("Configuración del Gráfico Temporal"),
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

      ),
      mainPanel(

      )
    )
  ),
  
  # Panel para los analisis
  tabPanel(
    tags$div("Análisis de interés 💡️", style = "font-size: 18px; font-weight: bold;"),
    sidebarLayout(
      sidebarPanel(
        # Selector desplegable
        selectInput(
          "main_view",
          "Seleccionar vista:",
          choices = c(
            "PCA 🔍" = "pca",
            "Análisis Cluster 💠️"= "cluster"
          ),
          selected = "pca"
        ),
        
        # Panel condicional para el pca
        conditionalPanel(
          condition = "input.main_view == 'pca'"

        ),
        
        # Panel condicional para el cluster
        conditionalPanel(
          condition = "input.main_view == 'cluster'"
          
        ),
      ),
      
      mainPanel(
        conditionalPanel(
          condition = "input.main_view == 'pca'",
          tabsetPanel(
            tabPanel('PCA', plotOutput(outputId = 'pca'))
          )
        ),
        
        conditionalPanel(
          condition = "input.main_view == 'cluster'",
          tabsetPanel(
            tabPanel('Cluster', plotOutput(outputId = 'analisis_cluster'))
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
  
}

# Run the application
shinyApp(ui = ui, server = server)