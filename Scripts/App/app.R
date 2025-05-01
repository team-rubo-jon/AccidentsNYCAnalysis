library(shiny)
library(shinythemes)
library(bslib)
library(tidyverse)
library(lubridate)
library(zoo)

data_sampled <- read.csv("data_sampled.csv")

# Supón que ya tienes cargado 'data_sampled'
data_time <- data_sampled |> 
  mutate(DATE = as.Date(DATE, format = "%Y-%m-%d")) |> 
  mutate(DATE_MONTH = yearmonth(DATE),
         DATE_YEAR = year(DATE)) |> 
  add_count(DATE, name = "FREQ_DAY") |> 
  add_count(DATE_MONTH, name = "FREQ_MONTH") |> 
  add_count(DATE_YEAR, name = "FREQ_YEAR") |>
  dplyr::select(DATE, DATE_MONTH, DATE_YEAR, FREQ_DAY, FREQ_MONTH, FREQ_YEAR) |> 
  arrange(DATE)

# Obtener fechas mínimas y máximas para el rango
min_date <- min(data_time$DATE)
max_date <- max(data_time$DATE)


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

# Tema personalizado para todos los gráficos al estilo NY
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
  
  tabPanel(
    titlePanel("Frecuencia de Accidentes 🚦"),
    sidebarLayout(
      sidebarPanel(
        h3("Configuración de visualización"),
        
        # Widget para seleccionar el barrio
        selectInput("borough", "Seleccionar Barrio:",
                    choices = c("Todos", unique(data_sampled$BOROUGH)),
                    selected = "Todos"),
        
        # Widget para tipo de agregación temporal
        selectInput("freq_type", "Seleccionar frecuencia temporal:",
                    choices = c("Diario" = "daily", "Mensual" = "monthly", "Anual" = "yearly"),
                    selected = "daily"),
        
        # Rango de fechas
        dateRangeInput(
          'date_range', 'Filtrar por rango de fechas',
          start = min_date, end = max_date,
          min = min_date, max = max_date,
          format = 'yyyy-mm-dd', startview = 'year',
          language = 'es', separator = " a "
        ),
        
        # Slider para seleccionar el número de bins
        sliderInput("num_bins", "Número de Bins:", 
                    min = 5, max = 100, value = 30, step = 5)
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel('Accident Plot', plotOutput(outputId = 'accident_plot'), textOutput("summary_text")),
          tabPanel('Accident Bar', plotOutput(outputId = 'accident_bar')),
          tabPanel('Accident by Borough', plotOutput(outputId = 'accident_borough')),
          tabPanel('Bar Chart Stacked by neighborhood', plotOutput(outputId = 'accident_evolution'))
        )
      )
    )
  )
)


# SERVER
server <- function(input, output) {
  
  filtered_data <- reactive({
    req(input$date_range)
    df <- data_sampled |> 
      filter(DATE >= input$date_range[1], DATE <= input$date_range[2])
    
    # Filtrar por barrio si no es "Todos"
    if (input$borough != "Todos") {
      df <- df |> 
        filter(BOROUGH == input$borough)
    }
    
    return(df)
  })
  
  filtered_data_time <- reactive({
    req(input$date_range)
    data_time |> 
      filter(DATE >= input$date_range[1],
             DATE <= input$date_range[2])
  })
  
  output$accident_plot <- renderPlot({
    df <- filtered_data_time()
    
    if (input$freq_type == "daily") {
      ggplot(df, aes(x = DATE, y = FREQ_DAY)) +
        geom_line(color = "#1F3B73", size = 1.2) +
        geom_point(color = "#FF4C4C", size = 2) +
        labs(title = "Frecuencia de Accidentes por Día",
             x = "Fecha", y = "Frecuencia") +
        theme_nyc()
      
    } else if (input$freq_type == "monthly") {
      df_month <- df |> distinct(DATE_MONTH, FREQ_MONTH)
      ggplot(df_month, aes(x = DATE_MONTH, y = FREQ_MONTH)) +
        geom_line(color = "#1F3B73", size = 1.2) +
        geom_point(color = "#FF4C4C", size = 2) +
        geom_text(aes(label = FREQ_MONTH), vjust = -0.5, size = 3, color = "#2E2E2E") +
        labs(title = "Frecuencia de Accidentes por Mes",
             x = "Mes", y = "Frecuencia") +
        theme_nyc()
      
    } else if (input$freq_type == "yearly") {
      df_year <- df |> distinct(DATE_YEAR, FREQ_YEAR)
      ggplot(df_year, aes(x = DATE_YEAR, y = FREQ_YEAR)) +
        geom_line(color = "#1F3B73", size = 1.2) +
        geom_point(color = "#FF4C4C", size = 2) + 
        geom_text(aes(label = FREQ_YEAR), vjust = -0.5, size = 3, color = "#2E2E2E") +
        labs(title = "Frecuencia de Accidentes por Año",
             x = "Año", y = "Frecuencia") +
        theme_nyc()
    }
  })
  
  # Gráfico de barras (Mensual o Anual) con ajuste dinámico de bins
  output$accident_bar <- renderPlot({
    df <- filtered_data_time()
    
    if (input$freq_type == "daily") {
      ggplot(df, aes(x = DATE, y = FREQ_DAY)) +
        geom_bar(stat = "identity", fill = "#1F3B73") + 
        labs(title = "Frecuencia de Accidentes por Día",
             x = "Día", y = "Frecuencia") +
        theme_nyc()
      
    } else if (input$freq_type == "monthly") {
      df_month <- df |> 
        group_by(DATE_MONTH) |> 
        summarise(FREQ_MONTH = sum(FREQ_MONTH)) |> 
        mutate(MONTH = month(DATE_MONTH, label = TRUE))
      
      ggplot(df_month, aes(x = MONTH, y = FREQ_MONTH)) + 
        geom_bar(stat = "identity", fill = "#1F3B73") + 
        geom_text(aes(label = FREQ_MONTH), vjust = -0.5, size = 3, color = "#2E2E2E") +
        labs(title = "Frecuencia de Accidentes por Mes",
             x = "Mes", y = "Frecuencia") +
        theme_nyc()
      
    } else if (input$freq_type == "yearly") {
      df_year <- df |> 
        group_by(DATE_YEAR) |> 
        summarise(FREQ_YEAR = sum(FREQ_YEAR))
      
      ggplot(df_year, aes(x = DATE_YEAR, y = FREQ_YEAR)) + 
        geom_bar(stat = "identity", fill = "#1F3B73") + 
        geom_text(aes(label = FREQ_YEAR), vjust = -0.5, size = 3, color = "#2E2E2E") +
        labs(title = "Frecuencia de Accidentes por Año",
             x = "Año", y = "Frecuencia") +
        theme_nyc()
    }
  })
  
  # Gráfico de distribución de accidentes por barrio
  output$accident_borough <- renderPlot({
    df <- filtered_data()
    
    ggplot(df, aes(x = BOROUGH)) +
      geom_histogram(stat = "count", fill = "#1F3B73") +
      geom_text(aes(label = ..count..), stat = "count", vjust = -0.5, size = 3, color = "#2E2E2E") +
      labs(title = "Distribución de Accidentes por Barrio",
           x = "Barrio", y = "Número de Accidentes") +
      theme_nyc()
  })
  
  # Gráfico de evolución de accidentes por barrio a lo largo del tiempo
  output$accident_evolution <- renderPlot({
    df <- filtered_data()
    
    df_borough_time <- df |> 
      group_by(DATE, BOROUGH) |> 
      summarise(accidents = n()) |> 
      mutate(DATE = year(DATE))
    
    ggplot(df_borough_time, aes(x = DATE, y = accidents, fill = BOROUGH)) +
      geom_bar(stat = "identity") +
      labs(title = "Accidentes por Barrio a lo Largo del Tiempo",
           x = "Fecha", y = "Número de Accidentes") +
      theme_nyc() +
      scale_fill_viridis_d()
  })
  
  output$summary_text <- renderText({
    df <- filtered_data_time()
    total <- sum(df$FREQ_DAY, na.rm = TRUE)
    paste("Número total de accidentes en el período seleccionado:", total)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
