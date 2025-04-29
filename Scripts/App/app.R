library(shiny)
library(shinythemes)
library(tidyverse)
library(lubridate)
library(zoo)

# Supón que ya tienes cargado 'data_sampled'
data_time <- data_sampled |> 
  mutate(DATE_DAY = as.Date(DATE, format = "%m/%d/%Y")) |>  
  mutate(DATE_MONTH = yearmonth(DATE_DAY),
         DATE_YEAR = year(DATE_DAY)) |> 
  add_count(DATE_DAY, name = "FREQ_DAY") |> 
  add_count(DATE_MONTH, name = "FREQ_MONTH") |> 
  add_count(DATE_YEAR, name = "FREQ_YEAR") |>
  dplyr::select(DATE_DAY, DATE_MONTH, DATE_YEAR, FREQ_DAY, FREQ_MONTH, FREQ_YEAR) |> 
  arrange(DATE_DAY)

# Obtener fechas mínimas y máximas para el rango
min_date <- min(data_time$DATE_DAY)
max_date <- max(data_time$DATE_DAY)

# UI
ui <- navbarPage(
  p(strong('Visualización de Accidentes de Tráfico')),
  theme = shinytheme('darkly'),
  
  tabPanel(
    titlePanel('Frecuencia Temporal'),
    sidebarLayout(
      sidebarPanel(
        h3("Configuración de visualización"),
        
        # Widget para tipo de agregación temporal
        selectInput("freq_type", "Seleccionar frecuencia temporal:",
                    choices = c("Diario" = "daily", "Mensual" = "monthly", "Anual" = "yearly"),
                    selected = "daily"),
        
        # Rango de fechas
        dateRangeInput(
          'date_range', label = h4('Filtrar por rango de fechas'),
          start = min_date, end = max_date,
          min = min_date, max = max_date,
          format = 'yyyy-mm-dd', startview = 'year',
          language = 'es', separator = " a "
        )
      ),
      
      mainPanel(
        plotOutput("accident_plot")
      )
    )
  )
)

# SERVER
server <- function(input, output) {
  
  filtered_data <- reactive({
    req(input$date_range)
    data_time |> 
      filter(DATE_DAY >= input$date_range[1],
             DATE_DAY <= input$date_range[2])
  })
  
  output$accident_plot <- renderPlot({
    df <- filtered_data()
    
    if (input$freq_type == "daily") {
      ggplot(df, aes(x = DATE_DAY, y = FREQ_DAY)) +
        geom_line(color = "steelblue") +
        labs(title = "Frecuencia de Accidentes por Día",
             x = "Fecha", y = "Frecuencia") +
        theme_minimal()
      
    } else if (input$freq_type == "monthly") {
      df_month <- df |> distinct(DATE_MONTH, FREQ_MONTH)
      ggplot(df_month, aes(x = DATE_MONTH, y = FREQ_MONTH)) +
        geom_line(color = "steelblue") +
        labs(title = "Frecuencia de Accidentes por Mes",
             x = "Mes", y = "Frecuencia") +
        theme_minimal()
      
    } else if (input$freq_type == "yearly") {
      df_year <- df |> distinct(DATE_YEAR, FREQ_YEAR)
      ggplot(df_year, aes(x = DATE_YEAR, y = FREQ_YEAR)) +
        geom_line(color = "steelblue") +
        labs(title = "Frecuencia de Accidentes por Año",
             x = "Año", y = "Frecuencia") +
        theme_minimal()
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
