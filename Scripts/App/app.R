library(shiny)
library(shinythemes)
library(bslib)
library(tidyverse)
library(lubridate)
library(zoo)
library(dplyr)
library(dendextend)
library(DT)
library(ggdendro)


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
    tags$div("Clúster Jerárquico 🧩", style = "font-size: 18px; font-weight: bold;"),
    sidebarLayout(
      sidebarPanel(
        sliderInput("k", "Número de clústeres:", min = 2, max = 4, value = 3),
        checkboxInput("show_rect", "Mostrar rectángulos", TRUE)
      ),
      mainPanel(
        plotOutput("dendrogramPlot", height = "600px"),
        br(),
        h4("Interpretación"),
        p("El análisis de clúster jerárquico permite identificar patrones comunes entre los distritos de Nueva York en función de sus características relacionadas con los accidentes de tráfico. Independientemente del número de clústeres elegido, las agrupaciones tienden a formarse en torno a factores como la densidad de población, el nivel de urbanización y la distribución geográfica. Así, distritos con contextos similares tienden a agruparse juntos, ya sea por presentar una elevada concentración de tráfico, zonas predominantemente residenciales o una menor intensidad de incidentes. Estas agrupaciones ayudan a entender cómo varía la siniestralidad vial en función del entorno urbano. Este dendrograma agrupa los barrios según la suma total de personas heridas y fallecidas en los accidentes de tráfico. 
          Puedes modificar el número de clústeres (k) y el método de enlace para explorar diferentes agrupaciones."),
        h4("Totales por distrito y clúster asignado"),
        DTOutput("boroughTable")
      )
    )
  )
)


# SERVER
server <- function(input, output) {
  
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
    
    dend <- as.dendrogram(hc)
    dend <- color_branches(dend, k = input$k)
    dend <- set(dend, "labels_cex", 0.9)     # tamaño del texto
    dend <- set(dend, "hang", -1)            # evita que las etiquetas queden cortadas
    
    # Obtener etiquetas ordenadas según el dendrograma
    ordered_labels <- data_borough_sum$BOROUGH[hc$order]
    labels(dend) <- ordered_labels  # Asigna etiquetas correctamente
    
    par(las=2)
    plot(dend, main = "Dendrograma de distritos", horiz = FALSE) # vertical con etiquetas horizontales
    if (input$show_rect) rect.hclust(hc, k = input$k, border = "blue")
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
}

# Run the application 
shinyApp(ui = ui, server = server)
