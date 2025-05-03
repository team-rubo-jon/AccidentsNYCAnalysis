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
library(cluster)
library(factoextra)


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
ui <- fluidPage(
  
  # Aplicar el tema personalizado 'ny_theme'
  theme = ny_theme,
  
  titlePanel("Análisis de Agrupamiento"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("plot_choices", 
                         "Selecciona las opciones para ver:",
                         choices = c("Dendrograma", "Silueta", "Tabla"),
                         selected = c("Dendrograma", "Silueta", "Tabla")),
      sliderInput("k", 
                  "Número de grupos (k):", 
                  min = 2, max = 4, value = 3),
      checkboxInput("show_rect", "Mostrar rectángulos en el dendrograma", value = TRUE)
    ),
    
    mainPanel(
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
    )
  )
)

# Server
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
}

# Run the application 
shinyApp(ui = ui, server = server)
