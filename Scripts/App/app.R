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
library(FactoMineR)
library(shinyjs)


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

# Variables HOUR y DAYS_OF_WEEK:
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
  
  useShinyjs(),
  
  theme = ny_theme,
  
  titlePanel("Análisis de Datos de Accidentes en NYC"),
  
  tabsetPanel(
    
    tabPanel("Análisis de Agrupamiento",
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
    ),
    
    tabPanel("Análisis de Correspondencias",
             sidebarLayout(
               sidebarPanel(
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
               ),
               mainPanel(
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


# Server
server <- function(input, output, session) {
  
  # ELEMENTOS PARA EL CLÚSTER JERÁRQUICO
  
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
    var1 <- input$var1
    var2 <- input$var2
    df <- data_sampled
    
    # Filtrado de top 10 para VEHICLE_1 y CAUSE
    if (var1 %in% c("CAUSE", "VEHICLE_1")) {
      top10_var1 <- names(sort(table(df[[var1]]), decreasing = TRUE))[1:10]
      df <- df[df[[var1]] %in% top10_var1, ]
    }
    if (var2 %in% c("CAUSE", "VEHICLE_1")) {
      top10_var2 <- names(sort(table(df[[var2]]), decreasing = TRUE))[1:10]
      df <- df[df[[var2]] %in% top10_var2, ]
    }
    
    # Asegurarse de que las variables no estén vacías ni completamente NA
    if (nrow(df) == 0 || all(is.na(df[[var1]])) || all(is.na(df[[var2]]))) {
      return(NULL)
    }
    
    # Filtrado de las variables para que no tengan valores vacíos
    df_filtered <- df %>%
      filter(!is.na(df[[var1]]) & !is.na(df[[var2]]))
    
    table(df_filtered[[var1]], df_filtered[[var2]])
  })
  
  
  
  ca_result <- reactive({
    req(input$var1, input$var2)
    
    var1_top <- names(sort(table(data_sampled[[input$var1]]), decreasing = TRUE))[1:10]
    var2_top <- names(sort(table(data_sampled[[input$var2]]), decreasing = TRUE))[1:10]
    
    df <- data_sampled[data_sampled[[input$var1]] %in% var1_top & 
                         data_sampled[[input$var2]] %in% var2_top, ]
    
    tab <- table(df[[input$var1]], df[[input$var2]])
    
    # Eliminar filas o columnas vacías (todo ceros)
    tab <- tab[rowSums(tab) > 0, colSums(tab) > 0]
    
    print(tab)
    
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
