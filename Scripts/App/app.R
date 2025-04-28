library(shiny)
library(shinythemes)

# UI
ui <- navbarPage(
  p(strong("APLICACIÓN")),
  theme = shinytheme("flatly"),
  
  tabPanel(
    "Pestaña 1",
    
    sidebarLayout(
      sidebarPanel(
        varSelectInput("selector", "Seleccionar",data=NULL)
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Plot 1", plotOutput(outputId = "id1")),
          tabPanel("Plot 2", tableOutput("id2"))
        )
      )
    )
  ),
  
  tabPanel(
    "Pestaña 2",
  )
)

# SERVER
server <- function(input, output, session) {


  
}

# RUN APP
shinyApp(ui = ui, server = server)
