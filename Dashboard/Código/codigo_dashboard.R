# Cargar librerías necesarias

library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(readr)
library(here)

# Interfaz Usario

ui <- fluidPage(
  titlePanel("Dashboard de Desigualdades Sociales y Económicas en Europa"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Seleccione una variable para visualizar:", 
                  choices = c("Felicidad" = "happy", "Salud" = "health", "Educación" = "eisced"))
    ),
    mainPanel(
      leafletOutput("mymap"),
      plotOutput("plot")
    )
  )
)


# Servidor
server <- function(input, output, session) {
  data_path_depurados <- here::here('Datos/Base de datos depurada/datos_clean2.csv')
  datos_dashboard <- read_csv(data_path_depurados) %>%
    mutate(
      cntry = as.factor(cntry),
      gndr = as.factor(gndr),
      eisced = as.factor(eisced),
      evmar = as.factor(evmar),
      health = as.factor(health),
      happy = as.factor(happy)
    )
  
  # Cargar el mapa de Europa desde el paquete rnaturalearth
  europe_map <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  
  output$mymap <- renderLeaflet({
    # Crear un mapa básico centrado en Europa
    leaflet(europe_map) %>%
      addTiles() %>%
      addPolygons(stroke = TRUE, smoothFactor = 0.5, fillOpacity = 0.5,
                  color = ~colorQuantile("YlOrRd", as.numeric(datos_dashboard$happy), n = 5)(as.numeric(datos_dashboard$happy)),
                  popup = ~paste("País:", name, "<br> Felicidad:", as.numeric(datos_dashboard$happy))) %>%
      setView(lng = 10, lat = 50, zoom = 4)
  })
  
  output$plot <- renderPlot({
    selected_var <- input$variable
    
    datos_agrupados <- datos_dashboard %>%
      group_by(!!sym(selected_var)) %>%
      summarize(count = n())
    
    ggplot(datos_agrupados, aes(x = !!sym(selected_var), y = count)) +
      geom_bar(stat = "identity") +
      labs(title = paste("Distribución de", input$variable))
  })
}

shinyApp(ui = ui, server = server)