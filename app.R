## Proyecto Dashboard del Equipo 15
#setwd("~/Documents/CURSOS/DATA SCIENCE/BEDU - SANTANDER/FASE 2 - ESTADÍSTICA Y R/PROYECTO FINAL")
#install.packages("shinydashboard")
library(shiny)
library(shinydashboard)
#install.packages("shinythemes")
library(shinythemes)
#El primer paso es determinar la Interfaz de Usuario del Dashboard
ui <-
  fluidPage(
    dashboardPage(
      dashboardHeader(title = "Proyecto R Equipo 15 BEDU 2022"),
      dashboardSidebar(
        sidebarMenu(
          menuItem("Gráficos de Barras", tabName = "Dashboard", icon = icon("dashboard", verify_fa = FALSE)),
          menuItem("Imágenes del Postwork 3", tabName = "img", icon = icon("file-picture-o", verify_fa = FALSE)),
          menuItem("Data Table", tabName = "data_table", icon = icon("table", verify_fa = FALSE)),
          menuItem("Imágenes Factores de Ganancia", tabName = "img2", icon = icon("file-picture-o", verify_fa = FALSE))
        )
      ),
      dashboardBody(
        tabItems(
          # Gráficos de Barras
          tabItem(tabName = "Dashboard",
                  fluidRow(
                    titlePanel("Gráficos de goles locales y visitantes"),
                    selectInput("x", "Seleccione el valor de X",
                                choices = c("home.score", "away.score")),
                    selectInput("zz", "Selecciona la variable del grid",
                                choices = c("away.team")),
                    box(plotOutput("plot1", height = 400, width = 700)),
            
                  )
          ),
          # Imágenes del Postwork 3
          tabItem(tabName = "img",
                  fluidRow(
                    titlePanel(h3("Imágenes del Postwork 3")),
                    div(img(src ="probabilidad_casa.png", heigt = 350, width = 350, alt="Hubo un error", deleteFile=FALSE), style = "text-align:center;"),
                    div(img(src ="probabilidad_visita.png", heigt = 350, width = 350, alt="Hubo un error", deleteFile=FALSE), style = "text-align:center;"),
                    div(img(src ="Rplot_calor.png", heigt = 350, width = 350, alt="Hubo un error", deleteFile=FALSE), style = "text-align:center;")
                  )
          ),
          tabItem(tabName = "data_table",
                  fluidRow(
                    titlePanel(h3("Data Table")),
                    dataTableOutput ("data_table")
                  )
          ),
          tabItem(tabName = "img2",
                  fluidRow(
                    titlePanel(h3("Imágenes de Factores de Ganancia Promedio y Máximo")),
                    div(img(src ="Momios_maximos.png", heigt = 350, width = 350, alt="Hubo un error", deleteFile=FALSE), style = "text-align:left;"),
                    div(img(src ="Momios_promedio.png", heigt = 350, width = 350, alt="Hubo un error", deleteFile=FALSE), style = "text-align:right;")
                  )
          )
        )
      )
    )
  )
#Una vez definidas las pestañas del Dashboard, se procede a programar el servidor
server <- function(input, output) {
  library(ggplot2)
  #Gráficos de Barras
  output$plot1 <- renderPlot({
    x <- data[,input$x]
    bin <- seq(min(x), max(x), length.out = 1)
    ggplot(data, aes(x)) +
      geom_bar( breaks = bin, col = "black", fill = "orange") +
      labs( xlim = c(0, max(x))) +
      theme_light() + 
      xlab(input$x) + ylab("Frecuencia") +
      facet_wrap(input$zz)
  })
  #Imágenes del Postwork 3
  output$img <- renderImage({
    list(src ="Momios_maximos.png", heigt = 350, width = 350, alt="Hubo un error", deleteFile=FALSE)
  }
  )
  #Data Table
  output$data_table <- renderDataTable( {data},
                                        options = list(aLengthMenu = c(5,25,50),
                                                       iDisplayLength = 5)
  )
  #Imágenes de Factores de Ganancias Máximas y Promedio
  output$img2 <- renderImage({
    list(src ="Momios_maximos.png", heigt = 350, width = 350, alt="Hubo un error", deleteFile=FALSE)
    list(src ="Momios_promedio.png", heigt = 350, width = 350, alt="Hubo un error", deleteFile=FALSE)
  }
  )
}
shinyApp(ui, server)
