library(xts)
library(shiny)
library(tidyverse)
library(shinythemes)
library(jsonlite)
library(forecast)
library(xts)
library(tradestatistics)
library(ggplot2)
library(dplyr)
library(DT)

# Listado Empresa ---------------------------------------------------------


lista_empresas <- c("NUEVAPOLAR", "SMU", "BESALCO", "COPEC", "FALABELLA",
                    "BSANTANDER", "CMPC", "CHILE", "SQM-B", "ENELAM", "CENCOSUD",
                    "BCI", "LTM", "ENELCHILE", "SM-CHILE B", "CCU", "PARAUCO",
                    "ITAUCORP", "AGUAS-A", "COLBUN", "ENTEL", "ECL", "CONCHATORO",
                    "RIPLEY", "AESGENER", "ANDINA-B", "SONDA", "CAP", "ILC",
                    "SALFACORP", "SECURITY", "VAPORES", "ENELGXCH", "ANTARCHILE",
                    "BANMEDICA", "EMBONOR-B", "FORUS", "IAM", "MASISA", "ORO BLANCO",
                    "SK", "SMSAAM")


# Descargador -------------------------------------------------------------

obtener_indicadores <- function(empresa = "NUEVAPOLAR") 
  
{
  
  url <- stringr::str_c("https://www.elmercurio.com/inversiones/json/json.aspx?categoria=",
                        empresa, "&time=10&indicador=2")
  base1 <- jsonlite::read_json(url)$Data %>%
    stringr::str_split(";") %>%
    dplyr::first() %>%
    I() %>%
    readr::read_delim(delim = ",", col_names = c("fecha", "precio", "vol"))
  base1 <- base1 %>%
    mutate(
      fecha = lubridate::ymd_hms(fecha),
      anio = lubridate::year(fecha)
    )
}

d <- data.frame(obtener_indicadores("AESGENER"))

# UI ----------------------------------------------------------------------


ui <- fluidPage(
  
  #  Incorporar un diseño con el paquete shinythemes o bslib
  theme = shinytheme("spacelab"),
  
  titlePanel("Visualización de datos de empresas inversionstas"),
  
  #Disponga la información a través de un layout de sidebar y mainpanel
  
  sidebarLayout(
    
    #Desarrollo sidebarPanel
    sidebarPanel(
      
      #Posea un input de tipo select para escoger un elemento de lista_empresas
      
      selectInput(inputId = "compania",
                  label = "Seleccione Empresa",
                  choices = lista_empresas
                  
      ),
      
      #Posea un input range slider con el fin de seleccionar años
      sliderInput(
        inputId = "fecha_anio",
        label = h3("Seleccione Año"),
        
        min = min(d$anio),
        max = max(d$anio),
        value = c(min(d$anio),max(d$anio)),
        sep=""
      )
    ),
    
    
    #Desarrollo del mainPanel
    mainPanel(
      
      tabsetPanel(
        
        #La aplicación debe considerar salidas/outpus para un gráfico para los datos históricos
        
        tabPanel(title = "Gráfico tendencias",
                 plotOutput("grafico_pred")
        ),
        
        #La aplicación debe considerar salidas/outpus para una tabla resumida por años
        
        tabPanel(title = "Tabla resumen por año",
                 DTOutput(outputId = "tabla")
        )
      )
    )
  )
)
    
# Server ------------------------------------------------------------------



server <- function(input, output) {
  
  #outputs para tabla resumen por año
  output$tabla = renderDT({
    c<- d %>%
      filter(anio >= input$fecha_anio[1] &
               anio <= input$fecha_anio[2]) %>%
    
       group_by(Año=anio) %>%
      summarise("Precio Promedio Anual" =round(mean(precio),0))
    datatable(c)
  })
  
  #output para grafico de tendencias
  output$grafico_pred = renderPlot({
    #coneccion con descargador a traves de funcion y filtro según inputs de ui
    base1= obtener_indicadores(input$compania)
    base1 = base1 %>%
      filter(anio >= input$fecha_anio[1] &
               anio <= input$fecha_anio[2])
    
    # detalle grafico
    ggplot(base1)+geom_line(aes(x=fecha,y=precio))
  })
  
}


shinyApp(ui, server)


install.packages("rsconnect")
library(rsconnect)




