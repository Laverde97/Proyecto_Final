library(shinydashboard)
library(shiny)
library(readxl)
library(magrittr)
library(tidyverse)
library(shiny)
library(mapview)

shinyUI(dashboardPage(skin = "green",
                      dashboardHeader(title = "Dashboard Oportunidad de Entrega Sector Retail"
                                      ,titleWidth = 600),
                      dashboardSidebar(sidebarMenu(
                        menuItem("Data", tabName = "Data", icon = icon("dashboard")),
                        menuItem("Descriptivos", tabName = "Descriptivos", icon = icon("dashboard")),
                        menuItem("Gráficas", tabName = "Gráficas", icon = icon("dashboard")),
                        menuItem("Inferencia", tabName = "Inferencia", icon = icon("dashboard")),
                        menuItem("Sentimientos", tabName = "Sentimientos", icon = icon("dashboard")),
                        menuItem("Geoespacial", tabName = "Geoespacial", icon = icon("dashboard"))
                        
                        
                      ),
                      selectInput(inputId ="IDFamilia",
                                  label="FAMILIAS PRODUCTOS",
                                  choices = c("TODOS",
                                              "ACCESORIOS AUTOMOVILES",
                                              "AIRE LIBRE",
                                              "ALQUILER DE HERRAMIENTAS",
                                              "ASEO",
                                              "BANOS Y COCINAS",
                                              "CASA INTELIGENTE",
                                              "DECORACION",
                                              "ELECTRICIDAD",
                                              "ELECTROHOGAR",
                                              "FERRETERIA",
                                              "FIERRO/HIERRO",
                                              "HERRAMIENTAS Y MAQUINARIAS",
                                              "ILUMINACION Y VENTILADORES",
                                              "JARDIN",
                                              "MADERA Y TABLEROS",
                                              "MENAJE",
                                              "MUEBLES",
                                              "OBRA GRUESA",
                                              "ORGANIZACION",
                                              "PINTURA Y ACCESORIOS",
                                              "PISOS",
                                              "PLOMERIA / GASFITERIA",
                                              "PROMOCIONES - SERVICIO TECNICO",
                                              "PUERTAS/VENTANAS/MOLDURAS",
                                              "TABIQUERIA/TECHUMBRE/AISLACION",
                                              "TEMPORADA"
                                  )
                                  
                      ),
                      selectInput(inputId ="IDciudad",
                                  label="CIUDAD",
                                  choices = c("NACIONAL",
                                              "ARMENIA",
                                              "BARRANQUILLA",
                                              "BOGOTA",
                                              "BUCARAMANGA",
                                              "CALI",
                                              "CARTAGENA",
                                              "CUCUTA",
                                              "FUNZA",
                                              "GIRARDOT",
                                              "IBAGUE",
                                              "MANIZALES",
                                              "MEDELLIN",
                                              "MONTERIA",
                                              "NEIVA",
                                              "PEREIRA",
                                              "SANTA MARTA",
                                              "TUNJA",
                                              "VALLEDUPAR",
                                              "VILLAVICENCIO",
                                              "YOPAL"
                                              
                                              
                                  )
                                  
                      )
                      ),
                      dashboardBody(
                        tabItems(
                          # Data
                          
                          tabItem(tabName = "Data",
                                  titlePanel("Clientes"),
                                  fluidRow(column(DT::dataTableOutput("RawData"), width = 12)),
                                  
                          ),
                          
                          
                          
                          # Primera Tabla de contenido
                          
                          tabItem(tabName = "Descriptivos",
                                  fluidRow(
                                    valueBoxOutput("total_entregas",width = 3),
                                    valueBoxOutput("total_cumplimiento",width = 3),
                                    valueBoxOutput("total_devolucion",width = 3),
                                    valueBoxOutput("Promedio_entregas",width = 3)),
                                  fluidRow(
                                    box(
                                      title = "Entregas por dia Semana", status = "primary", solidHeader = TRUE,
                                      collapsible = TRUE,
                                      plotOutput("lineplot", height = 250)
                                    ),
                                    
                                    fluidRow(
                                      box(
                                        title = "Diagrama de Caja", status = "primary", solidHeader = TRUE,
                                        collapsible = TRUE,
                                        plotOutput("boxplot", height = 250)
                                      )),
                                    
                                    fluidRow(
                                      dataTableOutput("tab"))
                                  )),
                          
                          
                          # Segunda  Tabla de contenido
                          tabItem(tabName = "Inferencia",
                                  sidebarLayout(
                                    sidebarPanel(
                                      selectInput(
                                        "varchoice",
                                        "Elija la variable para la que desea comprobar la normalidad",
                                        choices = c("CONTEO_DESPACHOS","DIA_ENTREGA")
                                      ),
                                      radioButtons(
                                        "normchoice",
                                        "¿Como quieres comprobar la normalidad?",
                                        choices = c("Plots", "Tests"),
                                        selected = "Plots"
                                      ),
                                      conditionalPanel(
                                        "input.normchoice == 'Plots'",
                                        selectInput(
                                          "plotchoice",
                                          "Elige el grafico que  quieres",
                                          choices = c("Histogram", "QQ-Plot")
                                        )
                                      )
                                      
                                      
                                    ),
                                    mainPanel(
                                      conditionalPanel("input.normchoice == 'Plots'", plotOutput("myplot")),
                                      conditionalPanel("input.normchoice == 'Tests'", verbatimTextOutput("mytest"))
                                    )
                                  )),
                          
                          # Tercera Tabla de contenido
                          tabItem(tabName = "Sentimientos",
                                  
                                  h2("Satisfaccion Cliente"),
                                  fluidRow(
                                    valueBoxOutput("Detractores",width = 3),
                                    valueBoxOutput("Neutros",width = 3),
                                    valueBoxOutput("Promotores",width = 3),
                                    valueBoxOutput("Satisfaccion",width = 3)),
                                  titlePanel("Nube de Palabras"),
                                  
                                  fluidRow(
                                    sidebarLayout(
                                      # Sidebar with a slider and selection inputs
                                      sidebarPanel(
                                        selectInput("Selecciona", "Familia Producto",
                                                    choices = c("ACCESORIOS AUTOMOVILES",
                                                                "AIRE LIBRE",
                                                                "ALQUILER DE HERRAMIENTAS",
                                                                "ASEO",
                                                                "BANOS Y COCINAS",
                                                                "CASA INTELIGENTE",
                                                                "DECORACION",
                                                                "ELECTRICIDAD",
                                                                "ELECTROHOGAR",
                                                                "FERRETERIA",
                                                                "FIERRO/HIERRO",
                                                                "HERRAMIENTAS Y MAQUINARIAS",
                                                                "ILUMINACION Y VENTILADORES",
                                                                "JARDIN",
                                                                "MADERA Y TABLEROS",
                                                                "MENAJE",
                                                                "MUEBLES",
                                                                "OBRA GRUESA",
                                                                "ORGANIZACION",
                                                                "PINTURA Y ACCESORIOS",
                                                                "PISOS",
                                                                "PLOMERIA / GASFITERIA",
                                                                "PROMOCIONES - SERVICIO TECNICO",
                                                                "PUERTAS/VENTANAS/MOLDURAS",
                                                                "TABIQUERIA/TECHUMBRE/AISLACION",
                                                                "TEMPORADA"
                                                    )),
                                        hr(),
                                        sliderInput("freq",
                                                    "Mínimo Frecuencias:",
                                                    min = 1,  max = 150, value = 15),
                                        sliderInput("max",
                                                    "Máximo número de palabras:",
                                                    min = 1,  max = 250,  value = 100)
                                      ),
                                      
                                      # Show Word Cloud
                                      mainPanel(
                                        plotOutput("plot")
                                      )
                                    ))
                                  
                          ),
                          #Gráficas 
                          tabItem(tabName = "Gráficas",
                                  fluidRow(
                                    sidebarLayout(
                                      sidebarPanel(
                                        selectInput(inputId = "Mes",
                                                    label = "Seleccione el Mes:",
                                                    choices = c("Enero",
                                                                "Febrero",
                                                                "Marzo",
                                                                "Abril")),
                                        selectInput(inputId = "Famili",
                                                    label = "Familia Producto:",
                                                    choices = unique(DATA_NP_SIN_DEVOLUCIONES$FAMILIA_PRODUCTO)),
                                        selectInput(inputId = "Ciudad",
                                                    label = "Ciudad",
                                                    choices =unique(DATA_NP_SIN_DEVOLUCIONES$CIUDAD))
                                      ),
                                      # Show plot and table
                                      mainPanel(
                                        plotOutput("deathPlot_Ciudad"),
                                        DT::dataTableOutput("deathTable_Ciudad"),
                                        plotOutput("deathPlot_Mes"),
                                        DT::dataTableOutput("deathTable_Mes"),
                                        plotOutput("deathPlot"),
                                        DT::dataTableOutput("deathTable")
                                        
                                      )
                                    )
                                  )),
                          
                          # Cuarta Tabla de contenido
                          tabItem(tabName = "Geoespacial",   
                                  mainPanel(
                                    mapview:::mapviewOutput("map")
                                  )
                                  
                          )
                          
                        )
                      )
))
