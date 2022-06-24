library(shinydashboard)
library(shiny)
library(readxl)
library(magrittr)
library(tidyverse)


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
                                    )
                                    
                        ),
                        selectInput(inputId ="IDciudad",
                                    label="CIUDAD",
                                    choices = c("ARMENIA",
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
                                  splitLayout(
                                    selectInput(inputId ="IDFamilia",
                                                label="FAMILIAS PRODUCTOS",
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
                                                )
                                                
                                    ),
                                    selectInput(inputId ="IDciudad",
                                                label="CIUDAD",
                                                choices = c("ARMENIA",
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
                                                
                                    ),
                                    selectInput(inputId ="IDEstado_entrega",
                                                label="ESTADO ENTREGA",
                                                choices = c("DEVUELTO",
                                                            "ENTREGADO COMPLETO"
                                                            
                                                            
                                                )
                                                
                                    ),
                                    selectInput(inputId ="IDCumplimiento_entrega",
                                                label="CUMPLIMIENTO ENTREGA",
                                                choices = c("SI",
                                                            "NO",
                                                            "DEVUELTO"
                                                            
                                                            
                                                )
                                                
                                    ),
                                    selectInput(inputId ="IDAnno",
                                                label="AÑO",
                                                choices = c("2022"                         
                                                )
                                                
                                    ),
                                    selectInput(inputId ="IDMes",
                                                label="MES",
                                                choices = c("Enero",
                                                            "Febrero",
                                                            "Marzo",
                                                            "Abril"
                                                            
                                                            
                                                )
                                                
                                    ),
                                    selectInput(inputId ="IDDia_Entrega",
                                                label="DIA ENTREGA",
                                                choices = c("Lunes",
                                                            "Martes",
                                                            "Miércoles",
                                                            "Jueves",
                                                            "Viernes",
                                                            "Sábado",
                                                            "Domingo",
                                                            "DEVUELTO"
                                                            
                                                            
                                                )
                                                
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
                                        "Choose the variable for which you want to check the normality",
                                        choices = c("CONTEO_DESPACHOS","DIA_ENTREGA")
                                      ),
                                      radioButtons(
                                        "normchoice",
                                        "How do you want to check the normality?",
                                        choices = c("Plots", "Tests"),
                                        selected = "Plots"
                                      ),
                                      conditionalPanel(
                                        "input.normchoice == 'Plots'",
                                        selectInput(
                                          "plotchoice",
                                          "Choose which plot you want?",
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
                                  titlePanel("Nube de Palabras"),
                                  
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
                                  )
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
                                        plotOutput("deathPlot"),
                                        DT::dataTableOutput("deathTable")
                                      )
                                    )
                                  )),
                          
                          # Cuarta Tabla de contenido
                          tabItem(tabName = "Geoespacial",
                                  h2("Geoespacial")
                          )

                        )
                      )
))
