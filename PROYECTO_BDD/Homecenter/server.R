library(shiny)
library(leaflet)
library(readxl)
# getwd()

Homecenter <- read_excel("C:/Users/ingca/Downloads/2022 (1).xlsx")
Homecenter$FECHA_CREACION= as.Date(Homecenter$FECHA_CREACION, format = "%d/%m/%Y")
Homecenter$FECHA_REAL_ENTREGA= as.Date(Homecenter$FECHA_REAL_ENTREGA, format = "%d/%m/%Y")
Homecenter$FECHA_COMPROMETIDA= as.Date(Homecenter$FECHA_COMPROMETIDA, format = "%d/%m/%Y")

DATA_NP= Homecenter
DATA_NP_SIN_DEVOLUCIONES=DATA_NP
DATA_NP_SIN_DEVOLUCIONES=DATA_NP_SIN_DEVOLUCIONES %>% filter(DIA_SEMANA_ENTREGA !="DEVUELTO")
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   # DATA 
  
  output$RawData <- DT::renderDataTable(
    DT::datatable({
      Homecenter
    }, options = list(lengthMenu = list(c(7, 15, -1), c("5", "15", "Todos")), pageLength =
                        15),
    filter = "top",
    selection = "multiple",
    style = "bootstrap")
  )
  
# DESCRIPTIVO
  
  output$tab <- renderDataTable({
    DATA_NP %>% filter(CIUDAD ==input$IDciudad,
                       FAMILIA_PRODUCTO ==input$IDFamilia) 
  })
  
  #conteo_Total_Entregas
  output$total_entregas <- renderValueBox({
    total_entregas=DATA_NP %>%
      filter(CIUDAD == input$IDciudad &
             FAMILIA_PRODUCTO ==input$IDFamilia)
    ntotal_entregas=nrow(total_entregas)
      valueBox(value=ntotal_entregas, subtitle = "Numero entregas", icon = icon("credit-card"))
  })
  
  #Tasa_Cumplimiento
  output$total_cumplimiento <- renderValueBox({
    total_entregas=DATA_NP %>%
      filter(CIUDAD == input$IDciudad &
               FAMILIA_PRODUCTO ==input$IDFamilia)
    ntotal_entregas=nrow(total_entregas)
    
    total_cumple=DATA_NP %>%
      filter(CIUDAD == input$IDciudad &
               FAMILIA_PRODUCTO ==input$IDFamilia & CUMPLIMIENTO_ENTREGA=="SI")
    ntotal_cumple=nrow(total_cumple)
    
    por_cumple=paste0(round(ntotal_cumple/ntotal_entregas*100,2),"%")
      valueBox(value=por_cumple, subtitle = "% Cumplimiento Entregas", icon = icon("thumbs-up"
                                                                 , lib = "glyphicon"),color = "green")
  })
  
  
  
  
  #Tasa_Devolucion
  output$total_devolucion <- renderValueBox({
    total_entregas=DATA_NP %>%
      filter(CIUDAD == input$IDciudad &
               FAMILIA_PRODUCTO ==input$IDFamilia)
    ntotal_entregas=nrow(total_entregas)
    
    total_dev=DATA_NP %>%
      filter(CIUDAD == input$IDciudad &
               FAMILIA_PRODUCTO == input$IDFamilia & CUMPLIMIENTO_ENTREGA == "DEVUELTO")
    ntotal_dev=nrow(total_dev)
    
    por_devo=paste0(round(ntotal_dev/ntotal_entregas*100,2),"%")
    
      valueBox(value=por_devo, subtitle = "% Devoluciones", icon = icon("thumbs-down"
                                                        , lib = "glyphicon"),color = "red")
  })
  
  
  #Promedio_Diario_Entregas
  output$Promedio_entregas <- renderValueBox({
      total_entregas=DATA_NP %>%
        filter(CIUDAD == input$IDciudad &
                 FAMILIA_PRODUCTO ==input$IDFamilia)
      ntotal_entregas=nrow(total_entregas)
    
      prom_diario=round(ntotal_entregas/120,2)
      
      valueBox(value=prom_diario, subtitle = "Promedio Diario Entregas", icon = icon("list"),color = "purple")
  })
  
  #Entregas_dia
  
  
  
  output$lineplot <- renderPlot({
    DATA_NP_SIN_DEVOLUCIONES2=DATA_NP_SIN_DEVOLUCIONES %>% 
      filter(CIUDAD == input$IDciudad &
               FAMILIA_PRODUCTO == input$IDFamilia)
    
    barplot(table(DATA_NP_SIN_DEVOLUCIONES2$DIA_SEMANA_ENTREGA),col=colors(),xlab="DIA SEMANA")
  })
 # BoxPlot Didactico 
  output$boxplot <- renderPlot({
    DATA_NP_SIN_DEVOLUCIONES2=DATA_NP_SIN_DEVOLUCIONES %>% 
      filter(CIUDAD == input$IDciudad &
               FAMILIA_PRODUCTO == input$IDFamilia)
    boxplot(DATA_NP_SIN_DEVOLUCIONES2$CONTEO_DESPACHOS ~ DATA_NP_SIN_DEVOLUCIONES2$DIA_SEMANA_ENTREGA,
            col=colors(),xlab="DIA SEMANA", ylab="CONTEO DESPACHOS")
  })
  
  
  
# PANEL GRÁFICAS
  selections = reactive({
    req(input$Mes)
    req(input$Famili)
    req(input$Ciudad)
    filter(DATA_NP_SIN_DEVOLUCIONES, MES_CREACION == input$Mes) %>%
      filter(FAMILIA_PRODUCTO %in% input$Famili) %>%
      filter(CIUDAD %in% input$Ciudad)
  })
  output$deathPlot = renderPlot({
    ggplot(data = selections(), aes(x = reorder(DIA_SEMANA_ENTREGA, -CONTEO_DESPACHOS), y = CONTEO_DESPACHOS )) +
      geom_bar(stat = 'identity', color = 'steelblue', fill = 'steelblue') +
      labs(
        title = " ",
        x = "DIA_SEMANA_ENTREGA",
        y = "CONTEO_DESPACHOS"
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust=1))
  })
  
  output$deathTable = 
    DT::renderDataTable({
      DT::datatable(selections()[,c("ANNO_CREACION", "FECHA_CREACION", "FECHA_REAL_ENTREGA", "FECHA_COMPROMETIDA","FAMILIA_PRODUCTO",
                                    "ESTADO_ENTREGA", "CONTEO_DESPACHOS", "DIA_ENTREGA", "CUMPLIMIENTO_ENTREGA", "TIENDA_ENTREGA","MES_CREACION",
                                    "SEMANA_ENTREGA")],
                    colnames = colnames(unique(DATA_NP_SIN_DEVOLUCIONES)),
                    options = list(order = list(2, 'des')),
                    rownames = FALSE,
      )
      
    })
  
  
  output$deathPlot_Ciudad = renderPlot({
    ggplot(data = selections(), aes(x = reorder(CIUDAD, -CONTEO_DESPACHOS), y = CONTEO_DESPACHOS )) +
      geom_bar(stat = 'identity', color = 'steelblue', fill = 'steelblue') +
      labs(
        title = " ",
        x = "CIUDAD",
        y = "CONTEO_DESPACHOS"
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust=1))
  })
  
  output$deathTable_Ciudad  = 
    DT::renderDataTable({
      DT::datatable(selections()[,c("ANNO_CREACION", "FECHA_CREACION", "FECHA_REAL_ENTREGA", "FECHA_COMPROMETIDA","FAMILIA_PRODUCTO",
                                    "ESTADO_ENTREGA", "CONTEO_DESPACHOS", "DIA_ENTREGA", "CUMPLIMIENTO_ENTREGA", "TIENDA_ENTREGA","MES_CREACION",
                                    "SEMANA_ENTREGA")],
                    colnames = colnames(unique(DATA_NP_SIN_DEVOLUCIONES)),
                    options = list(order = list(2, 'des')),
                    rownames = FALSE,
      )
      
    })
  
  
  output$deathPlot_Mes = renderPlot({
    ggplot(data = selections(), aes(x = reorder(MES_CREACION, -CONTEO_DESPACHOS), y = CONTEO_DESPACHOS )) +
      geom_bar(stat = 'identity', color = 'steelblue', fill = 'steelblue') +
      labs(
        title = " ",
        x = "MES_CREACION",
        y = "CONTEO_DESPACHOS"
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust=1))
  })
  
  output$deathTable_Mes  = 
    DT::renderDataTable({
      DT::datatable(selections()[,c("ANNO_CREACION", "FECHA_CREACION", "FECHA_REAL_ENTREGA", "FECHA_COMPROMETIDA","FAMILIA_PRODUCTO",
                                    "ESTADO_ENTREGA", "CONTEO_DESPACHOS", "DIA_ENTREGA", "CUMPLIMIENTO_ENTREGA", "TIENDA_ENTREGA","MES_CREACION",
                                    "SEMANA_ENTREGA")],
                    colnames = colnames(unique(DATA_NP_SIN_DEVOLUCIONES)),
                    options = list(order = list(2, 'des')),
                    rownames = FALSE,
      )
      
    })
# Nube de Palabras
  terms <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        getTermMatrix(input$selection)
      })
    })
  })
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot <- renderPlot({
    v <- terms()
    wordcloud_rep(names(v), v, scale=c(2,1),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Dark2"))
  })
  
  
# PANEL INFERENCIA
  
  output$myplot =  renderPlot({
    if(input$varchoice=="CONTEO_DESPACHOS"){
      variable_esc=DATA_NP_SIN_DEVOLUCIONES$CONTEO_DESPACHOS
      if (input$plotchoice == "Histogram")
        return(hist(variable_esc, main = "Histograma", xlab = input$varchoice))
      else
        return(qqnorm(variable_esc, main = paste("Cuantil-Cuantil de", input$varchoice)))
    }else if(input$varchoice=="DIA_ENTREGA"){
      variable_esc=DATA_NP_SIN_DEVOLUCIONES$DIA_ENTREGA
      if (input$plotchoice == "Histogram")
        return(hist(variable_esc, main = "Histograma", xlab = input$varchoice))
      else
        return(qqnorm(variable_esc, main = paste("Cuantil-Cuantil de", input$varchoice)))
    }
      
  })
  output$mytest = renderPrint({
    if(input$varchoice=="CONTEO_DESPACHOS"){
      variable_esc=sample(DATA_NP_SIN_DEVOLUCIONES$CONTEO_DESPACHOS,5000)
    }else if(input$varchoice=="DIA_ENTREGA"){
      variable_esc=sample(DATA_NP_SIN_DEVOLUCIONES$DIA_ENTREGA,5000)
    }
    shapiro.test(variable_esc)
  })

  ###### satisfaccion cliente
  
  
  DATA_ENCUESTA <- read_excel("C:/Users/ingca/Downloads/2022 (1).xlsx", 
                              sheet = "Encuesta")
  
  DATA_ENCUESTA_DETRACTORES<- DATA_ENCUESTA %>% filter(CALIFICACION %in% c(1,2,3,4,5,6))
  CONTEO_DETRACTORES<-nrow(DATA_ENCUESTA_DETRACTORES)
  
  DATA_ENCUESTA_NEUTROS<- DATA_ENCUESTA %>% filter(CALIFICACION %in% c(7,8))
  CONTEO_NEUTROS<-nrow(DATA_ENCUESTA_NEUTROS)
  
  DATA_ENCUESTA_PROMOTORES<- DATA_ENCUESTA %>% filter(CALIFICACION %in% c(9,10))
  CONTEO_PROMOTORES<-nrow(DATA_ENCUESTA_PROMOTORES)
  
  TOTAL_ENCUESTA <-CONTEO_DETRACTORES+CONTEO_NEUTROS+CONTEO_PROMOTORES
  
  INDICADOR_DETRACTORES <- (CONTEO_DETRACTORES/TOTAL_ENCUESTA)
  INDICADOR_NEUTROS <- (CONTEO_NEUTROS/TOTAL_ENCUESTA)
  INDICADOR_PROMOTORES <- (CONTEO_PROMOTORES/TOTAL_ENCUESTA)
  
  NPS <-(INDICADOR_PROMOTORES-INDICADOR_DETRACTORES)
  
  
  output$Satisfaccion <- renderValueBox({
    DATA_NP %>%
      summarise(paste0(round((sum(
        NPS
      )) * 100, 2)), "%") %>%
      valueBox(
        subtitle = "Satisfaccion Cliente = % Promotores - % Detractores",
        icon = icon("thumbs-up"
                    , lib = "glyphicon"),
        color = "olive"
      )
  })
  
  
  output$Detractores <- renderValueBox({
    DATA_NP %>%
      summarise(
        paste0(round((sum(INDICADOR_DETRACTORES))*100,2)),"%")%>%
      valueBox(subtitle = "% Detractores(Calificacion 1-6)", icon = icon("angry"),color="red")
  })
  
  output$Neutros <- renderValueBox({
    DATA_NP %>%
      summarise(
        paste0(round((sum(INDICADOR_NEUTROS))*100,2)),"%")%>%
      valueBox(subtitle = "% Neutros(Calificacion 7-8)", icon = icon("meh"),color="orange")
  })
  
  output$Promotores <- renderValueBox({
    DATA_NP %>%
      summarise(
        paste0(round((sum(INDICADOR_PROMOTORES))*100,2)),"%")%>%
      valueBox(subtitle = "% Promotores (Calificacion 9-10)", icon = icon("smile"),color="green")
  })
  
  
  
  
  
  
  
  
  
  
  
  

})
