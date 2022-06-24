library(shiny)
library(leaflet)
# getwd()

Homecenter <- read_excel("C:/Users/Admin/Documents/SEBASTIAN LAVERDE/MAESTRIA/PRIMER SEMESTRE/Introduccion a la estadistica y probabilidad/BASES DE DATOS STALYN/PROYECTO_HOMECENTER/data/2022 (1).xlsx")
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
    DATA_NP %>%
      filter(CIUDAD ==input$IDciudad,
             FAMILIA_PRODUCTO ==input$IDFamilia,
             Mes==input$IDMes)  %>% 
      summarise(
        sum(DATA_NP$CONTEO_DESPACHOS))%>%
      prettyNum(big.mark = ",") %>%
      valueBox(subtitle = "Numero entregas", icon = icon("credit-card"))
  })
  
  DATA_CUMPLIMIENTO <- DATA_NP%>%filter(CUMPLIMIENTO_ENTREGA==c("SI"))
  
  #Tasa_Cumplimiento
  output$total_cumplimiento <- renderValueBox({
    DATA_CUMPLIMIENTO %>%
           filter(CIUDAD ==input$IDciudad,
             FAMILIA_PRODUCTO ==input$IDFamilia,
             Mes==input$IDMes)  %>% 
      summarise(
        paste0(round((sum(DATA_CUMPLIMIENTO$CONTEO_DESPACHOS)/sum(DATA_NP$CONTEO_DESPACHOS))*100,2)),"%")%>%
      valueBox(subtitle = "% Cumplimiento Entregas", icon = icon("thumbs-up"
                                                                 , lib = "glyphicon"),color = "green")
  })
  
  
  DATA_DEVOLUCION <- DATA_NP%>%filter(ESTADO_ENTREGA==c("DEVUELTO"))
  
  #Tasa_Devolucion
  output$total_devolucion <- renderValueBox({
    DATA_DEVOLUCION  %>%
      filter(CIUDAD ==input$IDciudad,
             FAMILIA_PRODUCTO ==input$IDFamilia,
             Mes==input$IDMes)  %>% 
      summarise(
        paste0(round((sum(DATA_DEVOLUCION$CONTEO_DESPACHOS)/sum(DATA_NP$CONTEO_DESPACHOS))*100,2)),"%")%>%
      valueBox(subtitle = "% Devoluciones", icon = icon("thumbs-down"
                                                        , lib = "glyphicon"),color = "red")
  })
  
  DIAS_ENTREGA<- DATA_NP %>% group_by(DATA_NP$FECHA_CREACION) %>% summarise(sum(DATA_NP$CONTEO_DESPACHOS))
  DIAS_CREACION_ENTREGA<-nrow(DIAS_ENTREGA)
  
  #Promedio_Diario_Entregas
  output$Promedio_entregas <- renderValueBox({
    DATA_NP  %>%
      filter(CIUDAD ==input$IDciudad,
             FAMILIA_PRODUCTO ==input$IDFamilia,
             Mes==input$IDMes)  %>% 
      summarise(
        round(sum(DATA_NP$CONTEO_DESPACHOS)/DIAS_CREACION_ENTREGA,0))%>%
      prettyNum(big.mark = ",") %>%
      valueBox(subtitle = "Promedio Diario Entregas", icon = icon("list"),color = "purple")
  })
  
  #Entregas_dia
  
  
  
  output$lineplot <- renderPlot({
    DIA_ENTREGA =factor(DATA_NP_SIN_DEVOLUCIONES$DIA_SEMANA_ENTREGA,
                        levels = c("Lunes","Martes","Miércoles","Jueves","Viernes","Sábado","Domingo"),
                        labels = c("Lunes","Martes","Miércoles","Jueves","Viernes","Sábado","Domingo"))
    plot(DIA_ENTREGA,col=colors(),xlab="DIA_SEMANA",ylim=c(0,50000))
  })
 # BoxPlot Didactico 
  output$BoxplotDic <- renderPlot({
    his(DATA_NP_SIN_DEVOLUCIONES[,input$VarHom],main = input$title, xlab = input$xlab,
         ylab = input$ylab)
  })
  
  
  
  # Boxplot
  output$boxplot <- renderPlot({
    DIA_ENTREGA =factor(DATA_NP_SIN_DEVOLUCIONES$DIA_SEMANA_ENTREGA,
                        levels = c("Lunes","Martes","Miércoles","Jueves","Viernes","Sábado","Domingo"),
                        labels = c("Lunes","Martes","Miércoles","Jueves","Viernes","Sábado","Domingo"))
    boxplot(DATA_NP_SIN_DEVOLUCIONES$CONTEO_DESPACHOS~DIA_ENTREGA,col=colors()
            ,xlab ="DIA_ENTREGA",ylab = "CONTEO_DESPACHOS",log="y")
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
  var = reactive({
    DATA_NP_SIN_DEVOLUCIONES[, input$varchoice]
    
  })
  output$myplot =  renderPlot({
    if (input$plotchoice == "Histogram")
      return(hist(var(), main = "Histogram", xlab = input$varchoice))
    else
      return(qqnorm(var(), main = paste("QQ plot of", input$varchoice)))
  })
  output$mytest = renderPrint({
    shapiro.test(var())
  })

  
  
  
  
  
  
  
  
  
  
  
  

})
