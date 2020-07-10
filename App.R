


if(!require(pxR)){install.packages("pxR")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(circlize)){install.packages("circlize")}
if(!require(shinyWidgets)){install.packages("shinyWidgets")}


# Programa
url.sexo<-'https://www.ine.es/jaxiT3/files/t/es/px/8105.px?nocab=1'
# lectura de datos
flujos_se<-read.px(url.sexo)
datos_se<-flujos_se$DATA$value
# Recodificacion
levels(datos_se[,2])<-levels(datos_se[,3])<-c('Total', 'Ocupados', 'Parados', 'Inactivos', 'No consta')
names(datos_se)<-c('Periodo','Trimestre Anterior','Trimestre actual','Sexo','valor')
# funcion para agredir 
filtro_se<-function(datasetinput,fecha){
  sexos<-as.character(datasetinput)
  z <-datos_se%>%
    select(names(datos_se)[c(2,3,5)])%>%
    filter(datos_se[,1]==fecha&datos_se[,4]==sexos&datos_se[,2]!='Total'&datos_se[,3]!='Total'
           &datos_se[,2]!='No consta')
  return(z)}


# Edad
# Programa
url.edad<-'https://www.ine.es/jaxiT3/files/t/es/px/8116.px?nocab=1'
# lectura de datos
flujos_ed<-read.px(url.edad)
datos_ed<-flujos_ed$DATA$value
# Recodificacion
levels(datos_ed[,2])<-levels(datos_ed[,3])<-c('Total', 'Ocupados', 'Parados', 'Inactivos', 'No consta')
names(datos_ed)<-c('Periodo','Trimestre Anterior','Trimestre actual','Edad','Valor (Miles de Personas)')
levels(datos_ed$Edad)<-c('+16','16-24','25-34','35-44','45-54','55-64','+64')

filtro_ed<-function(datasetinput,fecha){
  edad<-as.character(datasetinput)
  z <-datos_ed%>%
    select(names(datos_ed)[c(2,3,5)])%>%
    filter(datos_ed[,1]==fecha&datos_ed[,4]==edad&datos_ed[,2]!='Total'&datos_ed[,3]!='Total'
           &datos_ed[,2]!='No consta')
  z$`Valor (Miles de Personas)`[which(is.na(z$`Valor (Miles de Personas)`))]<-0
  return(z)}
# Programa
url.nforma<-'https://www.ine.es/jaxiT3/files/t/es/px/8280.px?nocab=1'

# lectura de datos
flujos_nf<-read.px(url.nforma)
datos_nf<-flujos_nf$DATA$value

# Recodificacion
levels(datos_nf[,2])<-levels(datos_nf[,3])<-c('Total', 'Ocupados', 'Parados', 'Inactivos', 'No consta')
names(datos_nf)<-c('Periodo','Trimestre Anterior','Trimestre actual','nforma','Valor (Miles de personas)')

levels(datos_nf$nforma)<-c('Total','Primaria o menos','Primera etapa secundaria','Segunda etapa secundaria','Educacion Superior')
# funcion para añadir 
filtro_nf<-function(datasetinput,fecha){
  nforma<-as.character(datasetinput)
  z <-datos_nf%>%
    select(names(datos_nf)[c(2,3,5)])%>%
    filter(datos_nf[,1]==fecha&datos_nf[,4]==nforma &datos_nf[,2]!='Total'&datos_nf[,3]!='Total'
           &datos_nf[,2]!='No consta')
  return(z)}




ui<-fluidPage(
  # Titulo
  titlePanel('Flujos del mercado laboral'),
  
  
  tabsetPanel(
tabPanel('Sexo',
  # layout
  sidebarLayout(
    # Panel de inputs
    sidebarPanel(

      sliderTextInput('fecha1',label = 'Fecha de flujos',choices = levels(datos_se$Periodo)[length(levels(datos_se$Periodo)):1]
                      ,selected = levels(datos_se$Periodo)[length(levels(datos_se$Periodo))]),
      # Input
      radioButtons(inputId = 'sex',choiceNames = levels(datos_se$Sexo), 
                   choiceValues = levels(datos_se$Sexo),label = 'Sexos:'),
    ),
    mainPanel(
      fluidRow(
        column(8, align='center',      
               plotOutput('fluplot', width = '500',height = '500'),
               tableOutput('tabla'))
      )))),


tabPanel("Edad",
  # layout
  sidebarLayout(
    # Panel de inputs
    sidebarPanel(
      sliderTextInput('fecha2',label = 'Fecha de flujos',choices = levels(datos_ed$Periodo)[length(levels(datos_ed$Periodo)):1]
                      ,selected = levels(datos_ed$Periodo)[length(levels(datos_ed$Periodo))]),
      # Input
      radioButtons(inputId = 'edad',choiceNames = levels(datos_ed$Edad), 
                   choiceValues = levels(datos_ed$Edad),label = 'Grupos edad:'),
    ),
    mainPanel(
      fluidRow(
        column(8, align='center',      
               plotOutput('fluplot2', width = '500',height = '500'),
               tableOutput('tabla2'))
      )))),
tabPanel("Maximo nivel formativo",

  # layout
  sidebarLayout(
    # Panel de inputs
    sidebarPanel(
      sliderTextInput('fecha3',label = 'Fecha de flujos',choices = levels(datos_nf$Periodo)[length(levels(datos_nf$Periodo)):1]
                      ,selected = levels(datos_nf$Periodo)[length(levels(datos_nf$Periodo))]),
      # Input
      radioButtons(inputId = 'nforma',choiceNames = levels(datos_nf$nforma), 
                   choiceValues = levels(datos_nf$nforma),label = 'Grupos formativos: '),
    ),
    mainPanel(
      fluidRow(
        column(8, plotOutput('fluplot3', width = '500',height = '500'),
               tableOutput('tabla3')
        )))))))

server<-function(input,output){
  fechainput1<-renderText({input$fecha1})
  datasetinput1<-renderText({input$sex})
  output$tabla<-renderTable(
    z<-filtro_se(datasetinput1(),fechainput1())
  )
  output$fluplot<-renderPlot(
    chordDiagram(filtro_se(datasetinput1(),fechainput1()), c('Green','Orange','Gray'),
                 transparency = .25,annotationTrack = c("name", "grid"),
                 directional = 1,order = c('Ocupados','Parados','Inactivos'))
    ,res=125)
  
  fechainput2<-renderText({input$fecha2})
  datasetinput2<-reactive({input$edad})
  # output$opcion1<-renderText({input$sex})
  output$tabla2<-renderTable(
    z<-filtro_ed(datasetinput2(),fechainput2())
  )
  output$fluplot2<-renderPlot(
    chordDiagram(filtro_ed(datasetinput2(),fechainput2()), c('Green','Orange','Gray'),
                 transparency = .25,annotationTrack = c("name", "grid"),
                 directional = 1,order = c('Ocupados','Parados','Inactivos')),
    res = 125  )
  
  fechainput3<-renderText({input$fecha3})
  datasetinput3<-reactive({input$nforma})
  # output$opcion1<-renderText({input$sex})
  output$tabla3<-renderTable(
    z<-filtro_nf(datasetinput3(),fechainput3())
  )
  output$fluplot3<-renderPlot(
    chordDiagram(filtro_nf(datasetinput3(),fechainput3()), c('Green','Orange','Gray'),
                 transparency = .25,annotationTrack = c("name", "grid"),
                 directional = 1,order = c('Ocupados','Parados','Inactivos')),
    res = 125)
  
  
  
  
  
  
}

shinyApp(ui, server)


