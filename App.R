
require(ggplot2)
require(shiny)
require(plotly)
require(wesanderson)
require(sas7bdat)
require(haven)
require( shinydashboard)

load("DatosApp.Rdata")

fechas<-paste(rep(2005:2019,4),'T',1:4,sep="")
tnames<-sort(fechas)
index<-read_sas('index.sas7bdat')

index<-cbind(index, fecha=c(tnames,'2020T1'))
names(index)
indexnames<-names(index)[grep(names(index),pattern = 'i')]
graficos<-c('Tasas de paro', 'Formacion', 'Abandono estudios')
glabels<-c(  'Juvenil 16-24 años', 'Mayores 55-64',
             'Estudios superiores 30-34','Adultos en formación 25-64',
              'Abandono temprano 18-24','Ni estudian ni trabajan 20-34')
tecnicas<-c("Desestacionalización","Auto-ARIMA","Predicción")
nombres<-c('Tasa de paro juvenil ','Tasa de paro trabajadores mayores',
           'Jóvenes con estudios superiores', 'Adultos en formación continua',
           'Abandono temprado de la educación', 'Jovenes que ni estudian ni trabajan')
indexnames<-names(index)[grep(names(index),pattern = "i")];
graficos<-c("Tasas de paro", "Formacion",'Formacion', "Abandono estudios");
titulo<-c("Tasas de Paro","Jóvenes con estudios superiores","Adultos en formación","Abandono de estudios");
glabels<-c("Juvenil 16-24", "Mayores 55-64",
           "Estudios superiores 30-34","Adultos en formación 25-64",
           "Abandono temprano 20-24","Ni estudian ni trabajan 16-24");
subt<-c('% Sobre el total del grupo de edad, y de la población activa', '% Sobre el total del grupo de edad', '% Sobre el total del grupo de edad','% Sobre el total del grupo de edad')

rep_tendencia<-function(datos){
  
  entero<-ts(datos,start = 2005,frequency = 4)
  descomp<-stl(entero,4)
  ajus<-seasadj(descomp)
  p<-autoplot(ajus)
  ggplotly(p)
}

save(list = ls(),file = "DatosApp1.Rdata")


ui <- fluidPage(  

  dashboardPage(  
    dashboardHeader(title = "Aplicacones shiny sobre la EPA"),
    dashboardSidebar(
      selectInput("var_y", "Series temporales de algunos indices y tasas sobre el empleo", choices = titulo),
      checkboxGroupInput("var_tec","Técnica a utilizar:",tecnicas)
    ),
  dashboardBody(
#ayuda para la amplitud de la pagina https://stackoverflow.com/questions/47784427/shiny-how-to-center-and-fix-width-of-dashboard
      tags$style(
        "body{
    min-height: auto;
    height: auto;
    max-width: 1600px;
    margin: auto;
        }"
      )
    ,
    plotlyOutput("distPlot"),
    
splitLayout(
    plotlyOutput("detrend1"),plotlyOutput("detrend2")),
splitLayout(
    plotOutput("autoarima1"),plotOutput("autoarima2")),
splitLayout(
    plotlyOutput("pred1"),plotlyOutput("pred2"))
    
    
    )))


shinyApp(ui,server)
server <- function(input, output) {

  
  output$distPlot <- renderPlotly({
    req(input$var_y)
    titulo_ind<-which(titulo==input$var_y)
    rango<-switch(titulo_ind,1,3,4,5)
    ind1<-indexnames[rango]
    ind2<-indexnames[rango+1]
    auxlab1<-which(indexnames==ind1)
    
    auxlab2<-which(indexnames==ind2)
    if(titulo_ind==2 | titulo_ind==3){
      p<-ggplot(data = index,aes(fecha))+
        geom_line(aes_(y= index[[ind1]]*100,group=1,colour=glabels[auxlab1]),size=.75)+
        geom_point(aes_(y= index[[ind1]]*100,group=1,colour=glabels[auxlab1]),size=1)+
        scale_x_discrete(breaks= index$fecha[ grepl(".T1",x = index$fecha)],labels = 2005:2020)+
        theme_light()+
        theme(axis.text.x = element_text(angle = 90,size = 12 ,vjust = .5),
              legend.background = element_rect(linetype = 1,size = .25,colour = 1),
              axis.text = element_text(colour = 1,size = 12),
              legend.position = "right",
              panel.border = element_rect(colour = "black",fill = NA))+
        labs(caption=" ",subtitle = " ",x="",y="")+      
        scale_color_manual(" ",values =wes_palette(name = "Darjeeling1",n = 2,type = "dis"))
      pplotly<-ggplotly(p)
      pplotly<-pplotly%>%layout(title=(text=paste0("<b>",titulo[titulo_ind],"</b><br><sup>",subt[titulo_ind])),
                                legend=list(title=list(text=paste0("<b>",graficos[2],"</b>"))),
                                xaxis=list( title = "<sup>Fuente: INE y Eurostat</b>"), margin=list(l=50,r=50,t=50,b=100))
      pplotly$x$data[[1]]$text<-gsub(pplotly$x$data[[1]]$text,pattern = "colour", replacement = "Serie")
      pplotly$x$data[[1]]$text<-gsub(pplotly$x$data[[1]]$text,pattern = "y", replacement = "Valor indice")
      pplotly$x$data[[1]]$text<-gsub(pplotly$x$data[[1]]$text,pattern = "fecha", replacement = "Fecha")
      
    }else{
      p<-ggplot(data = index,aes(fecha))+
        geom_line(aes_(y= index[[ind1]]*100,group=1,colour=glabels[auxlab1]),size=.75)+
        geom_point(aes_(y= index[[ind1]]*100,group=1,colour=glabels[auxlab1]),size=1)+
        geom_line(aes_(y= index[[ind2]]*100, group=2,colour=glabels[auxlab2]),size=.75)+
        geom_point( aes_(y=index[[ ind2]]*100, group=2,colour=glabels[auxlab2]),size=1)+
        scale_x_discrete(breaks= index$fecha[ grepl(".T1",x = index$fecha)],labels = 2005:2020)+
        theme_light()+
        theme(axis.text.x = element_text(angle = 90,size = 12 ,vjust = .5),
              legend.background = element_rect(linetype = 1,size = .25,colour = 1),
              axis.text = element_text(colour = 1,size = 12),
              legend.position = "right",
              panel.border = element_rect(colour = "black",fill = NA))+
        labs(caption=" ",subtitle = " ",x="",y="")+      
        scale_color_manual(" ",values =wes_palette(name = "Darjeeling1",n = 2,type = "dis"))
      pplotly<-ggplotly(p)
      pplotly<-pplotly%>%layout(title=(text=paste0("<b>",titulo[titulo_ind],"</b><br><sup>",subt[titulo_ind])),
                                legend=list(title=list(text=paste0("<b>",graficos[titulo_ind],"</b>"))),
                                xaxis=list( title = "<sup>Fuente: INE y Eurostat</b>"), margin=list(l=50,r=50,t=50,b=100))
      pplotly$x$data[[1]]$text<-gsub(pplotly$x$data[[1]]$text,pattern = "colour", replacement = "Serie")
      pplotly$x$data[[1]]$text<-gsub(pplotly$x$data[[1]]$text,pattern = "y", replacement = "Valor indice")
      pplotly$x$data[[1]]$text<-gsub(pplotly$x$data[[1]]$text,pattern = "fecha", replacement = "Fecha")
      pplotly$x$data[[2]]$text<-gsub(pplotly$x$data[[2]]$text,pattern = "colour", replacement = "Serie")
      pplotly$x$data[[2]]$text<-gsub(pplotly$x$data[[2]]$text,pattern = "y:", replacement = "Valor indice:")
      pplotly$x$data[[2]]$text<-gsub(pplotly$x$data[[2]]$text,pattern = "fecha", replacement = "Fecha")
    }
    pplotly
  })

  
  output$detrend1<-renderPlotly({
    req(input$var_y)
    if (tecnicas[1]%in%input$var_tec) {
      titulo_ind<-which(titulo==input$var_y)
      rango<-switch(titulo_ind,1,2,4,5)
      ind1<-indexnames[rango]
      ind2<-indexnames[rango+1]
      auxlab1<-which(indexnames==ind1)
      

      entero<-ts(index[ind1][,1],start = 2005,frequency = 4)
      descomp<-stl(entero,4)
      ajus<-seasadj(descomp)

        p<-ggplotly(autoplot(ajus,xlab='',ylab='',main= ""))
        p%>%layout(title=(text=paste0("<b>",nombres[auxlab1],"</b><br><sup>")),margin=list(
          l = 50,r = 50,  b = 100,t = 80,pad = 4))
      }
      })
  output$detrend2<-renderPlotly({
    req(input$var_y)
    if (tecnicas[1]%in%input$var_tec & input$var_y%in%titulo[c(1,4)]) {
      titulo_ind<-which(titulo==input$var_y)
      rango<-switch(titulo_ind,1,2,4,5)
      ind1<-indexnames[rango]
      ind2<-indexnames[rango+1]
      auxlab1<-which(indexnames==ind1)
      auxlab2<-which(indexnames==ind2)
      
      
      entero<-ts(index[ind2][,1],start = 2005,frequency = 4)
      descomp<-stl(entero,4)
      ajus<-seasadj(descomp)
      
      p<-ggplotly(autoplot(ajus,xlab='',ylab='',main= ""))
      p%>%layout(title=(text=paste0("<b>",nombres[auxlab2],"</b><br><sup>")),margin=list(
        l = 50,r = 50,  b = 100,t = 80,pad = 4))
    }
  })
  
  
  output$autoarima1<-renderPlot({
    req(input$var_y)
    if (tecnicas[2]%in%input$var_tec) {
      titulo_ind<-which(titulo==input$var_y)
      rango<-switch(titulo_ind,1,2,4,5)
      ind1<-indexnames[rango]
      ind2<-indexnames[rango+1]
      
      entero<-ts(index[ind2][,1],start = 2005,frequency = 4)
      recor<-ts(entero[time(entero)<2020],start = 2005, frequency = 4)
      au<-auto.arima(recor)
      checkresiduals(au)
    }
  })
  output$autoarima2<-renderPlot({
    req(input$var_y)
    if (tecnicas[2]%in%input$var_tec & input$var_y%in%titulo[c(1,4)]) {
      titulo_ind<-which(titulo==input$var_y)
      rango<-switch(titulo_ind,1,2,4,5)
      ind1<-indexnames[rango]
      ind2<-indexnames[rango+1]
      
      
      entero<-ts(index[ind2][,1],start = 2005,frequency = 4)
      recor<-ts(entero[time(entero)<2020],start = 2005, frequency = 4)
      au<-auto.arima(recor)
      checkresiduals(au)
    }
  })
  
  output$pred1<-renderPlotly({
    req(input$var_y)
    if (tecnicas[3]%in%input$var_tec) {
      titulo_ind<-which(titulo==input$var_y)
      rango<-switch(titulo_ind,1,2,4,5)
      ind1<-indexnames[rango]
      ind2<-indexnames[rango+1]
      
      entero<-ts(index[ind1][,1],start = 2005,frequency = 4)
      recor<-ts(entero[time(entero)<2020],start = 2005, frequency = 4)
      au<-auto.arima(recor)
      
      z<-au%>%forecast(h=4)
      tz<-ts(cbind(as.data.frame(z)),start = 2020,frequency = 4)
      grf<-autoplot(entero, ylab = '')+
      autolayer(tz)
      ggplotly(grf)

    }
  })
  output$pred2<-renderPlotly({
    req(input$var_y)
    if (tecnicas[3]%in%input$var_tec & input$var_y%in%titulo[c(1,4)]) {
      titulo_ind<-which(titulo==input$var_y)
      rango<-switch(titulo_ind,1,2,4,5)
      ind1<-indexnames[rango]
      ind2<-indexnames[rango+1]
      
      entero<-ts(index[ind2][,1],start = 2005,frequency = 4)
      recor<-ts(entero[time(entero)<2020],start = 2005, frequency = 4)
      au<-auto.arima(recor)
      
      z<-au%>%forecast(h=4)
      tz<-ts(cbind(as.data.frame(z)),start = 2020,frequency = 4)
      grf<-autoplot(entero, ylab = '')+
        autolayer(tz)
      ggplotly(grf)

    }
  })
  
  
  
}
shinyApp(ui = ui,server = server)












# 
# 
# 
# rep_tendencia(index$itpj)
# 
#   titulo_ind<-which(titulo==input$var_y)
#   rango<-switch(titulo_ind,1,2,4,5)
#   ind1<-indexnames[rango]
#   ind2<-indexnames[rango+1]
#   
# # 
# 
# 
# 
# 
# 
# 
# 
# # modelizamos respecto 2019
# 
# entero<-ts(as.data.frame(index$itpj,colnames='Real'),start = 2005,frequency = 4)
# recor<-ts(entero[time(entero)<2020],start = 2005, frequency = 4)
# 
# # Quitar tendencia, entero
# descomp<-stl(recor,12)
# ts.sea<-seasadj(ts.stl)
# rep_tendencia<-function(datos){
#   entero<-ts(datos,start = 2005,frequency = 4)
#   descomp<-stl(entero,12)
#   ajus<-seasadj(descomp)
#   autoplot(ajus)
# }
# 
# ajaj<-rep_tendencia(index$itpj)
# # Arima presentado como validación, recortado
# 
# checkresiduals(auto.arima(entero))
# 
# 
# # Representacion con plotly
# 
# 
# ggplotly(ajaj)
# z<-modelo%>%forecast(h=4)
# tz<-ts(cbind(as.data.frame(z)),start = 2020,frequency = 4)
# grf<-autoplot(entero, ylab = '')+
#   autolayer(tz)
# ggplotly(grf)
# 
# # 
# 
# # 
# # 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# setwd("C:/Users/GenderIsmael/Desktop/TFG/Datos/Libreria")
# 
# # La de verdad, la buena.
# 
# # R Importar los Indices calculados en sas y visualización con shiny, y plotly , a ser posible que se encuentren
# #  en el mismo directorio
# library(wesanderson)
# require(sas7bdat)
# require(haven)
# require(ggplot2)
# require(shiny)
# library(plotly)
# library(astsa)
# library(forecast)
# 
# tnames<-sort(paste(rep(2005:2019,4),'T',1:4,sep=""))
# index<-read_sas('index.sas7bdat')
# index<-cbind(index, tnames)
# 
# indexnames<-names(index)[grep(names(index),pattern = "i")];
# graficos<-c("Tasas de paro", "Formacion",'Formacion', "Abandono estudios");
# titulo<-c("Tasas de Paro","Jóvenes con estudios superiores","Adultos en formación","Abandono de estudios");
# glabels<-c("Juvenil 16-24", "Mayores 55-64",
#            "Estudios superiores 30-34","Adultos en formación 25-64",
#            "Abandono temprano 20-24","Ni estudian ni trabajan 16-24");
# subt<-c('% Sobre el total del grupo de edad, y de la población activa', '% Sobre el total del grupo de edad', '% Sobre el total del grupo de edad','% Sobre el total del grupo de edad')
# ui <- fluidPage(
#   selectInput("var_y", "Series temporales de algunos indices y tasas sobre el empleo", choices = titulo),
#   plotlyOutput("distPlot"))
# indexnames
# server <- function(input, output) {
#   output$distPlot <- renderPlotly({
#     req(input$var_y)
#     titulo_ind<-which(titulo==input$var_y)
#     rango<-switch(titulo_ind,1,2,4,5)
#     ind1<-indexnames[rango]
#     ind2<-indexnames[rango+1]
#     if(titulo_ind==2 | titulo_ind==3){
#       p<-ggplot(data = index,aes(tnames))+
#         geom_line(aes_(y= index[[ind1]]*100,group=1,colour=glabels[titulo_ind+1]),size=.75)+
#         geom_point(aes_(y= index[[ind1]]*100,group=1,colour=glabels[titulo_ind+1]),size=1)+
#         scale_x_discrete(breaks= index$tnames[ grepl(".T1",x = index$tnames)],labels = 2005:2019)+
#         theme_light()+
#         theme(axis.text.x = element_text(angle = 90,size = 12 ,vjust = .5),
#               legend.background = element_rect(linetype = 1,size = .25,colour = 1),
#               axis.text = element_text(colour = 1,size = 12),
#               legend.position = "right",
#               panel.border = element_rect(colour = "black",fill = NA))+
#         labs(caption=" ",subtitle = " ",x="",y="")+      
#         scale_color_manual(" ",values =wes_palette(name = "Darjeeling1",n = 2,type = "dis"))
#       pplotly<-ggplotly(p)
#       pplotly<-pplotly%>%layout(title=(text=paste0("<b>",titulo[titulo_ind],"</b><br><sup>",subt[titulo_ind])),
#                                 legend=list(title=list(text=paste0("<b>",graficos[2],"</b>"))),
#                                 xaxis=list( title = "<sup>Fuente: INE y Eurostat</b>"), margin=list(l=50,r=50,t=50,b=100))
#       pplotly$x$data[[1]]$text<-gsub(pplotly$x$data[[1]]$text,pattern = "colour", replacement = "Serie")
#       pplotly$x$data[[1]]$text<-gsub(pplotly$x$data[[1]]$text,pattern = "y", replacement = "Valor indice")
#       pplotly$x$data[[1]]$text<-gsub(pplotly$x$data[[1]]$text,pattern = "tnames", replacement = "Fecha")
# 
#     }else{
#     p<-ggplot(data = index,aes(tnames))+
#       geom_line(aes_(y= index[[ind1]]*100,group=1,colour=glabels[titulo_ind]),size=.75)+
#       geom_point(aes_(y= index[[ind1]]*100,group=1,colour=glabels[titulo_ind]),size=1)+
#       geom_line(aes_(y= index[[ind2]]*100, group=2,colour=glabels[titulo_ind+1]),size=.75)+
#       geom_point( aes_(y=index[[ ind2]]*100, group=2,colour=glabels[titulo_ind+1]),size=1)+
#       scale_x_discrete(breaks= index$tnames[ grepl(".T1",x = index$tnames)],labels = 2005:2019)+
#       theme_light()+
#       theme(axis.text.x = element_text(angle = 90,size = 12 ,vjust = .5),
#             legend.background = element_rect(linetype = 1,size = .25,colour = 1),
#             axis.text = element_text(colour = 1,size = 12),
#             legend.position = "right",
#             panel.border = element_rect(colour = "black",fill = NA))+
#       labs(caption=" ",subtitle = " ",x="",y="")+      
#       scale_color_manual(" ",values =wes_palette(name = "Darjeeling1",n = 2,type = "dis"))
#     pplotly<-ggplotly(p)
#     pplotly<-pplotly%>%layout(title=(text=paste0("<b>",titulo[titulo_ind],"</b><br><sup>",subt[titulo_ind])),
#                               legend=list(title=list(text=paste0("<b>",graficos[titulo_ind],"</b>"))),
#                               xaxis=list( title = "<sup>Fuente: INE y Eurostat</b>"), margin=list(l=50,r=50,t=50,b=100))
#     pplotly$x$data[[1]]$text<-gsub(pplotly$x$data[[1]]$text,pattern = "colour", replacement = "Serie")
#     pplotly$x$data[[1]]$text<-gsub(pplotly$x$data[[1]]$text,pattern = "y", replacement = "Valor indice")
#     pplotly$x$data[[1]]$text<-gsub(pplotly$x$data[[1]]$text,pattern = "tnames", replacement = "Fecha")
#     pplotly$x$data[[2]]$text<-gsub(pplotly$x$data[[2]]$text,pattern = "colour", replacement = "Serie")
#     pplotly$x$data[[2]]$text<-gsub(pplotly$x$data[[2]]$text,pattern = "y:", replacement = "Valor indice:")
#     pplotly$x$data[[2]]$text<-gsub(pplotly$x$data[[2]]$text,pattern = "tnames", replacement = "Fecha")
#     }
#     pplotly
#   })
# }
# shinyApp(ui = ui,server = server)
# 
# # 
# 
# 
# # 
#  # 
# 
# 
# # 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # Pasaporte con mi nombre rumbo al infinito
# # Hay mucho mito, pero nadie ha vuelto y lo ha descrito
# 
# 
# ui <- fluidPage(
#   selectInput("var_y", "Series temporales de indices sobre el empleo", choices = graficos),
#   plotlyOutput("indPlot")
#   
# )
# 
# 
# 
# 
# server <- function(input, output) {
#   
#   
#   # reactopt<-reactive({input$var_y})
#   
#   output$indplot <- renderPlotly({
#     req(input$var_y)
#     rango<-which(graficos==input$var_y)*2-1
#     ind1<-indexnames[rango]
#     ind2<-indexnames[rango+1]
#     
#     p<-ggplot(data = index,aes(tnames))+
#       geom_line(aes_(y= index[[ind1]]*100,group=1,colour=glabels[rango]),size=.75)+
#       geom_point(aes_(y= index[[ind1]]*100,group=1,colour=glabels[rango]),size=1)+
#       geom_line(aes_(y= index[[ind2]]*100, group=2,colour=glabels[rango+1]),size=.75)+
#       geom_point( aes_(y=index[[ ind2]]*100, group=2,colour=glabels[rango+1]),size=1)+
#       scale_x_discrete(breaks= index$tnames[ grepl(".T1",x = index$tnames)],labels = 2005:2019)+
#       theme_light()+
#       theme(axis.text.x = element_text(angle = 90,size = 12 ,vjust = .5),
#             legend.background = element_rect(linetype = 1,size = .25,colour = 1),
#             axis.text = element_text(colour = 1,size = 12),
#             legend.position = 'right',
#             panel.border = element_rect(colour = 'black',fill = NA))+
#       labs(caption=" ",subtitle = ' ',x='',y='')+      
#       scale_color_manual(' ',values =wes_palette(name = 'Darjeeling1',n = 2,type = 'dis'))
#     
#     pplotly<-ggplotly(p)
#     
#     pplotly<-pplotly%>%layout(title=(text=paste0('<b>','Series temporales de indices sobre el empleo','</b><br><sup> % Sobre el total del grupo de edad de 16 a 25 años')),
#                               legend=list(title=list(text=paste0('<b>',graficos[which(graficos==input$var_y)],'</b>'))),
#                               xaxis=list( title = '<sup>Fuente: INE y Eurostat</b>'), margin=list(l=50,r=50,t=50,b=100))
#     
#     pplotly$x$data[[1]]$text<-gsub(pplotly$x$data[[1]]$text,pattern = 'colour', replacement = 'Serie')
#     pplotly$x$data[[1]]$text<-gsub(pplotly$x$data[[1]]$text,pattern = 'y', replacement = 'Valor indice')
#     pplotly$x$data[[1]]$text<-gsub(pplotly$x$data[[1]]$text,pattern = 'tnames', replacement = 'Fecha')
#     pplotly$x$data[[2]]$text<-gsub(pplotly$x$data[[2]]$text,pattern = 'colour', replacement = 'Serie')
#     pplotly$x$data[[2]]$text<-gsub(pplotly$x$data[[2]]$text,pattern = 'y:', replacement = 'Valor indice:')
#     pplotly$x$data[[2]]$text<-gsub(pplotly$x$data[[2]]$text,pattern = 'tnames', replacement = 'Fecha')
#     
#     pplotly
#   })
#   
#   
# }
# shinyApp(ui = ui,server = server)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# indexnames
# pure<-ggplot(data = index,aes(tnames))+
#   geom_line(aes_(y= index$itpj,group=1,colour=glabels[1]),size=.75)+
#   geom_point(aes_(y= index$itpj,group=1,colour=glabels[1]),size=1)+
#   geom_line(aes_(y=  index$iurow, group=2,colour=glabels[1+1]),size=.75)+
#   geom_point( aes_(y= index$iurow, group=2,colour=glabels[1+1]),size=1)+
#   labs(col='Series')
# 
# pure
# 
# ptly<-ggplotly(pure) 
# 
# 
# ptly$x$layout$legend
# 
# 
# ptly%>%style(text(paste0('Fecha',index$tnames,
#                          '</br></br>','Indice: ',
#                          r$x$data[[1]]$y
#                          
#                          )))
# # 
# 
# ptly$x$data
# 
# 
# r <- ggplotly(pure) %>% 
#   plotly::config(displayModeBar = T) %>%
#   layout(legend = list(orientation = "h", x = 0.4, y = -0.2))
# 
# 
# 
# style(r,text = paste0("Fecha: ",index$tnames, '</br></br> Indice: ', r$x$data[[1]]$y))
# r$x$data[[1]]$text
# 
# 
# r$x$data[[1]]$text<-gsub(r$x$data[[1]]$text,pattern = 'colour', replacement = 'Serie')
# r$x$data[[1]]$text<-gsub(r$x$data[[1]]$text,pattern = 'y', replacement = 'Valor indice')
# r$x$data[[1]]$text<-gsub(r$x$data[[1]]$text,pattern = 'tnames', replacement = 'Fecha')
# 
# r$x$data[[2]]$text<-gsub(r$x$data[[1]]$text,pattern = 'colour', replacement = 'Serie')
# r$x$data[[2]]$text<-gsub(r$x$data[[1]]$text,pattern = 'y', replacement = 'Valor indice')
# r$x$data[[2]]$text<-gsub(r$x$data[[1]]$text,pattern = 'tnames', replacement = 'Fecha')
# 
# 
# r
# 
# style(p = ptly,traces = 1, text(paste0('Fecha:', r$x$data[[1]]$y)))
# 
# 
# r
# class(r)
# 
# pure<-ggplot(data = index,aes(tnames))+
#   geom_line(aes_(y= index$itpj,group=1,colour=glabels[1]),size=.75)+
# 
#   r %>% style(text = paste0("Date:",index$tnames))
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ui <- fluidPage(
#   selectInput("var_y", "Eje Y", choices = graficos),
#   plotOutput("indplot", hover = "plot_hover", hoverDelay = 0),
#   uiOutput("dynamic")
#   
# )
# 
# index$tnames
# 
# 
# server <- function(input, output) {
#   
#    # reactrank<-reactive({input$var_y})
#   
#   
#   output$distPlot <- renderPlot({
#     req(input$var_y)
#     rango<-which(graficos==input$var_y)*2-1
#     ind1<-indexnames[rango]
#     ind2<-indexnames[rango+1]
#     
#     ggplot(data = index,aes(tnames))+
#       geom_line(aes_(y= index[[ind1]],group=1,colour='blue'),size=1)+
#       geom_point(aes_(y= index[[ind1]],group=1,colour='blue'),size=2)+
#       geom_line(aes_(y= index[[ind2]], group=2,colour='red'),size=1)+
#       geom_point( aes_(y=index[[ ind2]], group=2,colour='red'),size=2)+
#       # scale_color_discrete(name=output$var_y, labels=(c('Jovenes','Mayores')))+
#       scale_x_discrete(breaks= index$tnames[ grepl(".T1",x = index$tnames)],labels = 2005:2019)+
#       theme(
#         panel.grid.minor =    element_blank(),
#         axis.text.x = element_text(angle = 90,size = 12 ,vjust = .5))
#     # labs(title = "Serie Temporal de la Tasa De Paro Juvenil ",
#     #      caption="Fuente: INE y Eurostat",
#     #      y='Tasa de paro juvenil',
#     #      subtitle = '% sobre el total del grupo de edad de 16 a 25 años',
#     #      x='')
#     
#     
#     
#     
#     
#   })
#   
#   output$dynamic <- renderUI({
#     req(input$plot_hover) 
#     verbatimTextOutput("vals")
#   })
#   
#   output$vals <- renderPrint({
#     hover <- input$plot_hover 
#     # print(str(hover)) # list
#     y <- nearPoints(index, input$plot_hover)[index[[ind1]]]
#     req(nrow(y) != 0)
#     y
#   })
#   
# }
# shinyApp(ui = ui, server = server)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# library(plotly)
# 
# 
# library(plotly)
# ploty
# 
# 
# ?plotlyOutput
# ui <- fluidPage(
#   selectInput("var_y", "Eje Y", choices = graficos),
#   plotlyOutput("distPlot"),
# 
#   
# )
# 
# index$tnames
# 
# 
# server <- function(input, output) {
#   
#   # reactrank<-reactive({input$var_y})
#   
#   
#   output$distPlot <- renderPlotly({
#     req(input$var_y)
#     rango<-which(graficos==input$var_y)*2-1
#     ind1<-indexnames[rango]
#     ind2<-indexnames[rango+1]
#     
#     ggplot(data = index,aes(tnames))+
#       geom_line(aes_(y= index[[ind1]],group=1,colour='blue'),size=1)+
#       geom_point(aes_(y= index[[ind1]],group=1,colour='blue'),size=2)+
#       geom_line(aes_(y= index[[ind2]], group=2,colour='red'),size=1)+
#       geom_point( aes_(y=index[[ ind2]], group=2,colour='red'),size=2)+
#       # scale_color_discrete(name=output$var_y, labels=(c('Jovenes','Mayores')))+
#       scale_x_discrete(breaks= index$tnames[ grepl(".T1",x = index$tnames)],labels = 2005:2019)+
#       theme(
#         panel.grid.minor =    element_blank(),
#         axis.text.x = element_text(angle = 90,size = 12 ,vjust = .5))
#     # labs(title = "Serie Temporal de la Tasa De Paro Juvenil ",
#     #      caption="Fuente: INE y Eurostat",
#     #      y='Tasa de paro juvenil',
#     #      subtitle = '% sobre el total del grupo de edad de 16 a 25 años',
#     #      x='')
#     
#     
#     
# 
# 
#   })
#   # 
#   # output$dynamic <- renderUI({
#   #   req(input$plot_hover) 
#   #   verbatimTextOutput("vals")
#   # })
#   # 
#   # 
#   # output$vals <- renderPrint({
#   #   rango<-which(graficos==input$var_y)*2-1
#   #   ind1<-indexnames[rango]
#   #   ind2<-indexnames[rango+1]
#   #   hover <- input$plot_hover 
#   #   # print(str(hover)) # list
#   #   y <- nearPoints(index, input$plot_hover)['itpj']
#   #   req(nrow(y) != 0)
#   #   y
#   #   # y
#   # })
#   
# }
# shinyApp(ui = ui, server = server)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ui <- fluidPage(
#   selectInput("var_y", "Y-Axis", choices = names(iris)),
#   plotOutput("distPlot", hover = "plot_hover", hoverDelay = 0),
#   uiOutput("dynamic")
#   
# )
# 
# server <- function(input, output) {
#   
#   output$distPlot <- renderPlot({
#     req(input$var_y)
#     ggplot(iris, aes_string("Sepal.Width", input$var_y)) + 
#       geom_point()
#   })
#   
#   output$dynamic <- renderUI({
#     req(input$plot_hover) 
#     verbatimTextOutput("vals")
#   })
#   
#   output$vals <- renderPrint({
#     hover <- input$plot_hover 
#     # print(str(hover)) 
#     y <- nearPoints(iris, input$plot_hover)[input$var_y]
#     req(nrow(y) != 0)
#     y
#   })
#   
# }
# shinyApp(ui = ui, server = server)
