This is a weekly team project for Theoretical Statistics II.
This is a code for R shiny application calculating life insurance and annuity insurance premium with life table

Input: insurance type(life/annuity), life table, sex, age, annual interest rate, payment expiration, pay expiration, benefit 
Output: premium



library(shiny)
library(shinydashboard)
library(tidyverse)
library(scales)
options(scipen=999)

header <- dashboardHeader(title='Insuarance Premium')

sidebar <- dashboardSidebar(
  
  sidebarMenu(id='type',
              menuItem('Life insuarance', tabName = 'life', icon=icon('heartbeat', verify_fa = FALSE)),
              menuItem('Annuity insuarance', tabName = 'annuity', icon=icon('money-check', verify_fa = FALSE))),
  
  fileInput('file1', 'Upload CSV File',
            multiple = FALSE,
            accept = c('text/csv',
                       'text/comma-separated-values,text/plain',
                       '.csv'))
  
)

body <- dashboardBody(
  fluidRow(
    valueBoxOutput('px')),

  fluidRow(
    box(title = 'Input',
        radioButtons('sex', label = h3('Sex'),
                     choices = list('Male'='Male', 'Female'='Female'), selected='Female'),
        sliderInput('x', h3('Age'), min = 0, max = 100, value=30, step=1),
        numericInput('i', h3('Annual Interst Rate'), value=0.05),
        numericInput('n', h3('Payment Expiration'), value=35),
        numericInput('m', h3('Pay Expiration'), value=35),
        numericInput('b', h3('Benefit'), value=10000), width = 6, solidHeader = T, status = 'primary'),
    
    box(title = 'life-table', tableOutput('df'), width = 6, status = 'danger', solidHeader = T)
   
  ))

ui <- dashboardPage(header, sidebar, body, skin='purple')


## function
pv.life <- function(data, x, i, sex, n, m, b){
  
  r <- log(1+i)
  if (sex=='Male'){
    lx <- data$lx_m
  } else{
    lx <- data$lx_f
  }
  a <- c()
  for (j in 1:m){
    a[j] <- (lx[x+1+j]/lx[x+1])*exp(-r*j)
    a.sum <- sum(a)
  }
  Ax.bar <- 1-r*((1/2)+a.sum+(lx[x+1+m]/lx[x+1])*exp(-r*(m+1))/(1-exp(-r)))
  
  c <- c()
  for (j in 1:n-1){
    c[j]<-(lx[x+1+j]/lx[x+1])*exp(-r*j)
    c.sum<-sum(c)
  }
  ax<-(1/2)+c.sum+(1/2)*(lx[x+1+n]/lx[x+1])*exp(-r*n)
  value<-(Ax.bar/ax)
  
  px<-b*value/12
  
  return(px)
}

pv.annual<-function(data,x,i,sex,n,m,b){
  r<-log(1+i)
  a<-c()
  c<-c()
  if (sex=='Male'){
    lx<-data$lx_m
  } else{
    lx<-data$lx_f
  }
  for (j in (m+1):100){
    a[j]<-(lx[x+1+j]/lx[x+1])*exp(-r*j)
    a.sum<-sum(a,na.rm = T)
    
  }
  Ax.bar<-(1/2)*(lx[x+1+m]/lx[x+1])*(exp(-r*m))+a.sum
  
  for (j in 1:n-1){
    c[j]<-((lx[x+1+j]/lx[x+1])*exp(-r*j))
    c.sum<-sum(c)
  }
  ax<-1/2+c.sum+(1/2)*(lx[x+1+n]/lx[x+1])*exp(-r*n)
  value<-(Ax.bar/ax)
  px<-b*value/12
  
  return(px)
}


life.table <- function(data, sex){
  if (sex=='Male'){
    df <- data[, c(1, 2, 4, 6, 8)]
  }
  else{
    df <- data[, c(1, 3, 5, 7, 9)]
  }
  colnames(df) <- c('age', 'e', 'q', 'l', 'L')
  return(df)
}

## server.R ##

server <- function(input, output, session){
  
  Data <- reactive({
    req(input$file1)
    df = read.csv(input$file1$datapath)
    colnames(df) = c('x', 'ex_m', 'ex_f', 'qx_m', 'qx_f', 'lx_m', 'lx_f', 'Lx_m', 'Lx_f')
    return(df)
  })
  
  output$px <- renderValueBox({
    valueBox({
      if(req(input$type)=="life"){
        format(round(pv.life(Data(), input$x, input$i, input$sex, input$n,input$m,input$b),2)*10000,big.mark = ",")
      }
      else{
        format(round(pv.annual(Data(), input$x, input$i, input$sex, input$n,input$m,input$b),2)*10000,big.mark = ",")
      }
    }, h4("Premium"), icon = icon("won-sign"), color = "red")       
  })
  
  output$df <- renderTable(rbind(head(life.table(Data(), input$sex)), 
                                 data.frame(age=c('...'), e=c('...'), q=c('...'), l=c('...'), L=c('...')), 
                                 tail(life.table(Data(), input$sex))))
}

shinyApp(ui=ui, server=server)
