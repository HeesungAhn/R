This is a weekly team project for Theoretical Statistics II.
This is a code for R shiny application organizing optimal portfolio within 5 stocks and bank deposit by maximizing sharp ratio.

Input_investment information: daily return rate for 5 stocks initial investment amount, target return, investing period, period of validation data
Input_constraint: constraint ratior for each stock and deposit 
Output: optimal portfolio rate, expected return rate(mean, sd), sharp ratio

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(stats)
library(kableExtra)
library(dashboardthemes)
library(ggrepel)

header <- dashboardHeader(title = "Optimal Portfolio")

sidebar <- dashboardSidebar(
  
  sidebarMenu(id="type",
              menuItem("Input", tabName = "input", icon = icon("chart-bar"),
                       fileInput('file1', label=h5('Upload Deposit Data'),
                                 accept=c('text/csv', 
                                          'text/comma-separated-values,text/plain', '.csv')),
                       numericInput("V0", h5("Initial Investment Amount"), value=10000000),
                       numericInput("i", h5("Target Return (annual average)"), value=0.05),
                       numericInput("M", h5("Investment period (per year)"), value=5)),
              numericInput("N", h5("Period of Validation data (per year)"), value=5),
              
              menuItem("Constraints", tabName = "con", icon = icon("lock-open"),
                       sliderInput("s0", h5("Deposit"), min=0, max=1, value =c(0,1)),
                       sliderInput("s1", h5("SAMSUNG"), min=0, max=1, value =c(0,1)),
                       sliderInput("s2", h5("KAKAO"), min=0, max=1, value =c(0,1)),
                       sliderInput("s3", h5("KT&G"), min=0, max=1, value =c(0,1)),
                       sliderInput("s4", h5("KOGAS"), min=0, max=1, value =c(0,1)),
                       sliderInput("s5", h5("NAVER"), min=0, max=1, value =c(0,1)))
  )
)

body <- dashboardBody(
  valueBoxOutput("mean", width=4),
  valueBoxOutput("sd", width=4),
  valueBoxOutput("SR", width=4),
  
  fluidRow(
    box(title = "Information", 
        # column(12, align='center', tableOutput('Table')),
        status = 'warning', width = 6, height=470, solidHeader = T,
        plotOutput('Plot_theta')),
    box(title = "Time-Series Plot", 
        plotOutput('Plot_ts'), status = 'warning', width = 6, height=470, solidHeader = T)
  ),
  
  fluidRow(
    box(title="Validation", 
        column(12, align='center', tableOutput('Valid')), width=12)
  )
)

ui <- dashboardPage(header, sidebar, body, shinyDashboardThemes(
  theme = "blue_gradient"
))


shape.ratio <- function(theta, r, i){ # input: theta, return, target return ratio
  theta <- c( 1-sum(theta), theta) # theta0 = 1-sum(theta)
  theta <- matrix(theta, nrow=nrow(r), ncol=length(theta), byrow = T)
  rp <- rowSums(r*theta)
  rp.bar <- mean(rp)
  sp <- sd(rp)
  r.star <- (1+i)^(1/12)-1
  
  return((rp.bar - r.star)/sp)
} 

cal.theta <- function(train, i, a0, a1){
  train <- train[,-1]
  j <- ncol(train)-1
  ui <- rbind(rep(-1, j), diag(j))
  ui <- c(t(cbind(ui, -ui)))
  ui <- matrix(ui, ncol=j, byrow = T)
  ci <- c(a0-1, a1) # a0: interest rate interval / a1: others
  ci <- ci*c(1,-1)
  
  # initial value
  ci2 <- matrix(ci+0.1, ncol=1)
  theta0 <- c(MASS::ginv(ui) %*% ci2)
  
  # optim
  res <- constrOptim(theta = theta0, f=shape.ratio, grad=NULL, ui=ui, ci=ci, r=train, i=i, control = list(fnscale = -1))
  
  return(res)
}


outputs <- function(train,test,theta,i,v0){
  
  # i12(t)
  r.star <- (1+i)^(1/12)-1
  
  date <- test$t 
  
  train <- data.matrix(train[,-1])
  test <- data.matrix(test[,-1])
  n1 <- nrow(train)
  n2 <- nrow(test)
  
  
  theta <- c(1-sum(theta), theta) # theta0 = 1-sum(theta)
  
  mat1 <- matrix(theta, nrow=n1, ncol=length(theta), byrow = T)
  mat2 <- matrix(theta, nrow=n2, ncol=length(theta), byrow = T)
  
  rpt<-rowSums(train*mat1)
  rpt.star <- rowSums(test*mat2)
  
  rp.bar <-mean(rpt)
  sp <-sd(rpt)
  z<-(rp.bar-r.star)/sp
  
  ex.mean <- 12*rp.bar
  ex.sd <- sqrt(12)*sp
  
  
  real.mean <-mean(rpt.star)*12
  real.sd <-sd(rpt.star)*sqrt(12)
  vt <- v0*cumprod(1+rpt)
  vt.star <- v0*cumprod(1+rpt.star)
  
  result <- list(theta = theta,
                 sharpe = z%>%round(3),
                 month = c(rp.bar,sp)%>%round(3), 
                 expected = c(ex.mean,ex.sd)%>%round(3),
                 realized = c(real.mean,real.sd)%>%round(3),
                 #vt = data.frame(t = seq(61,119,1), vt.star = vt.star[-length(vt.star)]),
                 vt = data.frame(t = seq(1,60,1) ,vt=vt, vt.star = c(vt.star, NA)),
                 name = colnames(train))
  
  
  return(result)
} 


## server.R ##


server <- function(input, output, session){
  
  data <- reactive({
    infile1 <- input$file1
    int <- read.csv(infile1$datapath)
    print(dim(int))
    return(int)
  })
  
  out<-reactive({
    
    data  <- data()
    validate(
      need(data, "Please upload a data set"))
    n <- input$N*12
    #train <- data[1:n,]
    m <- input$M *12
    #test  <- data[(n+1):(n+m),]
    
    train<-data[1:n,]
    test<-data[n+1:n,]
    test<-test[-nrow(test),]
    #train <- data.matrix(train[-1]) 
    #test <- data.matrix(test[-1])
    
    
    a1 <- c(input$s1,input$s2, input$s3,
            input$s4, input$s5)
    theta<-cal.theta(train, input$i, a0=input$s0, a1=a1)$par
    
    result<- outputs(train,test,theta,input$i,input$V0)
    return(result)
  })
  
  # out <- out()
  output$mean <- renderValueBox({
    valueBox({
      out <- out()
      out$month[1]}, h4("Mean"),color = "aqua")       
  })
  output$sd <- renderValueBox({
    valueBox({
      out <- out()
      out$month[2]}, h4("SD"),color = "maroon")       
  })
  output$SR <- renderValueBox({
    valueBox({
      out <- out()
      out$sharpe}, h4("Sharpe Ratio"),color = "fuchsia")       
  })
  
  # output$Table <- renderTable({
  #     out <- out()
  #     data <- data()
  #     df_theta <- matrix(out$theta, nrow=1)
  #     colnames(df_theta) <- out$name
  #     print(df_theta)
  # })
  
  output$Plot_theta <- renderPlot({
    out <- out()
    data <- data()
    df_theta <- data.frame(theta=out$theta, stock=out$name)
    if(!is.null(out)){
      pie_plot <- ggplot(df_theta, aes(x=stock, y=theta, fill=stock)) + geom_bar(stat='identity') + geom_text(aes(label = paste0(stock, "\n", round(theta*100), "%")), 
                                                                                                              position = position_stack(vjust = 0.5))+labs(x='', y='') + ggtitle("Optimal Portfolio Ratio") + theme(panel.grid = element_blank(), axis.ticks = element_blank(), axis.text.x=element_blank(), panel.border = element_blank(), panel.background = element_blank(), plot.title = element_text(hjust = 0.5))
      print(pie_plot)
    }
  })
  
  output$Valid <- function(){
    out <- out()
    df <- data.frame(cbind(out$expected, out$realized))
    colnames(df) <- c('expected', 'real')
    rownames(df) <- c('mean', 'sd')
    df %>% knitr::kable('html') %>% kable_styling('striped', full_width=T, font_size=20, position='center') %>% row_spec(row = 1:2,
                                                                                                                         color = "black")
    # background = "black")
  }
  
  output$Plot_ts <- renderPlot({
    out <- out()
    if(!is.null(out)){
      t_min <- min(out$vt$t)
      t_max <- max(out$vt$t)
      gg_plot <- out$vt %>% ggplot()+geom_line(aes(x=t,y=vt.star, group=1))+ labs(title=paste0(t_min, ' ~ ',t_max), x='date', y='Vt')
      print(gg_plot)
    }
  })
  
  
  
}

shinyApp(ui=ui, server = server)
