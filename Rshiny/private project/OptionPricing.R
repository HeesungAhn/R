This is a final project for Theoretical Statistics II.
This is a code for R shiny application calculating the option price through Monte-Carlo simulation.

Input: stock price for 2 years, option type(asian call/asian put), maturity, excercise price, interest rate, volatility
Output: option price, time-series plot



library(shiny)
library(shinydashboard)
library(tidyverse)
library(fresh)
library(plotly)
library(shinyWidgets)
library(anytime)

mytheme <- create_theme(
  adminlte_color(
    light_blue = "#6D9197"
  ),
  adminlte_sidebar(
    dark_bg = "#99AEAD",
    dark_hover_bg = "#99AEAD",
    dark_color = "#99AEAD"
  ),
  adminlte_global(
    content_bg = "#DEE1DD",
    box_bg = "#99AEAD", 
    info_box_bg = "#99AEAD"
  )
)

header <- dashboardHeader(title= "Asian Option Pricing")


sidebar <- dashboardSidebar(
  # load data
  fileInput('file', label=h5('Upload Data File'),
            accept=c('text/csv', 
                     'text/comma-separated-values,text/plain', 
                     '.csv')),
  
  actionButton('htmlbutton', label=h5('Data source site'), 
               onclick = "window.open('http://data.krx.co.kr/contents/MDC/MDI/mdiLoader/index.cmd?menuId=MDC0201020103')",
               icon = icon("fa-brands fa-internet-explorer"), width="90%"
               ))
  


body <- dashboardBody(
  
  use_theme(mytheme),
  tags$style(
    type = 'text/css', 
    '.bg-yellow {background-color: #2F575D!important; }',
    '.bg-red {background-color: #658B6F!important; }',
    '.bg-green {background-color: #28363D!important; }'
  ),
  tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background-color: #28363D}")),
  
  fluidRow(
    valueBoxOutput("price"), valueBoxOutput("stock"), valueBoxOutput("exercise")),
  
  fluidRow(
    #chooseSliderSkin("Flat"),
    box(
    column(4, 
           radioButtons('option', h4('Option'), choices = c("Asian Call" = 'call', "Asian Put" = 'put'),selected = 'call'),
           sliderInput("t", h4("Maturity (month)"), min=1, max=12, value=6)),
    column(4,
           numericInput("s0", h4("Recent Stock Price (won)"), value=70000),
           br(),
           numericInput("k", h4("Excercise Price (won)"), value=70000)),
          
    column(4,
           numericInput("r", h4("Riskless interest rate"),value=0.05),
           br(),
           numericInput("sigma", h4("Volatility"), value=0.5)),
    height = 250, width = 12)),
  
  fluidRow(
    box(title = 'Time-Series plot', status = 'primary',width = 12, plotlyOutput("Plot"), solidHeader = T)
  )
)




ui <- dashboardPage(header, sidebar, body)

## function
mc <- function(s0, K, t, r, sigma){
  M <- 100000
  delta.t <- 1/250
  large.t <- round(250*t)
  ci <- c()
  pi <- c()
  z <- matrix(rnorm(M*large.t), ncol=M)
  
  st <- matrix(c(rep(s0, M), rep(NA, M*large.t)), ncol=M, byrow=T)
  
  for(j in 1:large.t){
    st[j+1,] <- st[j,]*exp((r-sigma^2/2)*delta.t + sigma*sqrt(delta.t)*z[j,]) 
  }
  
  st.bar <- colMeans(st)
  c.list <- exp(-r*t)*sapply(st.bar-K, max, 0)
  p.list <- exp(-r*t)*sapply(K-st.bar, max, 0)
  
  ct <- mean(c.list)
  pt <- mean(p.list)
  
  return(list(call=ct, put=pt))
}


## server

server <- function(input, output, session){
  data <- reactive({
    infile <- input$file
    
    if(is.null(infile)){
      return(NULL)
    }
    df <- read.csv(infile$datapath, col.names = c('date', 'St'))
    #df$date <- as.Date(df$date)
    df <- df %>% arrange(date)
    return(df)
  })
  
  output$Plot <- renderPlotly({
    data <- data()
    if(!is.null(data)){
      #data <- data[(nrow(data)-250):nrow(data),]
      data$date<-anydate(data$date)
      
      gg_plot <- ggplot(data)+geom_line(aes(x=date, y=St), lwd=1) + ggtitle(paste(min(data$date),"~",max(data$date)))
      ggplotly(gg_plot)
    }
  })
  
  
  output$stock = renderValueBox({
    df = data()
    valueBox(
      value = input$s0,
      "Recent Stock Price (won)",
      color = "yellow",
      icon = icon("fa-light fa-money-bill-trend-up")
    )
  })
  
  output$exercise = renderValueBox({
    df = data()
    valueBox(
      value = input$k,
      "Exercise Price (won)",
      color = "green",
      icon = icon("fa-regular fa-coins")
    )
  })
  
  
  observeEvent(input$option, {
    output$price <- renderValueBox({
      valueBox({
        data <- data()
        
        validate(need(data, "please upload"))
        #s0 <- data[data$date==input$date,]$St
        
        if(input$option=="call"){
          format(round(mc(input$s0, input$k, input$t/12, input$r, input$sigma)$call),big.mark = ",")
        }
        else{
          format(round(mc(input$s0, input$k, input$t/12, input$r, input$sigma)$put), big.mark = ",")
        } 
        
      }, "Option Price (won)", icon = icon("won-sign"),
      color = "red")       
    })
  })
  
}





shinyApp(ui=ui, server = server)
