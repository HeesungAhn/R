This is a weekly team project for Theoretical Statistics II.
This is a code for R shiny application calculating the option price through Monte-Carlo simulation.

Input: stock price for 2 years, option type(asian call/asian put), maturity, excercise price, interest rate, volatility
Output: option price, time-series plot

library(shiny)
library(shinydashboard)
library(tidyverse)
library(fresh)

mytheme <- create_theme(
  adminlte_color(
    light_blue = "#011f4b"
  ),
  adminlte_sidebar(
    dark_bg = "#03396c",
    dark_hover_bg = "#173614",
    dark_color = "#173614"
  ),
  adminlte_global(
    content_bg = "#dfe7f7",
    box_bg = "#03396c", 
    info_box_bg = "#173614"
  )
)

header <- dashboardHeader(title = "Asian Option Price")


sidebar <- dashboardSidebar(
  # load data
  fileInput('file', label=h5('Upload Data File'),
            accept=c('text/csv', 
                     'text/comma-separated-values,text/plain', 
                     '.csv')),
  
  radioButtons('option', 'Option',
               choices = c("Asian Call" = 'call',
                           "Asian Put" = 'put'),
               selected = 'call'),
  
  numericInput("r", h5("Riskless interest rate"),value=0.05), 
  numericInput("sigma", h5("Volatility"), value=0.5),
  numericInput("s0", h5("Stock Price"), value=3000),
  numericInput("k", h5("Excercise Price"), value=3000),
  sliderInput("t", h5("Year"), min=1, max=12, value=1),
  solidHeader = T, status = "warning")



body <- dashboardBody(
  
  use_theme(mytheme),
  
  fluidRow(
    valueBoxOutput("price"), valueBoxOutput("stock"), valueBoxOutput("exercise")),
  
  fluidRow(
    box(title = 'Time-Series plot', status = 'primary',width = 12, plotOutput("Plot"), solidHeader = T)
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
    df$date <- as.Date(df$date)
    df <- df %>% arrange(date)
    return(df)
  })
  
  output$Plot <- renderPlot({
    data <- data()
    if(!is.null(data)){
      data <- data[(nrow(data)-250):nrow(data),]
      gg_plot <- ggplot(data)+geom_line(aes(x=date, y=St), lwd=1) + ggtitle(paste(min(data$date),"~",max(data$date)))
      print(gg_plot)
    }
  })
  
  
  output$stock = renderValueBox({
    df = data()
    valueBox(
      value = input$s0,
      "Stock Price",
      color = "yellow",
      icon = icon("fa-sharp fa-solid fa-chart-line")
    )
  })
  
  output$exercise = renderValueBox({
    df = data()
    valueBox(
      value = input$k,
      "Exercise Price",
      color = "green",
      icon = icon("fa-sharp fa-solid fa-comments-dollar")
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
        
      }, "Price", icon = icon("won-sign"),
      color = "red")       
    })
  })
  
}





shinyApp(ui=ui, server = server)
