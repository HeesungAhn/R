This is a final project for Theoretical Statistics II.
This is a code for R shiny application calculating the car insurance premium through GLM.

Input_driver information: region, type of car, mileage, merit(accident-free career)
Input_insurance information: deductible, limit, exchange rate
Output: insurance premium(won, dollar, krona), coefficient effect plot



library(shiny)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(gridExtra)
library(fresh)
library(plotly)
library(shinyWidgets)

options('scipen'=100)


mytheme <- create_theme(
  adminlte_color(
    light_blue = "#3d5a80"
  ),
  adminlte_sidebar(
    dark_bg = "#3d5a80",
    dark_hover_bg = "#3d5a80",
    dark_color = "#3d5a80"
  ),
  adminlte_global(
    content_bg = "#edf2f4",
    box_bg = "#eaeaea", 
    info_box_bg = "#99AEAD"
  )
)

header = dashboardHeader(title = 'Car Insurance Premium')

sidebar = dashboardSidebar(
  
  
  sidebarMenu(fileInput('file', label=h6('Upload Data File'),
                        accept=c('text/csv', 
                                 'text/comma-separated-values,text/plain', 
                                 '.csv')),
              
              
              tags$hr(),
              
              sliderInput('a', h5('Deductible'), value=0 ,min = 0,max = 1000),
              sliderInput('b', h5('Limit'), value=0, min = 0, max = 2000000),
              
              numericInput('exchanged', h5("Krona-Dollar exchage rate"), value=0.097),
              numericInput('exchangew', h5("Krona-Won exchage rate"), value=126.43),
              
              actionButton('htmlbutton', label=h5('Exchange rate'), 
                           onclick = "window.open('https://www.google.com/finance/quote/SEK-USD?sa=X&ved=2ahUKEwiRpYvp0PP7AhVKRd4KHUbXD1YQmY0JegQIBhAc')",
                           icon = icon("fa-brands fa-internet-explorer"), width="90%"
              )           
              )
  )




body = dashboardBody(
  use_theme(mytheme),
  tags$style(
    type = 'text/css', 
    '.bg-yellow {background-color: #3d5a80!important; }',
    '.bg-red {background-color:  #ee6c4d!important; }',
    '.bg-green {background-color: #98c1d9!important; }',
    '.bg-teal {background-color: #293241!important; }'
  ),
  #tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background-color: #28363D}")),
  
  tabsetPanel(
    tabPanel("Premium",
             
  h3("Output"),
  box(
  fluidRow(column(width=4,valueBoxOutput("won", width=NULL)),
           column(width=4,valueBoxOutput("dollar", width=NULL)),
           column(width=4,valueBoxOutput("krona", width=NULL))), width=12),
  
  h3("Intput"),
  fluidRow(
    box(
      column(3, selectInput("Zone", h4("Zone"),
                  choices=list('Seoul'= 1,
                               'Gyeonggi-do, Incheon' = 2,
                               'Ulsan, Busan, Daegu'=3,
                               'Gyeongsang-do' = 4,
                               'Gwangju, Sejong, Daejeon' =5,
                               'Chungcheong-do' = 6,
                               'Jeju' = 7), selected=1)),
      
      column(3, selectInput("Make", h4("Type of Car"),
                  choices= list('Benz - Sedan'= 1,
                                'Granger - Sedan' = 2,
                                'KIA - K7'=3,
                                'Hyndai - Genesis' = 4,
                                'Hyndai - Sonata' =5,
                                'KIA - K9' = 6,
                                'Chevrolet - Malibu' = 7,
                                'Benz - Passenger Car' =8,
                                'Audi - Passenger Car' = 9), selected=1)),
      column(3, selectInput("Kilometers", h4("Kilometers"),
                  choices = list('less than 1000'= 1,
                                 'from 1000 to 15000' = 2,
                                 '15000 to 20000'=3,
                                 '20000 to 25000' = 4,
                                 'more than 25000' =5), selected=1)),
      column(3, sliderInput('Bonus', h4('Bonus(No Accident + 1)'), min=0, max=7, value=0)), 
      width=12
    )
  ),
  
  box(
    
      column(6, valueBoxOutput("Bonus", width=NULL),  valueBoxOutput("Kilometers", width=NULL)),
    
      column(6, valueBoxOutput("Zone", width=NULL),  valueBoxOutput("Make", width=NULL)), width=12, height=250
  )),
  
  tabPanel("Plot",
  fluidRow(
    box(title = 'Coefficient Effect Plot', width = 12, plotOutput('Plot'), solidHeader = T)
  ))
))




ui = dashboardPage(header, sidebar, body)


## function
data = read_excel('car insurance.xls',sheet = 1)

f_gamma = function(y, glm, new, d=TRUE){
  
  mu = predict(glm,newdata = new,type= 'response')
  shape = 1/summary(glm)$dispersion
  scale = mu/shape
  
  pdf = dgamma(y, shape=shape, scale=scale)
  cdf = pgamma(y, shape=shape, scale=scale)
  
  if(d==TRUE){
    y*pdf
  }
  else{
    cdf
  }
}


premium = function(f,A,B,new){
  
  data %>% mutate(
    Zone = factor(Zone),
    Make = factor(Make),
    Kilometres = factor(Kilometres),
    Bonus=factor(Bonus) ) -> data 
  
  df_fit1 = filter(data, Insured >= 1)
  df_fit2 = filter(data, Claims >= 1)
  
  glm_poiss =glm(Claims/Insured ~ Kilometres+Bonus+Make+Zone ,weights= Insured, family=poisson(link='log'), data=df_fit1)
  glm_gamma = glm(Payment/Claims~ Kilometres+Bonus+Make+Zone, weights = Claims, family=Gamma(link='log'), data=df_fit2)
  
  # EN
  EN = predict(glm_poiss,newdata = new,type= 'response')
  
  # EY
  B = ifelse(B==0, Inf, B)
  part1 = integrate(f, A, (A+B), glm=glm_gamma,new=new, d=TRUE)
  part2 = ifelse(B==Inf , 0, B*(1-f((A+B), glm_gamma,new=new, d=FALSE)))
  part3 = -A*(f((A+B), glm_gamma,new=new, d=FALSE) - f(A, glm_gamma,new=new, d=FALSE))
  EY = part1$value+part2+part3
  
  # Premium
  price =EN*EY
  return(list(coef1=glm_poiss$coef, coef2=glm_gamma$coef, price=price))
}

kfactor<-function(Bonus){
  if (Bonus==0 | Bonus==1){
    temp="less than 1000"
  }else if (Bonus==2){
    temp="from 1000 to 15000"
  }else if (Bonus==3){
    temp="15000 to 20000"
  }else if (Bonus==4){
    temp="20000 to 25000"
  }else{
    temp="more than 25000"}
  return(temp)
}

mfactor<-function(k){
  if (k==1){
    temp="Benz - Sedan"
  }else if (k==2){
    temp="Granger - Sedan"
  }else if (k==3){
    temp="KIA - K7"
  }else if (k==4){
    temp="Hyndai - Genesis"
  }else if (k==5){
    temp="Hyndai - Sonata"
  }else if (k==6){
    temp="KIA - K9"
  }else if (k==7){
    temp="Chevrolet - Malibu"
  }else if (k==8){
    temp="Benz - Passenger Car"
  }else{
    temp="Audi - Passenger Car"}
  return(temp)
}

zfactor<-function(k){
  if (k==1){
    temp="Seoul"
  }else if (k==2){
    temp="Gyeonggi-do, Incheon"
  }else if (k==3){
    temp="Ulsan, Busan, Daegu"
  }else if (k==4){
    temp="Gyeongsang-do"
  }else if (k==5){
    temp="Gwangju, Sejong, Daejeon"
  }else if (k==6){
    temp="Chungcheong-do"
  }else{
    temp="Jeju"}
  return(temp)
}



server = function(input, output, session){
  
  inp = reactive({
    df=data.frame(var=c('Bonus', 'Kilometres', 'Make', 'Zone'),
                  input=c(as.factor(input$Bonus+1), as.factor(input$Kilometers), 
                          as.factor(input$Make), as.factor(input$Zone)))
    
    return(df)
  })
  
  new = reactive({
    df=data.frame(Kilometres = as.factor(input$Kilometers), Bonus = as.factor(input$Bonus+1),
                  Make = as.factor(input$Make), Zone = as.factor(input$Zone))
    
    res = premium(f_gamma, input$a, input$b, df)
    return(res)
  })
  
  
  output$krona = shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value=round(new()$price),
      "Sweden Krona",
      color="green",
      icon = icon("fa-solid fa-euro-sign"),
      width=3
    )
  })
  
  output$dollar = shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value=round(new()$price*input$exchanged, 2),
      "Us Dollar",
      color="yellow",
      icon = icon("fa-solid fa-dollar-sign"),
      width=3
    )
  })
  
  output$won = shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value=round(new()$price*input$exchangew),
      "Korea Won",
      color="red",
      icon = icon("fa-solid fa-won-sign"),
      width=3
    )
  })

  
  output$Bonus = shinydashboard::renderValueBox({
    #df<-df()
    shinydashboard::valueBox(
      value=paste(input$Bonus, "year"),
      "No Accident Period (year)",
      color="teal",
      icon = tags$i(class = "fa-solid fa-car-burst", style="font-size: 72px; color: white")
    )
  })
  
  output$Kilometers = shinydashboard::renderValueBox({
    #df<-df()
    shinydashboard::valueBox(
      value=kfactor(input$Kilometers),
      "Mileage (km)",
      color="teal",
      icon = tags$i(class = "fa-solid fa-gauge-high", style="font-size: 72px; color: white")
    )
  })
  
  output$Make = shinydashboard::renderValueBox({
    #df<-df()
    shinydashboard::valueBox(
      value=mfactor(input$Make),
      "Type of Car",
      color="teal",
      icon = tags$i(class = "fa-solid fa-car", style="font-size: 72px; color: white")
    )
  })
  
  output$Zone = shinydashboard::renderValueBox({
    #df<-df()
    shinydashboard::valueBox(
      value=zfactor(input$Zone),
      "Zone",
      color="teal",
      icon = tags$i(class = "fa-solid fa-map-location-dot", style="font-size: 72px; color: white")
    )
  })
  
  output$Plot = renderPlot({
    df = new()
    l = inp()
    coef1 = data.frame(coef = df$coef1[-1]) 
    coef1['var'] = gsub('\\d', '', rownames(coef1))
    coef1['level'] = gsub('\\D', '', rownames(coef1))
    rownames(coef1) = 1:nrow(coef1)
    coef.zero = data.frame(coef=rep(0,4), var=unique(coef1$var), level=rep(1, 4))
    coef1 = rbind(coef1, coef.zero)
    
    g1 = ggplot(coef1) +
      geom_point(aes(x=level, y=coef, col=var), size=2) +
      geom_line(aes(x=level, y=coef, col=var, group=var), size=1) +
      geom_vline(data=l, mapping=aes(xintercept=input), color='red', size=1) +
      facet_wrap(~var, scales = 'free') +
      labs(title = 'Accident No.') + theme(legend.position = 'none')
    
    
    coef2 = data.frame(coef = df$coef2[-1]) 
    coef2['var'] = gsub('\\d', '', rownames(coef2))
    coef2['level'] = gsub('\\D', '', rownames(coef2))
    rownames(coef2) = 1:nrow(coef2)
    coef2 = rbind(coef2, coef.zero)
    
    
    g2 = ggplot(coef2) +
      geom_point(aes(x=level, y=coef, col=var), size=2) +
      geom_line(aes(x=level, y=coef, col=var, group=var), size=1) +
      geom_vline(data=l, mapping=aes(xintercept=input), color='red', size=1) +
      facet_wrap(~var, scales = 'free') +
      labs(title = 'Accident compensation') + theme(legend.position = 'none')
    
    grid.arrange(g1,g2, nrow=1)
    
  })
}




shinyApp(ui=ui, server = server)
