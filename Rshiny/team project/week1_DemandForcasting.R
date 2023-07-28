This is a weekly team project for Theoretical Statistics II.
This is a code for R shiny application forcasting movie demand through Difussion models.

Input: date, daily audience, Diffusion model(Bass, Gumbel, Logistic, Exponential), estimation method(OLS, QQ, MLE)
Output: expected total audience, coefficient of innovation, coefficient of immitation, QQ plot of each model(Bass, Gumbel, Logistic, Exponential)

library(dashboardthemes)
library(DT)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(fresh)

## estimate m, p, q

#### OLS ####
bass.ols = function(data){
  bassols = lm(St ~ 1+Yt_1+I(Yt_1^2), data)
  a = bassols$coef[1]
  b = bassols$coef[2]
  c = bassols$coef[3]
  
  m = max((-b-sqrt(b^2-4*a*c))/(2*c), (-b+sqrt(b^2-4*a*c))/(2*c))
  p = a/m
  q = -m*c
  
  return(list(m=m, p=p, q=q))
}

gum.ols = function(data){
  gumols = lm(St ~ Yt_1+(Yt_1:log(Yt_1))-1, data[data$Yt_1!=0, ])
  a = gumols$coef[1]
  b = gumols$coef[2]
  
  m = exp(a/(-b))
  p = 0
  q = -b
  
  return(list(m=m, p=p, q=q))
}

logis.ols = function(data){
  logisols = lm(St ~ Yt_1+I(Yt_1^2)-1, data)
  a = logisols$coef[1]
  b = logisols$coef[2]
  
  m = -a/b
  p = 0
  q = a
  
  return(list(m=m, p=p, q=q))
}

exp.ols = function(data){
  expols = lm(St ~ Yt_1, data)
  a = expols$coef[1]
  b = expols$coef[2]
  
  m = -a/b
  p = -b
  q = 0
  
  return(list(m=m, p=p, q=q))
}

#### QQ ####

bass.qq = function(data){
  
  m_temp = data.frame(m=seq(data$Yt[length(data$Yt)]*0.8, data$Yt[length(data$Yt)]*1.2, data$Yt[length(data$Yt)]*0.05), 
                      rm=rep(0, length(seq(data$Yt[length(data$Yt)]*0.8, data$Yt[length(data$Yt)]*1.2, data$Yt[length(data$Yt)]*0.05))))
  
  for(i in 1:length(m_temp$m)){
    bass.i = log((1+(bass.ols(data)$q/bass.ols(data)$p)*(data$Yt/(m_temp$m[i]+1)))/(1-data$Yt/(m_temp$m[i]+1))) # p와 q는 ols로 추정한 값 넣어줌
    m_temp$rm[i] = summary(lm(t~bass.i, data))$r.squared
  }
  
  m = m_temp$m[which.max(m_temp$rm)]
  
  bassols = lm(St ~ 1+Yt_1+I(Yt_1^2), data)
  a = bassols$coef[1]
  c = bassols$coef[3]
  
  p = a/m
  q = -m*c 
  k = p+q
  c = q/p
  
  return(list(m=m, p=p, q=q, mu=k, sig=c)) 
}

gum.qq = function(data){
  m_temp = data.frame(m=seq(data$Yt[length(data$Yt)]*0.8, data$Yt[length(data$Yt)]*1.2, data$Yt[length(data$Yt)]*0.05), 
                      rm=rep(0, length(seq(data$Yt[length(data$Yt)]*0.8, data$Yt[length(data$Yt)]*1.2, data$Yt[length(data$Yt)]*0.05))))
  
  for(i in 1:length(m_temp$m)){
    gum.i = -log(-log(data[data$Yt_1!=0, ]$Yt/(m_temp$m[i]+1)))
    m_temp$rm[i] = summary(lm(t~gum.i, data=data[data$Yt_1!=0, ]))$r.squared
  }
  
  m = m_temp$m[which.max(m_temp$rm)]
  
  data2_2 = data.frame(data) %>% mutate(rank=Yt, gum=-log(-log(rank/(m+1))))
  
  mu = lm(t~gum, data2_2)$coef[1]
  sig = lm(t~gum, data2_2)$coef[2]
  q = 1/sig
  
  return(list(m=m, p=0, q=q, mu=mu, sig=sig))
}

logis.qq = function(data){
  m_temp = data.frame(m=seq(data$Yt[length(data$Yt)]*0.8, data$Yt[length(data$Yt)]*1.2, data$Yt[length(data$Yt)]*0.05), 
                      rm=rep(0, length(seq(data$Yt[length(data$Yt)]*0.8, data$Yt[length(data$Yt)]*1.2, data$Yt[length(data$Yt)]*0.05))))
  
  for(i in 1:length(m_temp$m)){
    logis.i = log((data$Yt/(m_temp$m[i]+1))/(1-data$Yt/(m_temp$m[i]+1)))
    m_temp$rm[i] = summary(lm(t~logis.i, data))$r.squared
  }
  
  m = m_temp$m[which.max(m_temp$rm)]
  
  data3_2 = data.frame(data) %>% mutate(rank=Yt, logis=log((rank/(m+1))/(1-rank/(m+1))))
  
  mu = lm(t~logis, data3_2)$coef[1]
  sig = lm(t~logis, data3_2)$coef[2]
  q = 1/sig
  
  return(list(m=m, p=0, q=q, mu=mu, sig=sig))
}

exp.qq = function(data){
  m_temp = data.frame(m=seq(data$Yt[length(data$Yt)]*0.8, data$Yt[length(data$Yt)]*1.2, data$Yt[length(data$Yt)]*0.05), 
                      rm=rep(0, length(seq(data$Yt[length(data$Yt)]*0.8, data$Yt[length(data$Yt)]*1.2, data$Yt[length(data$Yt)]*0.05))))
  
  for(i in 1:length(m_temp$m)){
    exp.i = -log(1-(data$Yt/(m_temp$m[i]+1)))
    m_temp$rm[i] = summary(lm(t~exp.i, data))$r.squared
  }
  
  m = m_temp$m[which.max(m_temp$rm)]
  
  data4_2 = data.frame(data) %>% mutate(rank=Yt, exp=-log(1-rank/(m+1)))
  
  mu = lm(t~exp, data4_2)$coef[1]
  sig = lm(t~exp, data4_2)$coef[2]
  p = 1/sig
  
  
  return(list(m=m, p=p, q=0, mu=mu, sig=sig))
}

#### MLE ####
gum.mle = function(data){
  initial = c(gum.qq(data)$m, gum.qq(data)$mu, gum.qq(data)$sig)
  L = function(data, params){
    m = params[1]
    mu = params[2]
    sig = params[3]
    Ft = exp(-exp(-(data$t-mu)/sig))
    Ft_1 = c(0, Ft[1:(length(Ft)-1)])
    pj = Ft-Ft_1
    S = append(data$St, m-max(data$Yt))
    pj = append(pj, 1-Ft[length(Ft)])
    lik = lfactorial(m) - sum(lfactorial(S)) + sum(S*log(pj))
    
    return(-lik)
  }
  
  opt = optim(par=initial, data=data, fn=L)
  m=opt$par[1]; mu=opt$par[2]; sig=opt$par[3]
  q=1/sig
  
  return(list(m=m, p=0, q=q, mu=mu, sig=sig))
}

logis.mle = function(data){
  initial = c(logis.qq(data)$m, logis.qq(data)$mu, logis.qq(data)$sig)
  L = function(data, params){
    m = params[1]
    mu = params[2]
    sig = params[3]
    Ft = 1/(1+exp(-(data$t-mu)/sig))
    Ft_1 = c(0, Ft[1:(length(Ft)-1)])
    pj = Ft-Ft_1
    S = append(data$St, m-max(data$Yt))
    pj = append(pj, 1-Ft[length(Ft)])
    lik = lfactorial(m) - sum(lfactorial(S)) + sum(S*log(pj))
    
    return(-lik)
  }
  
  opt = optim(par=initial, data=data, fn=L)
  m=opt$par[1]; mu=opt$par[2]; sig=opt$par[3]
  q=1/sig
  
  return(list(m=m, p=0, q=q, mu=mu, sig=sig))
}


#### predict S(t) ####
# ols
bass.ols.pred = function(data, n){
  
  data1_1 = data.frame(data)
  
  for (i in 1:n){
    data1_1 = rbind(data1_1, NA)
  }
  
  data1_1['t'] = seq(data$t[1], data$t[length(data$t)]+n)
  bassols = lm(St ~ 1+Yt_1+I(Yt_1^2), data)
  for (i in 0:(n-1)){
    pred = predict(bassols, newdata=data.frame(Yt_1=data1_1$Yt[length(data$Yt)+i]))[[1]]
    data1_1[length(data$Yt)+i+1, 'St'] = pred
    data1_1[length(data$Yt)+i+1, 'Yt'] = data1_1[length(data$Yt)+i, 'Yt'] + pred
    data1_1[length(data$Yt)+i+1, 'Yt_1'] = data1_1[length(data$Yt)+i, 'Yt']
  }
  
  data1_1['Model'] = 'bass'
  data1_1['Method'] = 'ols'
  data1_1 = data1_1[, c('t', 'Model', 'Method', 'St')]
  
  return(data1_1)
}

gum.ols.pred = function(data, n){
  
  data2_1 = data.frame(data)
  
  for (i in 1:n){
    data2_1 = rbind(data2_1, NA)
  }
  
  data2_1['t'] = seq(data$t[1], data$t[length(data$t)]+n)
  gumols = lm(St ~ Yt_1+(Yt_1:log(Yt_1))-1, data[data$Yt_1!=0, ])
  for (i in 0:(n-1)){
    pred = predict(gumols, newdata=data.frame(Yt_1=data2_1$Yt[length(data$Yt)+i]))[[1]]
    data2_1[length(data$Yt)+i+1, 'St'] = pred
    data2_1[length(data$Yt)+i+1, 'Yt'] = data2_1[length(data$Yt)+i, 'Yt'] + pred
    data2_1[length(data$Yt)+i+1, 'Yt_1'] = data2_1[length(data$Yt)+i, 'Yt']
  }
  
  data2_1['Model'] = 'gum'
  data2_1['Method'] = 'ols'
  data2_1 = data2_1[, c('t', 'Model', 'Method', 'St')]
  
  return(data2_1)
}

logis.ols.pred = function(data, n){
  
  data3_1 = data.frame(data)
  
  for (i in 1:n){
    data3_1 = rbind(data3_1, NA)
  }
  
  data3_1['t'] = seq(data$t[1], data$t[length(data$t)]+n)
  logisols = lm(St ~ Yt_1+I(Yt_1^2)-1, data)
  for (i in 0:(n-1)){
    pred = predict(logisols, newdata=data.frame(Yt_1=data3_1$Yt[length(data$Yt)+i]))[[1]]
    data3_1[length(data$Yt)+i+1, 'St'] = pred
    data3_1[length(data$Yt)+i+1, 'Yt'] = data3_1[length(data$Yt)+i, 'Yt'] + pred
    data3_1[length(data$Yt)+i+1, 'Yt_1'] = data3_1[length(data$Yt)+i, 'Yt']
  }
  
  data3_1['Model'] = 'logis'
  data3_1['Method'] = 'ols'
  data3_1 = data3_1[, c('t', 'Model', 'Method', 'St')]
  
  return(data3_1)
}

exp.ols.pred = function(data, n){
  
  data4_1 = data.frame(data)
  
  for (i in 1:n){
    data4_1 = rbind(data4_1, NA)
  }
  
  data4_1['t'] = seq(data$t[1], data$t[length(data$t)]+n)
  expols = lm(St ~ Yt_1, data)
  for (i in 0:(n-1)){
    pred = predict(expols, newdata=data.frame(Yt_1=data4_1$Yt[length(data$Yt)+i]))[[1]]
    data4_1[length(data$Yt)+i+1, 'St'] = pred
    data4_1[length(data$Yt)+i+1, 'Yt'] = data4_1[length(data$Yt)+i, 'Yt'] + pred
    data4_1[length(data$Yt)+i+1, 'Yt_1'] = data4_1[length(data$Yt)+i, 'Yt']
  }
  
  data4_1['Model'] = 'exp'
  data4_1['Method'] = 'ols'
  data4_1 = data4_1[, c('t', 'Model', 'Method', 'St')]
  
  return(data4_1)
}

# qqplot
bass.qq.pred = function(data, n){
  
  data1_2 = data.frame(data)
  
  Shat_qq = function(x){
    p = bass.qq(data)$p
    q = bass.qq(data)$q
    m = bass.qq(data)$m
    p*m + (q-p)*x + (-q/m)*(x^2)
  }
  
  for (i in 1:n){
    data1_2 = rbind(data1_2, NA)
  }
  
  data1_2['t'] = seq(data$t[1], data$t[length(data$t)]+n)
  
  for (i in 0:(n-1)) {
    pred = Shat_qq(data1_2$Yt[length(data$Yt)+i])
    data1_2[length(data$Yt)+i+1, 'St'] = pred
    data1_2[length(data$Yt)+i+1, 'Yt'] = data1_2[length(data$Yt)+i, 'Yt'] + pred
    data1_2[length(data$Yt)+i+1, 'Yt_1'] = data1_2[length(data$Yt)+i, 'Yt']
  }
  
  data1_2['Model'] = 'bass'
  data1_2['Method'] = 'qqplot'
  data1_2 = data1_2[, c('t', 'Model', 'Method', 'St')]
  
  return(data1_2)
}


gum.qq.pred = function(data, n){
  
  data2_2 = data.frame(data)
  for (i in 1:n){
    data2_2 = rbind(data2_2, NA)
  }
  data2_2['t'] = seq(data$t[1], data$t[length(data$t)]+n)
  
  mu = gum.qq(data)$mu
  sig = gum.qq(data)$sig
  m = gum.qq(data)$m
  
  data2_2['x'] = (data2_2$t-mu)/sig
  
  gumf = function(x){
    (m/sig)*(exp(-x)*(exp(-exp(-x))))
  }
  
  data2_2[(nrow(data)+1):nrow(data2_2), 'St'] = gumf(data2_2[(nrow(data)+1):nrow(data2_2), 'x'])
  
  data2_2['Model'] = 'gum'
  data2_2['Method'] = 'qqplot'
  data2_2 = data2_2[, c('t', 'Model', 'Method', 'St')]
  
  
  return(data2_2)
}

logis.qq.pred = function(data, n){
  
  data3_2 = data.frame(data)
  for (i in 1:n){
    data3_2 = rbind(data3_2, NA)
  }
  data3_2['t'] = seq(data$t[1], data$t[length(data$t)]+n)
  
  m = logis.qq(data)$m
  mu = logis.qq(data)$mu
  sig = logis.qq(data)$sig
  
  data3_2['x'] = (data3_2$t-mu)/sig
  
  logisf = function(x){
    (m/sig)*(exp(-x)/(1+exp(-x))^2)
  }
  
  data3_2[(nrow(data)+1):nrow(data3_2), 'St'] = logisf(data3_2[(nrow(data)+1):nrow(data3_2), 'x'])
  
  data3_2['Model'] = 'logis'
  data3_2['Method'] = 'qqplot'
  data3_2 = data3_2[, c('t', 'Model', 'Method', 'St')]
  
  return(data3_2)
}

exp.qq.pred = function(data, n){
  
  data4_2 = data.frame(data)
  for (i in 1:n){
    data4_2 = rbind(data4_2, NA)
  }
  data4_2['t'] = seq(data$t[1], data$t[length(data$t)]+n)
  
  m = exp.qq(data)$m
  mu = exp.qq(data)$mu
  sig = exp.qq(data)$sig
  
  data4_2['x'] = (data4_2$t-mu)/sig
  expf = function(x){
    (1/sig)*(exp(-(1/sig)*x))
  }
  
  data4_2[(nrow(data)+1):nrow(data4_2), 'St'] = expf(data4_2[(nrow(data)+1):nrow(data4_2), 'x'])
  
  data4_2['Model'] = 'exp'
  data4_2['Method'] = 'qqplot'
  data4_2 = data4_2[, c('t', 'Model', 'Method', 'St')]
  
  return(data4_2)
}  

# mle
gum.mle.pred = function(data, n){
  
  data2_3 = data.frame(data)
  for (i in 1:n){
    data2_3 = rbind(data2_3, NA)
  }
  data2_3['t'] = seq(data$t[1], data$t[length(data$t)]+n)
  
  mu = gum.mle(data)$mu
  sig = gum.mle(data)$sig
  m = gum.mle(data)$m
  
  data2_3['x'] = (data2_3$t-mu)/sig
  
  gumf = function(x){
    (m/sig)*(exp(-x)*(exp(-exp(-x))))
  }
  
  data2_3[(nrow(data)+1):nrow(data2_3), 'St'] = gumf(data2_3[(nrow(data)+1):nrow(data2_3), 'x'])
  
  data2_3['Model'] = 'gum'
  data2_3['Method'] = 'mle'
  data2_3 = data2_3[, c('t', 'Model', 'Method', 'St')]
  
  
  return(data2_3)
}

logis.mle.pred = function(data, n){
  
  data3_3 = data.frame(data)
  for (i in 1:n){
    data3_3 = rbind(data3_3, NA)
  }
  data3_3['t'] = seq(data$t[1], data$t[length(data$t)]+n)
  
  m = logis.mle(data)$m
  mu = logis.mle(data)$mu
  sig = logis.mle(data)$sig
  
  data3_3['x'] = (data3_3$t-mu)/sig
  
  logisf = function(x){
    (m/sig)*(exp(-x)/(1+exp(-x))^2)
  }
  
  data3_3[(nrow(data)+1):nrow(data3_3), 'St'] = logisf(data3_3[(nrow(data)+1):nrow(data3_3), 'x'])
  
  data3_3['Model'] = 'logis'
  data3_3['Method'] = 'mle'
  data3_3 = data3_3[, c('t', 'Model', 'Method', 'St')]
  
  return(data3_3)
}


##############################
param_result = function(data, model, method){
  if (method == 'ols'){
    if(model == 'bass'){
      return (bass.ols(data))
    }
    else if (model == 'gum'){
      return (gum.ols(data))
    }
    else if (model == 'logis'){
      return (logis.ols(data))
    }
    else if (model == 'exp'){
      return (exp.ols(data))
    }
  }
  else if (method == 'qqplot'){
    if(model == 'bass'){
      return (bass.qq(data))
    }
    else if (model == 'gum'){
      return (gum.qq(data))
    }
    else if (model == 'logis'){
      return (logis.qq(data))
    }
    else if (model == 'exp'){
      return (exp.qq(data))
    }
    
  }
  
  else if (method == 'mle'){
    if (model == 'bass'){
      return (NA)
    }
    else if (model == 'gum'){
      return (gum.mle(data))
    }
    else if (model == 'logis'){
      return (logis.mle(data))
    }
    else if (model == 'exp'){
      return (NA)
    }
    
  }
  
}
#########################
library(shiny)

### Themes ##############
mytheme <- create_theme(
  adminlte_color(
    light_blue = '#434C5E'
  ),
  adminlte_sidebar(
    dark_bg = '#2E3440',
    dark_hover_bg = '#81A1C1',
    dark_color = '#2E3440'
  ),
  adminlte_global(
    content_bg = '#FFF',
    box_bg = '#D8DEE9', 
    info_box_bg = '#D8DEE9'
  )
)
##########################

ui <- dashboardPage(
  
  dashboardHeader(title = 'Demand Prediction'),
  
  ################# sidebar ####################
  dashboardSidebar(
    
    
    fileInput('file1', 'Upload CSV File',
              multiple = FALSE,
              accept = c('text/csv',
                         'text/comma-separated-values,text/plain',
                         '.csv')),
    
    sliderInput('n', 'Prediction Horizon', 0, 100, value=10, step=1),
    
    radioButtons('model', 'Diffusion Model',
                 choices = c(Bass = 'bass',
                             Gumbel = 'gum',
                             Logistic = 'logis',
                             Exponential = 'exp'),
                 selected = 'bass'),
    
    radioButtons('method', 'Method',
                 choices = c(OLS = 'ols',
                             'Q-Q Plot' = 'qqplot',
                             MLE = 'mle'),
                 selected = 'ols'),
    
    checkboxInput('weekend_effect', 'Weekend effect', FALSE)),
  #################################################
  
  
  dashboardBody(
    use_theme(mytheme),
    
    fluidRow(valueBoxOutput('estimate_m'), 
             valueBoxOutput('estimate_p'), 
             valueBoxOutput('estimate_q')),
    
    fluidRow(
      box(title = NULL, status='primary', width=12, plotOutput('plot1'))),
    
    fluidRow(
      box(title = 'First 10 data', status = 'primary', width=2,
          tableOutput('contents')),
      box(title = 'Last 10 data', status = 'primary', width = 2,
          tableOutput('contents1')),
      box(title='Prediction', status = 'primary',width=2,
          tableOutput('pred_value_table')),
      box(title = NULL, status = 'primary', width=6,
          plotOutput('plot2'))
    )
    
  ))


# Define server logic to read selected file ----
server <- function(input, output) {
  
  Data <- reactive({
    req(input$file1)
    df = read.csv(input$file1$datapath)
    
    if (any(names(df) %in% c('date')) == TRUE){
      df$date = as.Date(df$date)
      
      if (input$weekend_effect == TRUE){
        for (i in 1:length(df$St)){
          if(wday(df$date[i]) == 1 | wday(df$date[i])==7){
            df$St[i] = df$St[i] *2/3}
          
          else{
            df$St[i] = df$St[i]
          }
        }
        df['Yt']=cumsum(df$St)
        df['Yt_1'] = lag(df$Yt, default=0)
        
      }
      else{
        df['Yt']=cumsum(df$St)
        df['Yt_1'] = lag(df$Yt, default=0)
        
      }
      
    }
    else{
      df['Yt'] = cumsum(df$St)
      df['Yt_1'] = lag(df$Yt, defalut=0)
    }
    
    return(df)
  })
  
  # Data Head
  output$contents <- renderTable(bordered=TRUE, style='border-color:#2E3440', hover=TRUE, {
    df = Data()
    df = df %>% select('t', 'St')
    head(df, 10)
  })
  
  output$contents1 <- renderTable(bordered=TRUE, style='border-color:#2E3440', hover=TRUE, {
    df = Data()
    df = df %>% select('t', 'St')
    tail(df, 10)
  })
  
  
  # Estimated model Parameters
  # output$esti_table
  
  # output$plot1
  output$plot1 <- renderPlot({
    df = Data()
    n = input$n
    bass_ols_pred = bass.ols.pred(df, n); gum_ols_pred = gum.ols.pred(df, n); logis_ols_pred = logis.ols.pred(df, n); exp_ols_pred = exp.ols.pred(df, n);
    bass_qq_pred = bass.qq.pred(df, n); gum_qq_pred = gum.qq.pred(df, n); logis_qq_pred = logis.qq.pred(df, n); exp_qq_pred = exp.qq.pred(df, n);
    gum_mle_pred = gum.mle.pred(df, n); logis_mle_pred = logis.mle.pred(df, n)
    pred_plot = rbind(bass_ols_pred, gum_ols_pred, logis_ols_pred, exp_ols_pred, 
                      bass_qq_pred, gum_qq_pred, logis_qq_pred, exp_qq_pred,
                      gum_mle_pred, logis_mle_pred)
    
    gg = ggplot(pred_plot[1:length(df$t), ]) +  # 기존 주어진 year에 따른 St는 모형과 상관없이 동일하므로
      geom_line(aes(t, St), size=1, color='navy') + theme_bw() + 
      geom_line(data=pred_plot[((pred_plot$Model %in% input$model) & (pred_plot$Method %in% input$method)),][(length(df$t)+1):(length(df$t)+n), ], aes(t, St), color='red', size=1.5) + 
      ggtitle('Time-Series plot with Predictions') + 
      theme(plot.title=element_text(hjust=0.5, size=18), axis.title = element_text(size=15)) + 
      ylab('St') 
    
    print(gg)
  })
  
  
  output$plot2 <- renderPlot({
    df = Data()
    n = input$n
    bass_ols_pred = bass.ols.pred(df, n); gum_ols_pred = gum.ols.pred(df, n); logis_ols_pred = logis.ols.pred(df, n); exp_ols_pred = exp.ols.pred(df, n);
    bass_qq_pred = bass.qq.pred(df, n); gum_qq_pred = gum.qq.pred(df, n); logis_qq_pred = logis.qq.pred(df, n); exp_qq_pred = exp.qq.pred(df, n);
    gum_mle_pred = gum.mle.pred(df, n); logis_mle_pred = logis.mle.pred(df, n)
    pred_plot = rbind(bass_ols_pred, gum_ols_pred, logis_ols_pred, exp_ols_pred, 
                      bass_qq_pred, gum_qq_pred, logis_qq_pred, exp_qq_pred,
                      gum_mle_pred, logis_mle_pred)
    
    gg1 = ggplot(pred_plot[(length(df$t)-5):length(df$t), ]) +  # 기존 주어진 year에 따른 St는 모형과 상관없이 동일하므로
      geom_line(aes(t, St), size=1, color='navy') + theme_bw() + 
      geom_line(data=pred_plot[((pred_plot$Model %in% input$model) & (pred_plot$Method %in% input$method)),][(length(df$t)+1):(length(df$t)+n), ], aes(t, St), color='red', size=1.5) + 
      theme(plot.title=element_text(hjust=0.5, size=18), axis.title = element_text(size=15)) + 
      ylab('St') 
    
    print(gg1)
  })
  
  
  
  # output$pred_value_table
  output$pred_value_table <- renderTable(bordered=TRUE, style='border-color:#337ab7', hover=TRUE, {
    df = Data()
    n = input$n
    bass_ols_pred = bass.ols.pred(df, n); gum_ols_pred = gum.ols.pred(df, n); logis_ols_pred = logis.ols.pred(df, n); exp_ols_pred = exp.ols.pred(df, n);
    
    bass_qq_pred = bass.qq.pred(df, n); gum_qq_pred = gum.qq.pred(df, n); logis_qq_pred = logis.qq.pred(df, n); exp_qq_pred = exp.qq.pred(df, n);
    
    gum_mle_pred = gum.mle.pred(df, n); logis_mle_pred = logis.mle.pred(df, n)
    
    pred_value = rbind(bass_ols_pred, gum_ols_pred, logis_ols_pred, exp_ols_pred,
                       bass_qq_pred, gum_qq_pred, logis_qq_pred, exp_qq_pred,
                       gum_mle_pred, logis_mle_pred)
    
    pred_value = pred_value[((pred_value$Model %in% input$model) & (pred_value$Method %in% input$method)), c('t', 'St')]
    pred_value[(nrow(df)+1):(nrow(df)+10), ]
  })
  
  
  ##########################
  output$estimate_m = renderValueBox({
    df = Data()
    valueBox(
      value = round(unlist(param_result(df, input$model, input$method))[1],4),
      'Market Potential',
      color = 'navy',
      icon = icon('fa-solid fa-user-group')
    )
  })
  
  output$estimate_p = renderValueBox({
    df = Data()
    valueBox(
      value = round(unlist(param_result(df, input$model, input$method))[2],4),
      'Coefficient of Innovation',
      color = 'teal',
      icon = icon('fa-solid fa-thumbs-up')
    )
  })
  
  output$estimate_q = renderValueBox({
    df = Data()
    valueBox(
      round(unlist(param_result(df, input$model, input$method))[3],4),
      'Coefficient of Imitation',
      color = 'aqua',
      icon = icon('fa-solid fa-tower-cell')
    )
  })
  
}








# Create Shiny app ----
shinyApp(ui, server)
