This is a final project for Theoretical Statistics II.
This is a code for R shiny application predicting the probability of coronary heart disease through Cox PHM and ALT.

Input: sex, age, height, weight, blood pressure, serum cholesterol
Output: probability of disease predicted by ALT and Cox PHM model, level of health's condition (BMI, BP, cholesterol), survival plot, cummulative hazard plot



options(encoding = 'UTF-8')

library(DT)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(fresh)
library(showtext)
require(showtext)
font_add_google(name='Nanum Gothic', regular.wt = 400, bold.wt=700)
showtext_auto()
showtext_opts(dpi=122)
library(flexdashboard)
library(readxl)
library(tidyverse)
library(kableExtra)
library(reshape)
library(ggpubr)
library(gridExtra)
library(MASS)
library(survival)
library(survminer)
library(ggfortify)
library(dashboardthemes)
library(GGally)
library(gtsummary)
library(fastDummies)



header = dashboardHeader(title = 'Heart Disease Prob')

sidebar = dashboardSidebar(
  collapsed = TRUE)

body = dashboardBody(
  tabsetPanel(
    tabPanel("Probability",
  h3("Input"),
  
  fluidRow(
    box(
      column(3,
             id='type',
             textInput('name', h5('이름'), value='유나영'),
             sliderInput('age', h5('나이'), value=45, min=30, max=100 ),
             radioButtons('sex', h5('성별'), choices=list('남자'=1,
                                                        '여자'=0), selected=0)
      ),
      
      column(4,
             numericInput('height', h5('키(cm)'),value=163),
             numericInput('weight', h5('몸무게(kg)'),value=50),
             numericInput('month', h5('기본 검사가 이루어진 시기(월)'), value=6)
      ), 
      
      column(4,       
             sliderInput('sbp', h5('최대혈압(mm Hg)'), value=100, min = 0, max = 300),
             sliderInput('dbp', h5('최소혈압(mm Hg)'), value=80, min=0, max=300),
             sliderInput('scl', h5('혈청콜레스테롤 (mg/100ml)'), value=200,min=0,max=600)
             
             
      ), height = 350, width = 12, status="danger")),
  
  h3("Output"),
  
  fluidRow(
    box(
      box(column(h3("ALT"),flexdashboard::gaugeOutput('p.chart'), width = 6),
          column(h3("CoxPHM"),flexdashboard::gaugeOutput('p.chart2'), width = 6),
          shinydashboard::valueBoxOutput('bmi', width=12)
      ),
      column(6,
             br(),
             shinydashboard::valueBoxOutput('health', width=8),
             shinydashboard::valueBoxOutput('text', width=8), 
             shinydashboard::valueBoxOutput('chol', width=8)
      )
      
      , height = 400, width = 12, status="danger"))),
  
  tabPanel( "Plot",
  fluidRow(
    box(
        column(6, h3("Survival Plot"), plotOutput('splot')),
        column(6, h3("Cummulative Hazard Plot"), plotOutput('hplot')), height=500, width=12)
  ))
  
))



ui = dashboardPage(header,sidebar, body, shinyDashboardThemes(theme='grey_dark'))


df = read_excel('Framingham2.xls', sheet=1)
df = df[,-1]
df$sex = as.factor(df$sex)
df$month = as.factor(df$month)
# missing value: skewed->replce with median
df$scl = ifelse(is.na(df$scl), median(df$scl, na.rm=T), df$scl)
df$bmi = ifelse(is.na(df$bmi), median(df$bmi, na.rm=T), df$bmi)
# outlier delete
mlist = c('sbp', 'dbp', 'scl', 'bmi')
for (i in mlist){
  lower = fivenum(df[[i]])[2]
  upper = fivenum(df[[i]])[4]
  iqr = IQR(df[[i]])
  df = df[which(!((df[[i]]<lower-1.5*iqr)|(df[[i]]>upper+1.5*iqr))), ]
}
# grouping with age
df$age = as.factor(floor(df$age/10)*10)
# skewed->log
df$sbp = log(df$sbp)
df$dbp = log(df$dbp)
df$scl = log(df$scl)
df$bmi = log(df$bmi)
meansbp = mean(df$sbp)
meandbp = mean(df$dbp)
meanscl = mean(df$scl)
meanbmi = mean(df$bmi)

df[,c(1:3,5)] = scale(df[,c(1:3,5)], center=TRUE, scale=FALSE)

data.setting=function(df2){
  
  df2$sex = as.factor(df2$sex)
  df2$month = as.factor(df2$month)
  
  # grouping with age
  df2$age = as.factor(floor(df2$age/10)*10)
  
  # skewed->log
  df2$sbp = log(df2$sbp)
  df2$dbp = log(df2$dbp)
  df2$scl = log(df2$scl)
  df2$bmi = log(df2$bmi)
  
  df2$sbp = df2$sbp-meansbp # /sd(df$sbp)
  df2$dbp = df2$dbp-meandbp # /sd(df$dbp)
  df2$scl = df2$scl-meanscl # /sd(df$scl)
  df2$bmi = df2$bmi-meanbmi # /sd(df$bmi)
  
  return(df2)
}

alt = survreg(formula = Surv(followup, chdfate) ~ sbp + scl + age + 
                bmi + sex + scl:age + scl:sex, data=df, dist = 'weibull')

sigma.alt = alt$scale

cox=coxph(Surv(followup, chdfate) ~ sbp + dbp + scl + age + 
            bmi + sex + sbp:scl + sbp:age + dbp:scl + dbp:age + scl:age + 
            scl:sex, data=df)

S = basehaz(cox)
lm = lm(log(hazard)~log(time), data = S)
alpha = summary(lm)$coefficients[1]
beta = summary(lm)$coefficients[2]

p.function=function(newdata){
  mu.alt = predict(alt, type='lp', newdata=newdata)
  u.alt = (log(3650)-mu.alt)/sigma.alt
  alt.p=(1-exp(-exp(u.alt)))
  
  mu.cox = predict(cox,type = 'risk',newdata=newdata)
  cox.p=1-exp(-exp((alpha+beta*(log(3650))))*mu.cox)
  p=list(ALT=alt.p,CoxPHM=cox.p)
  return(p)
}




server = function(input, output, session){
  
  
  out = reactive({
    df=data.frame(sbp=input$sbp, dbp=input$dbp, scl=input$scl, age=input$age,
                  bmi=input$weight/((input$height)*0.01)^2, month=as.factor(input$month), sex=as.factor(input$sex))
    df2 = data.setting(df)
    res = p.function(df2)
    bmi=df$bmi
    scl=df$scl
    dbp=df$dbp
    sbp=df$sbp
    return(list(data=df, p.cox=res$CoxPHM, p.alt=res$ALT ,bmi=bmi, scl=scl, dbp=dbp, sbp=sbp, df2=df2))
  }
  )
  
  output$p.chart = renderGauge({
    out = out()
    rate = round(out$p.alt,3)*100
    gauge(as.numeric(rate) ,min = 0, max = 100, symbol = '%',  label= 'ALT', 
          gaugeSectors(
            success = c(0,30), warning = c(30,60), danger = c(60,100)
          )) 
  })
  
  output$p.chart2 =renderGauge({
    gauge(as.numeric(round(out()$p.cox,3)*100) ,min = 0, max = 100, symbol = '%', label= 'CoxPHM',
          gaugeSectors(
            success = c(0,30), warning = c(30,60), danger = c(60,100)
          )) 
  })
  
  
  output$bmi = shinydashboard::renderValueBox({
    shinydashboard::valueBox({out = out()
    print(tags$p(round(out$bmi,1), style="font-size: 50%;"))},
    h5(paste0(input$name,'의 BMI')), color='teal', icon=tags$i(class = "fas fa-weight-scale", style="font-size: 36px; color: white"))
  })
  
  output$health = shinydashboard::renderValueBox({
    out=out()
    if ((out$bmi)<=18.5){
      mycolor='yellow'
      myicon=tags$i(class = "fas fa-utensils", style="font-size: 36px; color: white")
    }
    else if (18.5<(out$bmi)&(out$bmi)<25){
      mycolor='green'
      myicon=tags$i(class = "fas fa-thumbs-up", style="font-size: 36px; color: white")
    }
    else if (25<=(out$bmi)&(out$bmi)<30){
      mycolor='orange'
      myicon=tags$i(class = "fas fa-thumbs-down", style="font-size: 36px; color: white")
    }
    else{
      mycolor='red'
      myicon=tags$i(class = "fas fa-skull-crossbones", style="font-size: 36px; color: white")
      
    }
    shinydashboard::valueBox({out = out()
    if ((out$bmi)<=18.5){
      print(tags$p('저체중입니다.', style="font-size: 50%;"))
      
    }
    else if (18.5<(out$bmi)&(out$bmi)<25){
      print(tags$p('건강합니다^^', style="font-size: 50%;"))
      
    }
    else if (25<=(out$bmi)&(out$bmi)<30){
      print(tags$p('과체중입니다.', style="font-size: 50%;"))
      
    }
    else{
      print(tags$p('비만입니다.', style="font-size: 50%;"))
    }
    
    }, h5('BMI 상태'), color=mycolor,  icon=myicon)
  })
  
  output$text=shinydashboard::renderValueBox({
    out=out()
    if (out$sbp >= 180 &out$dbp >=120){
      mycolor='red'
      myicon=tags$i(class = "fas fa-stethoscope", style="font-size: 36px; color: white")
    }
    else if (out$sbp>=130&out$sbp<180 | out$dbp<120&out$dbp > 80){
      mycolor='yellow'
      myicon=tags$i(class = "fas fa-thumbs-down", style="font-size: 36px; color: white")
    }
    else{
      mycolor='green'
      myicon=tags$i(class = "fas fa-thumbs-up", style="font-size: 36px; color: white")
    }
    
    shinydashboard::valueBox({out=out()
    if (out$sbp >= 180 &out$dbp >=120){
      print(tags$p('혈압이 너무 높습니다. 병원 가세요.', style="font-size: 50%;"))
    }
    else if (out$sbp>=130&out$sbp<180 | out$dbp<120&out$dbp>80){
      print(tags$p('혈압이 조금 높습니다. 주의하세요.', style="font-size: 50%;"))
    }
    else {
      print(tags$p('건강합니다^^', style="font-size: 50%;"))
    }
    
    }, h5('혈압 상태'), color=mycolor, icon=myicon)
  })
  
  output$chol=shinydashboard::renderValueBox({
    out=out()
    if (out$scl >= 240){
      mycolor='red'
      myicon=tags$i(class = "fas fa-stethoscope", style="font-size: 36px; color: white")
    }
    else if (out$scl>=200&out$scl<240){
      mycolor='yellow'
      myicon=tags$i(class = "fas fa-thumbs-down", style="font-size: 36px; color: white")
    }
    else{
      mycolor='green'
      myicon=tags$i(class = "fas fa-thumbs-up", style="font-size: 36px; color: white")
    }
    
    shinydashboard::valueBox({out=out()
    if (out$scl >= 240){
      print(tags$p('콜레스테롤이 너무 높아요. 병원 가세요.', style="font-size: 50%;"))
    }
    else if (out$scl>=200&out$scl<240){
      print(tags$p('콜레스테롤이 조금 높아요. 주의하세요.', style="font-size: 50%;"))
    }
    else {
      print(tags$p('건강합니다^^', style="font-size: 50%;"))
    }
    }, h5('콜레스테롤 상태'),color=mycolor, icon=myicon)
  })
  
  output$splot=renderPlot({
    out=out()
    ggsurvplot(survfit(cox), data=out$df2, conf.int = FALSE)
  })
  
  output$hplot=renderPlot({
    out=out()
    ggsurvplot(survfit(cox), fun='cumhaz', data=out$df2, conf.int = FALSE, palette = 'skyblue3')
    #plot(ecdf(predict(cox, data=out$df2)))
  })
}

shinyApp(ui=ui, server=server)
