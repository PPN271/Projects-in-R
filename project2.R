# Stat 133, Fall 2021
# Author: Shengnan Li
# Description: Shiny app that computes a data table and timeline of simulated retirement withdrawal, 
#     and graphs their timeline, using random rates of return.
# Inputs:
# - initial: present value (or initial amount)
# - age: retirement age
# - rate: withdrawal rate
# - sims: number of simulations
# - rate_r: average annual rate of return
# - rate_sd: average return volatility
# - inflation_i: average inflation rate
# - inflation_sd: average inflation volatility
# - seed: value of the random seed
# Details: the rates of return and inflation are generated with a normal distribution
# Note: code used for project 2

library(shiny)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(rsconnect)

ui <- fluidPage(

  titlePanel("Retirement Wealth Simulator"),
  fluidRow(
    column(3,
           h4("Column1"),
      numericInput(inputId = "initial",
                   label = "Initial Portfolio ($)",
                   min = 1,
                   value = 1000000),
      sliderInput(inputId = 'age', 
                  label = 'Retirement Age', 
                  min = 0, 
                  max = 100,
                  value = 60,
                  step = 1),

      sliderInput(inputId = "rate",
                   label = "Withdrawal Rate(in %)",
                   min = 0,
                   max = 0.2,
                   value = 0.04,
                   step = 0.01),
      ),
    
      column(3,
             h4("Column2"),
             sliderInput(inputId = 'rate_r', 
                         label = 'Avg annual return(in %)', 
                         min = 0, 
                         max = 0.2,
                         value = 0.10,
                         step = 0.01),
             sliderInput(inputId = 'rate_sd', 
                         label = 'Avg return volatility(in %)', 
                         min = 0, 
                         max = 0.25,
                         value = 0.18,
                         step = 0.01),
      ),
    column(3,
           
           h4("Column3"),
           sliderInput(inputId = 'inflation_i', 
                       label = 'Avg annual inflation(in %)', 
                       min = 0, 
                       max = 0.2,
                       value = 0.03,
                       step = 0.01),
           sliderInput(inputId = 'inflation_sd', 
                       label = 'Avg inflation volatility(in %)', 
                       min = 0, 
                       max = 0.2,
                       value = 0.035,
                       step = 0.01),
    ),
    column(3,
           h4("Column4"),
           numericInput(inputId = "sims",
                        label = "Number of simulations",
                        min = 1,
                        max = 100,
                        value = 50,
                        step = 1),
           numericInput(inputId = "seed", 
                        label = "Random seed",
                        value = 12345,
                        step = 1),
  ),

 mainPanel(
   hr(),
   hr(),
  h4('Retriment Withdrawal Timeline'),
  h5(' Median line is blue. 10-th and 90-th percentiles are reference line(colored yellow).               '),
    plotOutput('plot'),
  
  
  hr(),
  h4('Summary'),
  verbatimTextOutput('table'),
  
  
 #hr(),
 # h4('Statistics'),
 # verbatimTextOutput('tbl')

)
)
)


server <- function(input, output) {
  portfolio <- reactive({
    set.seed(input$seed)
    portfolio_list = as.list(1:input$sims)
    names(portfolio_list) = paste0("sim", 1:input$sims)

    
for(s in 1:input$sims){
     year = 1:(100 - input$age)
    withdraw  <- (input$initial/1000000)  *input$rate
    value_left <- (input$initial/1000000) * (1 - input$rate)
    total <- input$initial/1000000
    
    for(y in 1 : (100 - input$age)) {
    r = rnorm(1, input$rate_r, input$rate_sd)
    i = rnorm(1, input$inflation_i, input$inflation_sd)
         
    balance <- value_left  * (1 + r)
    value_left <- balance - withdraw * (1 + i)
    total <- c(total, balance)
    }    　　　
     portfolio_list[[s]] = total
     }
    
    portfolio_dat = data.frame(portfolio_list)
       
    portfolio_dat$years = 0:(100 - input$age)
       
       pivot_longer(
         portfolio_dat,
         cols = starts_with("sim"),
         names_to = "simulation",
         values_to = "amount")
       
  })   
  output$plot <- renderPlot({
    qt_10 <- portfolio() %>% group_by(years) %>% summarise(tenth = quantile(amount, 0.1))
    qt_90 <- portfolio() %>% group_by(years) %>% summarise(ninth = quantile(amount, 0.9))
    qt_50 <- portfolio() %>% group_by(years) %>% summarise(fifth = quantile(amount, 0.5))
    ggplot(data.frame()) +
      geom_line(data = portfolio(), aes(x = years, y = amount, group = simulation), alpha = 0.3) + 
      geom_line(data = qt_10, aes(x = years, y = tenth), size = 1.5, method = 'rqss', color = "yellow") +
      geom_line(data = qt_90, aes(x = years, y = ninth),  size = 1.5, method = 'rqss',color = "yellow") +
      geom_line(data = qt_50, aes(x = years, y = fifth),  size = 1.5, method = 'rqss',color = "blue") +
      geom_line(data = portfolio(), aes(x = years, y = 0), color = "red") +
      theme_minimal() +
      ylab("Protfolio balance(millions)") + 
      xlab("Years till reaching age 100") 
  })
  
  output$table <- renderPrint({
    summary(portfolio())
  })
  

}
shinyApp(ui = ui, server = server)
  