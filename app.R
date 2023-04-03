# ===============================================
# Fill in the following fields
# ===============================================
# Title:
# Description:
# Author: 
# Date:


# ===============================================
# Required packages
# ===============================================
library(tidyverse)
# ...



# ===============================================
# Define UserInterface "ui" for application
# ===============================================

ui <- fluidPage(
  
  titlePanel("Title of your app"),
  fluidRow(
    # Inputs for initial portfolio, retirement age, and withdrawal rate
    column(3,
           h4("column1")  # delete this line and replace it with your widgets!!!
    ),
    
    # Inputs for mean and standard deviation of annual return rates
    column(3,
           h4("column2")  # delete this line and replace it with your widgets!!!
    ),
    
    # Inputs for mean and standard deviation of annual inflation rates
    column(3,
           h4("column3")  # delete this line and replace it with your widgets!!!
    ),
    
    # Inputs for number of simulations, and random seed
    column(3,
           h4("column4")  # delete this line and replace it with your widgets!!!
    )
  ),
  
  hr(),
  h4('Dummy text for graph'),
  plotOutput('plot'),
  
  hr(),
  h4('Dummy text for statistics'),
  verbatimTextOutput('table')
)



# ===============================================
# Define Server "server" logic
# ===============================================

server <- function(input, output) {

  # you may need to create reactive objects
  # (e.g. data frame to be used for graphing purposes)
  dat <- reactive({
    # replace the code below with your code!!!
    data.frame(
      MPG = mtcars$mpg, 
      HP = mtcars$hp
    )
  })
  
  
  # code for graph
  # (e.g. reactive data frame used for graphing purposes)
  output$plot <- renderPlot({
    # replace the code below with your code!!!
    ggplot(data = dat(), aes(x = HP, y = MPG)) +
      geom_point()
  })
  
  
  # code for statistics
  output$table <- renderPrint({
    # replace the code below with your code!!!
    summary(dat())
  })

}



# ===============================================
# Run the application
# ===============================================

shinyApp(ui = ui, server = server)

