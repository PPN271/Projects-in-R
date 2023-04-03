# ===============================================
# Fill in the following fields
# ===============================================
# Title:
# Description:
# Author: 
# Date:


# ===============================================
# Packages
# ===============================================
library(tidyverse)
library(tidytext)


# ===============================================
# Import data
# ===============================================
# for demo purposes of the "template", we use data starwars
# (but you will have to replace this with the data in "u2-lyrics.csv")
dat <- dplyr::starwars


# ===============================================
# Define UserInterface "ui" for application
# ===============================================

ui <- fluidPage(
  
  titlePanel("Title of your app"),
  fluidRow(
    # replace with your widgets
    column(3,
           p(em("Input widgets")),
           radioButtons(inputId = "choose", 
                        label = "Choose one option", 
                        choices = c("option 1" = "opt1",
                                    "option 2" = "opt2",
                                    "option 3" = "opt3"), 
                        selected = "opt1")
    ),
    
    # replace with your widgets
    column(3,
           p(em("Input widgets")),
           selectInput(inputId = "numbers", 
                       label = "Make a selection",
                       choices = c("optional" = "num_opt",
                                   "only numbers" = "num_yes",
                                   "no numbers" = "num_no"),
                       selected = "num_opt")
    ),
    
    # replace with your widgets
    column(3,
           p(em("Input widgets")),
           radioButtons(inputId = "arrange", 
                        label = "Order bars by:", 
                        choices = c("decreasing freq" = "arr_dec",
                                    "increasing freq" = "arr_inc",
                                    "alphabetical a-z" = "arr_a2z",
                                    "alphabetical z-a" = "arr_z2a"),
                        selected = "arr_dec")
    ),
    
    # replace with your widgets
    column(3,
           p(em("Input widgets")),
           sliderInput(inputId = "binwidth",
                       label = "Binwidth",
                       min = 1,
                       max = 20,
                       value = 1),
           checkboxInput(inputId = "facets",
                         label = strong("Facet by letter"),
                         value = FALSE)
    )
  ),
  hr(),
  
  tabsetPanel(type = "tabs",
              tabPanel("Analysis1",
                       h3("What kind of analysis1?"),
                       plotOutput("barplot"),
                       hr(),
                       dataTableOutput('table1')),
              tabPanel("Analysis2", 
                       h3("What kind of analysis2"),
                       plotOutput("histogram"),
                       hr(),
                       verbatimTextOutput('table2'))
  )
)


# ===============================================
# Define Server "server" logic
# ===============================================

server <- function(input, output) {
  
  # you may need to create reactive objects
  # (e.g. data frame to be used in barchart)
  dat_freq <- reactive({
    dat %>% group_by(sex) %>% count()
  })
  
  
  # ===============================================
  # Outputs for the first TAB (i.e. barchart)
  # ===============================================
  
  # code for barplot
  output$barplot <- renderPlot({
    # replace the code below with your code!!!
    ggplot(data = dat_freq(), aes(x = sex, y = n)) +
      geom_col()
  })
  
  # code for numeric summaries of frequencies
  output$table1 <- renderDataTable({
    # replace the code below with your code!!!
    dat_freq()
  })
  
  
  # ===============================================
  # Outputs for the second TAB (i.e. histogram)
  # ===============================================
  
  # code for histogram
  output$histogram <- renderPlot({
    # replace the code below with your code!!!
    ggplot(data = dat, aes(x = height)) +
      geom_histogram(binwidth = 20)
  })
  
  # code for statistics
  output$table2 <- renderPrint({
    # replace the code below with your code!!!
    summary(dat$height)
  })
  
}



# ===============================================
# Run the application
# ===============================================

shinyApp(ui = ui, server = server)

