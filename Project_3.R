# ===============================================
# Fill in the following fields
# ===============================================
# Title:Analysis of U2’s Song Lyrics
# Description: This is a program for analysis of U2’s Song Lyrics
# Author: Shengnan Li
# Date:12/01/2021


# ===============================================
# Packages
# ===============================================
library(tidyverse)
library(tidytext)
library(wordcloud) 
library(RColorBrewer) 
library(igraph)
library(ggraph) 
library(ggplot2)
library(dplyr)
# ===============================================
# Import data
# ===============================================



dat <- read.csv(file = 'u2-lyrics.csv')
u2_dat <- data.frame(text = dat)
albums <- unique(dat$album)




# ===============================================
# Define UserInterface "ui" for application
# ===============================================

ui <- fluidPage(
  
  titlePanel("Analysis of U2’s Song Lyrics"),
  fluidRow(
    column(3,
       # code for analysis 1 graph 1
                  p(em("Analysis 1 graph 1")),
                  sliderInput(inputId = "i",
                              label = "Top __ most frequent words used in U2 lyrics",
                              min = 1,
                              max = 50,
                              value = 10),
           checkboxInput(inputId = "col1",
                         label = strong("Remove Stopwords"),
                         value = TRUE)
    ),
    
    # code for analysis 1 graph 2
    column(3,
           p(em("Analysis 1 graph 2")),
           selectInput(inputId = 'album', 
                       label = "Please select an album",
                       choices = c(albums),
                       selected = albums[1]),
 
           sliderInput(inputId = "r",
                       label = "Top __ most frequent words used in the selected album",
                       min = 1,
                       max = 30,
                       value = 10),
           checkboxInput(inputId = "col2",
                         label = strong("Remove Stopwords"),
                         value = TRUE)
          
    ),
    
    # code for analysis 1 graph 3
    column(3,
           p(em("Analysis 1 graph 3")),
           sliderInput(inputId = "j",
                       label = "Frequent words per album",
                       min = 1,
                       max = 20,
                       value = 5),
           checkboxInput(inputId = "col3",
                         label = strong("Remove Stopwords"),
                         value = TRUE),
           
    ),
    
    # code for analysis 2 
    column(3,
           p(em("Analysis 2")),
           numericInput(inputId = "seed", 
                        label = "Random seed",
                        value = 12345,
                        step = 1),
           sliderInput(inputId = "q",
                       label = "Top _ bigrams",
                       min = 1,
                       max = 30,
                       value = 5),
           sliderInput(inputId = "p",
                       label = "Common bigrams over the number _",
                       min = 1,
                       max = 30,
                       value = 5)

    )
  ),
  hr(),
  
  tabsetPanel(type = "tabs",
              tabPanel("Analysis1",
                       h3("Word Frequency Analysis"),
                       plotOutput("barplot1"),
                       hr(),
                       plotOutput("barplot2"),
                       hr(),
                       plotOutput("barplot3"),
                       hr(),
                       dataTableOutput('table1')),
              tabPanel("Analysis2", 
                       h3("Bigram Analysis"),
                       plotOutput("histogram"),
                       hr(),
                       plotOutput("graph"))
  )
)


# ===============================================
# Define Server "server" logic
# ===============================================

server <- function(input, output) {
  #code for first graph
  dat_A1 <- reactive({
   lyrics_tokens = unnest_tokens(tbl = u2_dat, output = word, input = text.lyrics)
    lyrics_freqs = lyrics_tokens %>% count(word)
    lyrics_top20_words = lyrics_freqs %>% 
      arrange(desc(n)) %>%
      slice_head(n = input$i)
    })
   
  #code for second graph
    dat_A2 <- reactive({
      album_1 <- slice(filter(u2_dat, text.album == input$album))
      album_1_tokens = unnest_tokens(tbl = album_1, output = word, input = text.lyrics)
      album_1_freqs = album_1_tokens %>% count(word)
      album_1_top20_words = album_1_freqs %>% 
      arrange(desc(n)) %>%
      slice_head(n = input$r)
      tidy_album_1_top20_words = album_1_tokens %>% 
          count(word)
         tidy_album_1_top20_words %>%
           arrange(desc(n)) %>%
           slice_head(n = input$r) 
      
    })
    #code for second graph
    dat_A22 <- reactive({
      album_1 <- slice(filter(u2_dat, text.album == input$album))
      album_1_tokens = unnest_tokens(tbl = album_1, output = word, input = text.lyrics)
      album_1_freqs = album_1_tokens %>% count(word)
      album_1_top20_words = album_1_freqs %>% 
        arrange(desc(n)) %>%
        slice_head(n = input$r)
      tidy_album_1 = album_1_tokens %>% 
        anti_join(stop_words, by = "word") %>%
       count(word)
      tidy_album_1 %>%
       arrange(desc(n)) %>%
      slice_head(n = input$r) 
    
    })
#code for third graph
    dat_A3 <- reactive({
      lyrics_tokens = unnest_tokens(tbl = u2_dat, output = word, input = text.lyrics)
   })
    #code for analysis 2
    datC <- reactive({
      set.seed(input$seed)
      u2_ly <- select(u2_dat, text.lyrics)
      u2_bigrams <- u2_ly %>%
        unnest_tokens(output = bigram, input = text.lyrics, token = "ngrams", n = 2) %>%
        filter(!is.na(bigram))
      count_bigrams <- u2_bigrams %>%
        count(bigram, sort = TRUE)
      
      bigrams_separated <- u2_bigrams %>%
        separate(bigram, c("word1", "word2"), sep = " ")
      
      bigrams_filtered <- bigrams_separated %>%
        filter(!word1 %in% stop_words$word) %>%
        filter(!word2 %in% stop_words$word)
      
      count_bigrams <- bigrams_filtered %>%
        count(word1, word2, sort = TRUE)
      
      bigrams_united = count_bigrams %>%
        unite(bigram, word1, word2, sep = " ")
      
      bigrams20 <- slice(bigrams_united, 1:input$q)
    })
  
    
    datC1 <- reactive({
      datC()
      bigrams_graph <- count_bigrams %>%
        filter(n > input$p) %>%
        graph_from_data_frame()
    })

 
    
   

  
  # ===============================================
  # Outputs for the first TAB (i.e. barchart)
  # ===============================================
  

  output$barplot1 <- renderPlot({
  
# code for histogram 1 in analysis 1  
if(input$col1 == FALSE){
  tidy_lyrics_top20_words = lyrics_tokens %>% 
    count(word)
  tidy_lyrics_top20_words %>%
    arrange(desc(n)) %>%
    slice_head(n = input$i) %>%
    ggplot(aes(x = reorder(word, n), y = n)) +
    geom_col() + 
    labs(title = "Most frequent words in U2 lyrics") +
    xlab("word") +
    ylab("count") + 
    coord_flip() +
    theme_minimal()
  
}
    
  else{
    tidy_lyrics = lyrics_tokens %>% 
      anti_join(stop_words, by = "word") %>%
      count(word)
    tidy_lyrics %>%
      arrange(desc(n)) %>%
      slice_head(n = input$i) %>%
      
      
      ggplot(aes(x = reorder(word, n), y = n)) +
      geom_col() + 
      labs(title = "Most frequent words in U2 lyrics", 
           subtitle = "(after removing stopwords)") +
      xlab("word") +
      ylab("count") + 
      # plot(y~n, yaxt="none", main="Bold y-axis tick labels") +
      coord_flip() +
      theme_minimal()
    
  } 
    
    
  }) 
  
    # code for histogram 2 in analysis 1 
    output$barplot2 <- renderPlot({
      
      if(input$col2 == FALSE){
          ggplot(dat_A2(), aes(x = reorder(word, n), y = n)) +
          geom_col() + 
          labs(title = "Most frequent words in a given album") +
          xlab("word") +
          ylab("count") + 
          coord_flip() +
          theme_minimal()
      }
      else{

          ggplot(dat_A22(), aes(x = reorder(word, n), y = n)) +
          geom_col() + 
          labs(title = "Most frequent words in a given album", 
               subtitle = "(after removing stopwords)") +
          xlab("word") +
          ylab("count") + 
          coord_flip() +
          theme_minimal()
        
      } 

  })
    
    # code for histogram 2 in analysis 1 
    output$barplot3 <- renderPlot({
     if(input$col3 == FALSE){
       tidy_lyrics <- lyrics_tokens 
       
       lyrics_words <- tidy_lyrics %>%
         count(text.album, word, sort = TRUE) %>%
         ungroup()
       
       lyrics_words %>%
         arrange(desc(n)) %>%
         group_by(text.album) %>%
         top_n(input$j) %>%
         ggplot() +
         geom_col(aes(reorder_within(word, n, text.album), n)) +
         scale_x_reordered() +
         facet_wrap(~ text.album, scales = "free") + 
         xlab(NULL) + 
         coord_flip() +
         labs(title = "Most common words by album")
     }
      else{
        tidy_lyrics <- lyrics_tokens %>%
          anti_join(stop_words, by = "word")
        
        lyrics_words <- tidy_lyrics %>%
          count(text.album, word, sort = TRUE) %>%
          ungroup()
        
        lyrics_words %>%
          arrange(desc(n)) %>%
          group_by(text.album) %>%
          top_n(input$j) %>%
          ggplot() +
          geom_col(aes(reorder_within(word, n, text.album), n)) +
          scale_x_reordered() +
          facet_wrap(~ text.album, scales = "free") + 
          xlab(NULL) + 
          coord_flip() +
          labs(title = "Most common words by album")
        
      }
    })
    #code for summary in analysis 1
      output$table1 <- renderDataTable({
        dat_A1()
      })
      
 
    output$histogram <- renderPlot({    
      ggplot(data =  datC()) +
        geom_col(aes(x = reorder(bigram, n), y = n)) +
        coord_flip() + 
        labs(title = "Most frequent bigrams ") +
        xlab("bigram") +
        ylab("count") +
        theme_minimal()
      
    })
  
  
  # ===============================================
  # Outputs for the second TAB (i.e. histogram)
  # ===============================================
  
  # code for last graph in analysis 2

  
  output$graph <- renderPlot({  
    ggraph( datC1(), layout = "fr") +
      geom_edge_link() +
      geom_node_point() +
      geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
      labs(title = "Common bigrams") +
      theme_void() + 
      xlim(c(10, 19))   
  
  })

}



# ===============================================
# Run the application
# ===============================================

shinyApp(ui = ui, server = server)

