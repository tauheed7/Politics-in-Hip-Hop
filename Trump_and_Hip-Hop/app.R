#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(tidyverse)
library(wordcloud2)


candidates_in_hiphop <- read_csv("genius_hip_hop_lyrics_csv.csv")


trump_only <- subset(candidates_in_hiphop, candidate == "donald trump")

# Here I downloaded and assigned my data set again as it helps me to further process and manipulate the data for the app. 
#I know the data should for the most part be ready to go but I find working on the same page helps my thought process
#and the data set is not too large. 
#I've also wrote in a subsetted dataframe that was necessary for me later on. 


# Define UI for application that draws a histogram
ui <- navbarPage("Hip-Hop's Feelings Towards 2016 Presidential Candidates", theme = shinytheme("slate"),
                 
                # Here I provide my one sentence pitch and four sentence elevator pitch. I'm not sure how effective 
                #mine are as I've never done it before but appreciate that we get to practice this. 
                 
                 tabPanel("About This App", fluidPage(
                   titlePanel("Summary"),
                   h4("Hip-Hop hates Trump but loves Trump's money."),
                   p("In this app, I am analyzing references of the 2016 presidential candidates
                     in Hip-Hop music from 1989 up until 2016. To do this, I am examining song data from
                     Genius. I am looking to see which candidates were mentioned most, whether or not those 
                     references were positive, negative, or neutral, and what exactly
                     was said about the popular candidates. I expect mentions of Trump to not only 
                     dominate but also be overwhelmingly negative."),
                   p("The data I used is available below and is from the popular music and lyric website Genius."),
                   p("Github link: https://github.com/tauheed7/Politics-in-Hip-Hop"),
                   downloadButton(outputId = "download_data", label = "Download Data")
                   )), 
                
                # I thought it good practice to include a download option for my data. 
                
                # I decided to do a tab format because I think it is very effective and visually pleasing. 
                 
                 tabPanel("2016 Candidate References in Hip-Hop", fluidPage(
                   titlePanel("Mentions of Each Candidate in Hip-Hop Songs"), 
                   sidebarLayout(
                     sidebarPanel(
                       selectInput(inputId = "sentiment", 
                                   label = "Sentiment", 
                                   selected = "neutral",
                                   c("neutral", "positive", "negative"),
                                   multiple = FALSE)),
                     mainPanel(plotOutput("Plot"))),
                   sidebarLayout( 
                     sidebarPanel(
                       radioButtons(inputId = "candidate", 
                                    label = "Candidate", 
                                    c("donald trump", "hillary clinton")), 
                       checkboxGroupInput(inputId = "theme", 
                                          label = "Theme", 
                                          selected = "money",
                                          c("personal", "hotel", "money", 
                                            "political", "the apprentice", "n/a"))), 
                     mainPanel(plotOutput("Plot2"))))),
                 
                 tabPanel("Key Takeaway", fluidPage(
                   titlePanel("Trump's Popularity"), 
                   sidebarLayout(
                     sidebarPanel(
                       checkboxGroupInput(inputId = "sentiment2", 
                                          label = "Sentiment", 
                                          selected = "negative",
                                          c("positive (green)" = "positive", "negative (red)" = "negative"))),
                     mainPanel(plotOutput("Plot3"))), 
                   sidebarLayout(
                     sidebarPanel(
                       checkboxGroupInput(inputId = "theme2", 
                                          label = "Theme", 
                                          selected = "money",
                                          c("money (green)" = "money", "politics (red)" = "political"))),
                     mainPanel(plotOutput("Plot4"))))))
                   
  # For the app, I wanted to use a variety of inputs, which explains why I have check boxes, radio buttons, and a drop
  #down menu. I wanted to make sure I can create all of these and ensure my app is as interactive as I can make it. 





# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$download_data <- downloadHandler(
    filename = "genius_hip_hop_lyrics_csv.csv", 
    content = function(file) {write.csv(candidates_in_hiphop, file)})
  
  # I chose bar graphs for my second tab, which is the main part of my app, because I thought it would best represent the 
  #data visually. I have categorical variables that I am looking at a count for and so bar graphs made the most sense
  #to me. 
  
  output$Plot <- renderPlot({
    
    req(input$sentiment)
    
    data1 <- candidates_in_hiphop %>% filter(sentiment == input$sentiment) 
    
    ggplot(data1, aes(x = candidate)) + geom_bar(fill = "#9933FF") + xlab("Candidate") +
      ylab("Number of Times Mentioned") + ggtitle("Mentions of Each Candidate by Sentiment") + coord_flip()
    
    
    
  })
  
  output$Plot2 <- renderPlot({
    
    req(input$theme, input$candidate)
    
    data2 <- candidates_in_hiphop %>% filter(theme == input$theme, candidate == input$candidate)
    
    ggplot(data2, aes(x = sentiment, fill = theme)) + geom_bar() + 
      xlab("Sentiment of Trump/Clinton in Hip-Hop Songs") + ylab("Number of Times Mentioned") + 
      ggtitle("Trump & Clinton References by Sentiment and Theme")
  })
  
  output$Plot3 <- renderPlot({
    
    # For my third tab in order to get three different lines on a plot, it was a little tricky for me and I'm sure 
    #there was a better way to do it but using three different datasets and filtering out what I needed ended up 
    #working well for me.
    
    req(input$sentiment2)
    
    data3 <- trump_only %>% filter(sentiment == "positive") %>% filter(sentiment == input$sentiment2)
    data4 <- trump_only %>% filter(sentiment == "neutral") %>% filter(sentiment == input$sentiment2)
    data5 <- trump_only %>% filter(sentiment == "negative") %>% filter(sentiment == input$sentiment2)
    
    ggplot() + 
      geom_smooth(data = data3, aes(x = album_release_date, y = ..count..), stat = "bin", color = "green") + 
      geom_smooth(data = data5, aes(x = album_release_date, y = ..count..), stat = "bin", color = "red") + 
      xlab("Year") + ylab("Number of Trump Mentions") + 
      ggtitle("Positive and Negative Mentions of Trump Over Time")

  })
  
  output$Plot4 <- renderPlot({
  
  req(input$theme2)
  
    data6 <- trump_only %>% filter(theme == "money") %>% filter(theme == input$theme2)
    data7 <- trump_only %>% filter(theme == "political") %>% filter(theme == input$theme2)
    
    ggplot() + 
      geom_smooth(data = data6, aes(x = album_release_date, y = ..count..), stat = "bin", color = "green") +
      geom_smooth(data = data7, aes(x = album_release_date, y = ..count..), stat = "bin", color = "red") + 
      xlab("Year") + ylab("Number of Trump mentions") + ggtitle("References to Trump's Money vs. Politics over
                                                                time.")
    
    
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

