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


candidates_in_hiphop <- read_csv("genius_hip_hop_lyrics_csv.csv")


positive_sentiment <- subset(candidates_in_hiphop, sentiment == "positive")


trump_only <- subset(candidates_in_hiphop, candidate == "donald trump")


trump_positive <- subset(trump_only, sentiment == "positive")


trump_negative <- subset(trump_only, sentiment == "negative")


trump_neutral <- subset(trump_only, sentiment == "neutral")

candidate_choices <- candidates_in_hiphop %>% 
  group_by(candidate) %>% summarize()

sentiment_choices <- candidates_in_hiphop %>% 
  group_by(sentiment) %>% summarize()

theme_choices <- candidates_in_hiphop %>% 
  group_by(theme) %>% summarize()


# Define UI for application that draws a histogram
ui <- navbarPage("Hip-Hop's Feelings Towards 2016 Presidential Candidates", theme = shinytheme("slate"),
                 
                 
                 
                 tabPanel("About This App", fluidPage(
                   titlePanel("Summary"),
                   p("Hip-Hop loves Trump's money but hates Trump."),
                   p("In this app, I am analyzing mentions of the 2016 presidential candidates
                     in Hip-Hop music up until 2016. To do this, I am examining song data from
                     Genius. I am looking to see which candidates were mentioned most and what 
                     was said about the candidates. I expect mentions of Trump to not only 
                     dominate but also be overwhelmingly negative."),
                   p("Github link: https://github.com/tauheed7/final_project_work")
                   )), 
                 tabPanel("2016 Candidates in Hip-Hop", fluidPage(
                   titlePanel("Mentions of Each Candidate in Hip-Hop Songs"), 
                   sidebarLayout(
                     sidebarPanel(
                       selectInput(inputId = "sentiment", 
                                   label = "Sentiment", 
                                   selected = "neutral",
                                   c("neutral" = "neutral", "positive" = "positive", "negative" = "negative"),
                                   multiple = TRUE)),
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
                     mainPanel(plotOutput("Plot2"))))))






# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$Plot <- renderPlot({
    
    req(input$sentiment)
    
    data1 <- candidates_in_hiphop %>% filter(sentiment == input$sentiment)
    
    ggplot(data1, aes(x = candidate, fill = candidate)) + geom_bar() + 
      ylab("Number of Times Mentioned") + ggtitle("Mentions of Each Candidate by Sentiment")
    
  })
  
  output$Plot2 <- renderPlot({
    
    req(input$theme, input$candidate)
    
    data2 <- candidates_in_hiphop %>% filter(theme == input$theme, candidate == input$candidate)
    
    ggplot(data2, aes(x = sentiment, fill = theme)) + geom_bar() + 
      xlab("Sentiment of Donald Trump in a Hip Hop Song") + ylab("Number of Times Mentioned") + 
      ggtitle("Trump & Clinton References by Sentiment and Theme")
  })
  
  output$Plot3 <- renderPlot({
    
    ggplot() + 
      geom_smooth(data = trump_positive, aes(x = album_release_date, y = ..count..), stat = "bin", color = "green") +
      geom_smooth(data = trump_neutral, aes(x = album_release_date, y = ..count..), stat = "bin", color = "black") + 
      geom_smooth(data = trump_negative, aes(x = album_release_date, y = ..count..), stat = "bin", color = "red") + 
      xlab("Year") + ylab("Number of Trump mentions")
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

