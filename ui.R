require(shiny)
library(plotly)
require(leaflet)
print("ttt")

# add word cloud of n Grams


shinyUI( pageWithSidebar(
    headerPanel("Predict Next Word"),
    sidebarPanel(
        h4("The goal of the application is to predict the next word given an input text string.")
        ,h4("As you start typing the words in input text box, we will provide 3 possible next words that were predicted by the algorithm.")
        ,h4("You can click on the suggseted words to add it to the text Input or type a different word.")
         ,h4("Once you click on send message. The message will appear on a grey text Area below, and will be added to the prediction model
             , so as to customize the prediction")
        ,h2("-----------------")
        ,h4("The wordcloud panel displays the most frequent nGrams used in the model")
    ),
    mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel( "Next word prediction"
                        ,h2( textAreaInput("nGram2", "Start typing !", value = "", width = '600px', height = '100px'))
                       , h2(actionButton("do1", textOutput("choice1"))
                           , actionButton("do2", textOutput("choice2"))
                           , actionButton("do3", textOutput("choice3")))
                       ,h2(" ")
                       , h2(actionButton("learn", "send message"))
                       ,h2(" --------------------- ")
                       , h4("Sent messages")
                       , h2(verbatimTextOutput("testooseterosse"))
                    )
                    ,tabPanel("Wordcloud model"
                        , h2("Here is some insights in the nGram model that is used for the prediction")
                        ,h4("WordCloud for 4grams")
                        , plotOutput("wordCloud4")
                        ,h4("WordCloud for 3grams")
                        , plotOutput("wordCloud3")
                        ,h4("WordCloud for 2grams")
                        , plotOutput("wordCloud2")
                        , (textAreaInput("sendTexts",label = "", value = "", width = '0px', height = '0px'))
                        
                    )
                    
                    )
        
    )                
)
)


