#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#   
#    http://shiny.rstudio.com/
#
require(shiny)
library(RecordLinkage)
library(tm)
library(dplyr)
library(plyr)
## Prepare data to be displayed
library(wordcloud)
load("trainModel.RData")
source('predFonction.R')


shinyServer(function(input, output,session) {
    myTexts <- ""
    nextWord <- reactive({
        predNextWord(input$nGram2, trainModelBase[[1]], trainModelBase[[2]], trainModelBase[[3]], trainModelBase[[4]])
    })
    testosse <- reactive({
        input$sendTexts
    })
    observeEvent(input$do1, {
        tmp <- nextWord()[1]
        updateTextAreaInput(session, "nGram2", value = paste(input$nGram2, tmp))
    })
    observeEvent(input$do2, {
        tmp <- nextWord()[2]
        updateTextAreaInput(session, "nGram2", value = paste(input$nGram2, tmp))
    })
    observeEvent(input$do3, {
        tmp <- nextWord()[3]
        updateTextAreaInput(session, "nGram2", value = paste(input$nGram2, tmp))
    })
    observeEvent(input$learn, {
        trainModelBase <<- learnNewNgram(input$nGram2,trainModelBase)
        myTexts <<-paste(input$nGram2,myTexts, sep = "\n")
        updateTextAreaInput(session, "sendTexts", value = myTexts)
        updateTextAreaInput(session, "nGram2", value = "")
        updateTextInput(session, "sendTexts2", value = myTexts)
    })
    output$choice1 <- renderText({
        nextWord()[1]
    })
    output$choice2 <- renderText({
        nextWord()[2]
    })
    output$choice3 <- renderText({
        nextWord()[3]
    })
    output$testooseterosse <- renderText({
        testosse()
    })
    output$wordCloud4 <- renderPlot({
        wordcloud(paste(trainModelBase[[1]]$first3,trainModelBase[[1]]$lastWord),trainModelBase[[1]]$Freq ,
                      max.words=50,
                      colors=brewer.pal(8, "Dark2"))
    })
    output$wordCloud3 <- renderPlot({
        wordcloud(paste(trainModelBase[[2]]$first2,trainModelBase[[2]]$lastWord),trainModelBase[[2]]$Freq ,
                  max.words=100,
                  colors=brewer.pal(8, "Dark2"))
    })
    output$wordCloud2 <- renderPlot({
        wordcloud(paste(trainModelBase[[3]]$first,trainModelBase[[3]]$lastWord),trainModelBase[[3]]$Freq,
                  max.words=200,
                  colors=brewer.pal(8, "Dark2"))
    })

})
    