#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(tm)
library(shiny)
library(stringr)
library(shiny)


quadgram <- readRDS("quadgram.RData");
trigram <- readRDS("trigram.RData");
bigram <- readRDS("bigram.RData");
mesg <<- ""



# Define server logic required to draw a histogram
    Predict <- function(x) {
      xclean <- removeNumbers(removePunctuation(tolower(x)))
      xs <- strsplit(xclean, " ")[[1]]
      
      if (length(xs)>= 3) {
        xs <- tail(xs,3)
        if (identical(character(0),head(quadgram[quadgram$unigram == xs[1] & quadgram$bigram == xs[2] & quadgram$trigram == xs[3], 4],1))){
          Predict(paste(xs[2],xs[3],sep=" "))
        }
        else {mesg <<- "Next word is predicted using 4-gram."; head(quadgram[quadgram$unigram == xs[1] & quadgram$bigram == xs[2] & quadgram$trigram == xs[3], 4],1)}
      }
      else if (length(xs) == 2){
        xs <- tail(xs,2)
        if (identical(character(0),head(trigram[trigram$unigram == xs[1] & trigram$bigram == xs[2], 3],1))) {
          Predict(xs[2])
        }
        else {mesg<<- "Next word is predicted using 3-gram."; head(trigram[trigram$unigram == xs[1] & trigram$bigram == xs[2], 3],1)}
      }
      else if (length(xs) == 1){
        xs <- tail(xs,1)
        if (identical(character(0),head(bigram[bigram$unigram == xs[1], 2],1))) {mesg<<-"No match found. Most common word 'the' is returned."; head("the",1)}
        else {mesg <<- "Next word is predicted using 2-gram."; head(bigram[bigram$unigram == xs[1],2],1)}
      }
    }
    
    shinyServer(function(input, output) {
      output$prediction <- renderPrint({
        result <- Predict(input$inputString)
        output$text2 <- renderText({mesg})
        result
      });
    
  output$text1 <- renderText({
    input$inputString});
  
}
)
