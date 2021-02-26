#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shiny)
library(shinythemes)
library(tidyr)
library(dplyr)
library(quanteda)
library(qdap)
library(ggplot2)
library(stringr)
library(tidytext)
library(data.table)
library(readr)
library(xtable)
library(DT)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    bigram_lookup <- readRDS("bigram_lookup.rds")
    print("done")
    trigram_lookup <- readRDS("trigram_lookup.rds")
    print("done")
    fourgram_lookup <- readRDS("fourgram_lookup.rds")
    print("done")
    fivegram_lookup <- readRDS("fivegram_lookup.rds")
    print("done")
    #Reading the data generated
    
    source("./Predict.R", local = T)
    
    output$processed <- renderText({
        req(input$text)
        input$text
    })
    #Picking the highest MLE word
    output$prediction<- renderText(({
        req(input$text)
            predicted <- predict(input$text)[[1]]
            print(predicted)
    }))
    #Showing other words found in table
    output$alt<- renderTable({
        req(input$text)
            print(result)
        
    }, hover= TRUE, width = 600, digits=9)
})
