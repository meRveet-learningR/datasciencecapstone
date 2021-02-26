#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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


# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Mock Swiftkey Prediction Project"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            textInput(inputId = "text", label= "Type something here and press ENTER.")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tags$h2("Predicted next word:"),
            tags$br(),
            tags$h3(textOutput("prediction")),
            tags$h2("Alternatives"),
            tags$p(tableOutput("alt"))
        )
    )
))
