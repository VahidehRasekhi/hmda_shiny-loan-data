#
#
#

library(shiny)
library(sortable)
library(tidyverse)
library(ggplot2)
library(httr)

wash_init <- read.csv("data/wash_init.csv")
lei_names <- read.csv("data/lei_name.csv")
lars_names <- read.csv("data/lars_lookup.csv")

ui <- fluidPage(mainPanel(tabsetPanel(
    tabPanel(
        "Select Institution ",
        fluidRow(
            column(6,
                selectInput("select", label = h3("Select box"), 
                            choices = lei_names$name, 
                            selected = 1),            
                selectInput(
                    'in3',
                    'Select Institution',
                    lei_names$name,
                    multiple = TRUE,
                    selectize = FALSE,
                    size = 40
                ),
            ),
            column(6,
                hr(),
                verbatimTextOutput('out3'),
                selectInput(
                    'in3',
                    'Select Institution',
                    c(),
                    multiple = TRUE,
                    selectize = FALSE,
                    size = 40
                )
            )
                    
        )

    ),
    tabPanel("Compare", verbatimTextOutput("summary"))
))
)

server <- function(input, output) {
    output$out3 <- renderPrint(input$in3)
}

shinyApp(ui = ui, server = server)
