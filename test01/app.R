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
msa_codes <- c("WA", 0, 13380, 14740, 28420, 30300, 31020, 34580, 36500, 
               38900, 42644, 44060, 45104, 47460, 48300, 49420, 99999)

ui <- fluidPage(mainPanel(tabsetPanel(
    tabPanel(
        "Select Institution ",
        fluidRow(
            column(6,
                selectInput("select_msa", label = h3("Regions"), 
                            choices = msa_codes, 
                            selected = 1),            
                selectInput(
                    'in_instit',
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
    wash_filtered <- reactive({
        wash_init %>% filter(derived_msa.md == input$select_msa)
    })
    
    # output$in_instit <- renderUI({
    #     selectInput(inputId = "in_instit", label = h4("Select Institution:"), choices =  wash_filtered())
    # })
                          
}

shinyApp(ui = ui, server = server)
