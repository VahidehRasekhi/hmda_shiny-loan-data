#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(httr)

url <- 'https://ffiec.cfpb.gov/v2/data-browser-api/view/filers?states=WA&years=2020'
response <- GET(url) 
lei <- content(response, type = 'text') %>% 
  jsonlite::fromJSON() %>% 
  .[[1]] %>%                      # Select the first element of the resulting list
  as_tibble()

# data <- read.csv("data/state_WA.csv")
lei_names <- read.csv("data/lei_name.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("HMDA Loan Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput("select", label = h3("Select box"), 
                      choices = lei_names$name, 
                      selected = 1),
          
          
        ),

        # Show a plot of the generated distribution
        mainPanel(
          fluidRow(column(10, verbatimTextOutput("value")))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

      output$value <- renderPrint({ input$select })

}

# Run the application 
shinyApp(ui = ui, server = server)
