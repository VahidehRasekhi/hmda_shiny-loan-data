#
# HMDA Shiny Load Data Application
#
# Pull down for selecting institution, typing in the input field will 
#   filter the items in the selection
#
#
#
#
#
#

library(shiny)
library(tidyverse)
library(httr)

# data <- read.csv("data/state_WA.csv")
lei_names <- read.csv("data/lei_name.csv")
lars_names <- read.csv("data/lars_lookup.csv")

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
            selectInput("action_taken", label = h3("Select box"), 
                        choices = c("All", lars_names %>% 
                                        filter(lars_names == "action_taken") %>% pull(value)),
                        selected = 1)
            
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            fluidRow(column(10, verbatimTextOutput("value1"))),
            fluidRow(column(10, verbatimTextOutput("value2")))
            # ggplot("plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$value1 <- renderPrint({ input$select })
    
    output$value2 <- renderPrint({ lars_names %>% 
            filter(lars_names == "action_taken", value == input$action_taken) %>% pull(key) })
    
    
    
    # renderPlot(date %>% select(input$select))
    
}

# Run the application 
shinyApp(ui = ui, server = server)