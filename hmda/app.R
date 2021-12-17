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
library(ggplot2)
library(httr)

wash_init <- read.csv("data/wash_init.csv")
lei_names <- read.csv("data/lei_name.csv")
lars_names <- read.csv("data/lars_lookup.csv")

ui <- fluidPage(
    
    titlePanel("HMDA Loan Data"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput("select", label = h3("Select box"), 
                        choices = lei_names$name, 
                        selected = 1),            
            selectInput("derived_ethnicity", label = h3("Select box"), 
                        choices = c("All", lars_names %>% 
                                        filter(lars_names == "derived_ethnicity") %>% pull(value)),
                        selected = 1),

            # selectInput("man",
            #             "Manufacturer:",
            #             c("All",
            #               unique(as.character(mpg$manufacturer)))),
            # 
            # selectInput("trans",
            #             "Transmission:",
            #             c("All",
            #               unique(as.character(mpg$trans)))),
            # 
            # selectInput("cyl",
            #             "Cylinders:",
            #             c("All",
            #               unique(as.character(mpg$cyl))))
        ),

        mainPanel(
            fluidRow(column(10, verbatimTextOutput("value1"))),
            fluidRow(column(10, verbatimTextOutput("value2"))),
            fluidRow(column(10, DT::dataTableOutput("table")))
            
        )
    )
)

server <- function(input, output) {
    
    one_lei <- reactive({
        lei_number <- lei_names %>% filter(name == input$select) %>% pull(lei)
        wash_init %>% filter(lei == lei_number)
    })
    
    demographic  <- reactive({
        wi_eth_tib <- wash_init %>% filter(derived_ethnicity == input$derived_ethnicity) %>%
            count(action_taken)
        lei_eth_tib <- one_lei() %>% filter(derived_ethnicity == input$derived_ethnicity) %>%
            count(action_taken)
        
        left_join(lei_eth_tib, wi_eth_tib, by = "action_taken")
    })

    output$value1 <- renderPrint({ input$select })
    
    output$value2 <- renderPrint({ lars_names %>%
            filter(lars_names == "derived_ethnicity", value == input$derived_ethnicity) %>% pull(key) })

    output$table <- DT::renderDataTable(DT::datatable({
        data <- wash_init %>% count(action_taken)
        if (input$derived_ethnicity != "All") {
            data <- demographic()
        }
        data
    }))
    
}

shinyApp(ui = ui, server = server, options = list("width" = 1600))