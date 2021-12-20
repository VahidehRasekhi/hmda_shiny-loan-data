library(shiny)
library(tidyverse)
library(shinythemes)


lei_names <- read.csv("data/lei_name.csv")
washington <-read_csv('data/state_WA.csv')
washington <- washington %>% filter(loan_purpose == "1", occupancy_type == "1")  
washington <- left_join(washington, lei_names, by = "lei") 

ui <- fluidPage(
    theme = shinytheme("superhero"),
    titlePanel("HMDA Comparisons"),
    
    sidebarLayout(
        
        sidebarPanel(
            selectInput("select",
                        label = h3("Choose Institution:"),
                        choices = unique(sort(washington$name)),
                        selected = 1),
            h3("Select Institutions"),
            selectInput("select_msa", label = h4("Regions (MSA/MD code)"),
                        choices = c("WA", unique(sort(washington$`derived_msa-md`))),
                        selected = 1),
            selectInput(inputId = "in_instit",
                        label = h4("Choosen Institutions:"),
                        multiple = TRUE,
                        selectize = FALSE,
                        width = 800,
                        size = 25,
                        choices =  unique(sort(washington$name)))
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Tabset w/ plot, summary, and table ----
            tabsetPanel(type = "tabs",
                        tabPanel("Gender", plotOutput("gender")),
                        tabPanel("Summary", verbatimTextOutput("summary")),
                        tabPanel("Table", tableOutput("table"))
            )
            
        )
    )
    
)

server <- function(input, output) {

}

shinyApp(ui = ui, server = server)
