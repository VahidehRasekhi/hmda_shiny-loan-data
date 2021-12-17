#
#
#

library(shiny)
library(sortable)

ui <- fluidPage(mainPanel(tabsetPanel(
    tabPanel(
        "Select Institution ",
        fluidRow(
            column(6,
                verbatimTextOutput('out3'),
                selectInput(
                    'in3',
                    'Select Institution',
                    state.name,
                    multiple = TRUE,
                    selectize = FALSE,
                    size = 40
                ),
            ),
            column(6,
                verbatimTextOutput('out3'),
                selectInput(
                    'in3',
                    'Select Institution',
                    state.name,
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
