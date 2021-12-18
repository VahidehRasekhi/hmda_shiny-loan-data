

library(shiny)

wash_init <- read.csv("data/wash_init.csv")
lei_names_msa <- read.csv("data/lei_names_msa.csv")
msa_codes <- c("WA", 0, 13380, 14740, 28420, 30300, 31020, 34580, 36500,
               38900, 42644, 44060, 45104, 47460, 48300, 49420, 99999)
names <- c()

ui <- fluidPage(
    fluidRow(
        column(6,
            selectInput("select_msa", label = h3("Regions"),
                        choices = msa_codes,
                        selected = 1),
            uiOutput("in_ui")
        ),
        column(6,
            uiOutput("out_ui")
        )
    )
  
)

server <- function(input, output) {

    output$in_ui <- renderUI({
        # names <- c()
        if (input$select_msa == "WA") {
            names <- lei_names_msa$name %>% unique() %>% sort()
        } else {
            names <- lei_names_msa %>%
                filter(derived_msa.md == input$select_msa) %>%
                pull(name) %>% sort()
        }
        selectInput(inputId = "in_instit",
                    label = h4("Select Institution:"),
                    # lei_names$name,
                    multiple = TRUE,
                    selectize = FALSE,
                    size = 40,
                    choices =  names)
    })
    
    out_instit <- reactive({

        which(names %in% input$in_instit)

    })

    output$out_ui <- renderUI({

        selectInput(inputId = "out_instit",
                    label = h4("Select Institution:"),
                    # out_instit,
                    multiple = TRUE,
                    selectize = FALSE,
                    size = 40,
                    choices =  names)
    })
    

}

shinyApp(ui = ui, server = server)
