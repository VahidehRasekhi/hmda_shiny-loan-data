library(shiny)

wash_init <- read.csv("data/wash_init.csv")
lei_names_msa <- read.csv("data/lei_names_msa.csv")
msa_codes <- c("WA", 0, 13380, 14740, 28420, 30300, 31020, 34580, 36500,
               38900, 42644, 44060, 45104, 47460, 48300, 49420, 99999)
names <- lei_names_msa$name %>% unique() %>% sort()
lars_names <- read.csv("data/lars_lookup.csv")

ui <- fluidPage(mainPanel(tabsetPanel(
    tabPanel(
        "Select Institution ",
        selectInput("select_msa", label = h3("Regions"),
                    choices = msa_codes,
                    selected = 1),
        
        fluidRow(
            column(6,
                   selectInput(inputId = "in_instit",
                    label = h4("Select Institution:"),
                    multiple = TRUE,
                    selectize = FALSE,
                    size = 40,
                    choices =  names)
        
            ),
            column(6,
                   selectInput(inputId = "out_instit",
                               label = h4("Institutions to compare to:"),
                               multiple = TRUE,
                               selectize = FALSE,
                               size = 40,
                               choices =  c())
            )
        )
    ),
    tabPanel(
        "Compare", 
        fluidRow(
            column(6, 
                   selectInput("select", label = h3("Select box"), 
                               choices = lei_names$name, 
                               selected = 1),            
                   selectInput("derived_ethnicity", label = h3("Select box"), 
                            choices = c("All", lars_names %>% 
                                            filter(lars_names == "derived_ethnicity") %>% pull(value)),
                            selected = 1),
            ),
            column(6, 
                DT::dataTableOutput("table")
            )
        )
    ),
    tabPanel(
        "Test", 
        fluidRow(
            column(10, 
                   DT::dataTableOutput("test")
            )
        )
    )
)))

server <- function(input, output, session) {

    ########### Code for first panel
    # updates first selectInput on change of pulldown
    observe({
        if (input$select_msa == "WA") {
            names <- lei_names_msa$name %>% unique() %>% sort()
        } else {
            names <- lei_names_msa %>%
                filter(derived_msa.md == input$select_msa) %>%
                pull(name) %>% sort()
        }
        updateSelectInput(session, "in_instit",
                          choices = names
        )
    })
    
    # updates the second selectInput 
    observe({
        updateSelectInput(session, "out_instit",
                          choices = input$in_instit
        )
        
    })
 
    ########### Code for second panel
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
    
    output$table <- DT::renderDataTable(DT::datatable({
        data <- wash_init %>% count(action_taken)
        if (input$derived_ethnicity != "All") {
            data <- demographic()
        }
        data
    }))
    
    ########### Code for third panel
    selected_lei <- reactive({
        lei_codes <- lei_names_msa %>% filter(name %in% input$in_instit)
        wash_init %>% filter(lei %in% lei_codes$lei)
        
    })
    
    output$test <- DT::renderDataTable(DT::datatable({
        selected_lei()
    }))
    
    
       
}

shinyApp(ui = ui, server = server)
