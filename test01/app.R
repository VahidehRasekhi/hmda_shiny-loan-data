library(shiny)
library(tidyverse)
library(shinythemes)
library (plotly)


lei_names <- read.csv("data/lei_name.csv")
washington <-read_csv('data/state_WA.csv')
washington <- washington %>% filter(loan_purpose == "1", occupancy_type == "1")  
washington <- left_join(washington, lei_names, by = "lei") 
washington <- washington %>% mutate(name_trunc = substr(name, 1,20))
lars_names<- read_csv('data/lars_lookup.csv')


ui <- fluidPage(
    theme = shinytheme("superhero"),
    titlePanel("HMDA Comparisons"),
    
    sidebarLayout(
        
        sidebarPanel(
            selectInput("selected_instit",
                        label = h3("Choose Institution:"),
                        choices = unique(sort(washington$name)),
                        selected = 1),
            h3("Select Institution"),
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
                        tabPanel("Race", plotOutput("race")),
                        tabPanel("Age", plotOutput("age")),
                        tabPanel("Low Income", plotOutput("low_income")),
                        tabPanel("Loans Approved", plotOutput("white_minority"))
            )
            
        )
    )
    
)

server <- function(input, output, session) {
    observe({
        if (input$select_msa == "WA") {
            names <- unique(sort(washington$name))
        } else {
            names <- washington %>%
                filter(`derived_msa-md` == input$select_msa) %>%
                pull(name) %>% unique() %>% sort()
        }
        updateSelectInput(session, "in_instit",
                          choices = names
        )
    })
    
    lei_selected <- reactive({
        washington %>% filter(name == input$selected_instit) %>% 
            pull(lei) %>% head(1)
    })
    
    multi_lei_selected <- reactive({
        washington %>% 
        filter(if (is.null(input$in_instit)) {TRUE} 
            else {name == input$in_instit})  %>% 
        select(lei) %>% unique()
    })
    
    output$gender <- renderPlot({
        lei_single <- lei_selected()
        
        lei_multiple <- multi_lei_selected()
        
        #gender
        gender<-washington%>%
            # if wash selected else all
            filter(if (input$select_msa == "WA") {TRUE} 
                   else {`derived_msa-md` == input$select_msa})  %>% 
            select(lei,`derived_msa-md`,  derived_sex, action_taken) %>%
            mutate(approved=ifelse(action_taken == 1, 1, 0)) %>% 
            group_by(lei, derived_sex) %>%
            summarize(approved=sum(approved), denied=n()-sum(approved)) %>% 
            mutate(pct_approval = 100 * approved/(approved + denied), 
                   pct_denied = 100 * denied/(approved + denied))

        wa_result_single_gender <- gender %>%
            filter(lei == lei_single) 
        
        wa_result_multi_gender <- gender %>% 
            filter(lei %in% lei_multiple$lei) %>% 
            group_by(derived_sex) %>%
            summarize(approved = sum(approved), denied = sum(denied))  %>% 
            mutate(pct_approval = 100 * approved/(approved + denied), 
                   pct_denied = 100 * denied/(approved + denied),
                   lei = "Aggrage") 
            
        wa_result_multi_gender
        
        total_gender<- rbind(wa_result_single_gender, wa_result_multi_gender) #%>% 
        
        total_gender%>%
            ggplot(aes(x=derived_sex, y=pct_denied, text="Compare"), fill=lei)+ 
            geom_bar(position = "dodge", stat="identity",aes(fill=lei))+
            coord_flip() + 
            labs(title="Loan Denial Rates by Gender ") +
            theme(plot.title = element_text(size = 20, color = "orange")) +
            labs(y=" % pct")
        
    })
    
    #race
    output$race <- renderPlot({ 
        lei_single <- lei_selected()
        
        
        #To get to computations for Low Income
        wa_race<-washington%>%
            select(lei, county_code, `derived_msa-md`, census_tract, derived_race, action_taken, tract_population, tract_minority_population_percent)%>%
            mutate(approved=ifelse(action_taken == 1, 1, 0)) %>% group_by(lei, `derived_msa-md`, derived_race) %>%
            summarize(approved=sum(approved), denied=n()-sum(approved))
        
        #2. Compute % of Loans Denied and Approved by msa_county_code and lei
        wa_total<-wa_race %>% group_by(lei, `derived_msa-md`) %>% summarize(total_applic=sum(approved)+sum(denied))
        wa_result<-merge(wa_total, wa_race, by=c("lei","derived_msa-md")) %>% 
            mutate(pct_approval=100*approved/total_applic, pct_denied=100*denied/total_applic)
        head(wa_result, 5)
        
        #Select Single LEi
        wa_result_single<-wa_result %>% 
            select(lei, `derived_msa-md`, derived_race, pct_denied) %>% 
            pivot_wider(names_from = derived_race, values_from = pct_denied, values_fill = list(value=0))%>%
            replace(is.na(.), 0)%>%rowwise%>%
            mutate(Other=sum(c_across(c('Joint','Free Form Text Only', '2 or more minority races', 'Race Not Available')))) %>% 
            select(lei, `derived_msa-md`, c("White", "Asian", "Black or African American", "American Indian or Alaska Native" , "Native Hawaiian or Other Pacific Islander", "Other") ) %>% 
            pivot_longer(cols = c("White", "Asian", "Black or African American", "American Indian or Alaska Native" , "Native Hawaiian or Other Pacific Islander", "Other"), names_to = "race", values_to = "values")%>% 
            filter(`derived_msa-md`==42644 & lei == lei_single)
        wa_result_single
        
        #Select Multi Lei
        wa_result_multi<-wa_result %>% 
            select(lei, `derived_msa-md`, derived_race, pct_denied) %>% 
            pivot_wider(names_from = derived_race, values_from = pct_denied, values_fill = list(value=0))%>%
            replace(is.na(.), 0)%>%rowwise%>%
            mutate(Other=sum(c_across(c('Joint','Free Form Text Only', '2 or more minority races', 'Race Not Available')))) %>% 
            select(lei, `derived_msa-md`, c("White", "Asian", "Black or African American", "American Indian or Alaska Native" , "Native Hawaiian or Other Pacific Islander", "Other") ) %>% 
            pivot_longer(cols = c("White", "Asian", "Black or African American", "American Indian or Alaska Native" , "Native Hawaiian or Other Pacific Islander", "Other"), names_to = "race", values_to = "values")%>% 
            filter(`derived_msa-md`==42644 & lei %in% c("0S8H5NJFLHEVJXVTQ413", "549300KM40FP4MSQU941", "7H6GLXDRUGQFU57RNE97",  
                                                        "549300AG64NHILB7ZP05", "549300FGXN1K3HLB1R50",  "KB1H1DSPRFMYMCUFXT09"))
        wa_result_multi
        
        total<-rbind(wa_result_single, wa_result_multi)
        
        total <- left_join(total, washington %>% 
            select(lei, name_trunc), by = "lei") %>%
            select(-lei)
        
        total%>%
            ggplot(aes(x=name_trunc, y=values), fill=race)+ 
            geom_bar(position="stack", stat="identity",aes(fill=race)) +
            coord_flip() + labs(title="Loan Denial Rates by Race ") + 
            theme(plot.title = element_text(size = 20, color = "orange")) + 
            labs(y=" % pct")
        
    })
    
}    





shinyApp(ui = ui, server = server)