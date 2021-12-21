library(shiny)
library(tidyverse)
library(shinythemes)
library (plotly)


lei_names <- read.csv("../data/lei_name.csv")
washington <-read_csv('../data/state_WA.csv')
washington <- washington %>% filter(loan_purpose == "1", occupancy_type == "1")  
washington <- left_join(washington, lei_names, by = "lei") 
lars_names<- read_csv('../data/lars_lookup.csv')


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
    
    output$gender <- renderPlot({
        lei <- lei_selected()
        
        #gender
        gender<-washington%>%
            select(lei,`derived_msa-md`,  derived_sex, action_taken)%>%
            mutate(approved=ifelse(action_taken == 1, 1, 0)) %>% group_by(lei, `derived_msa-md`, derived_sex) %>%
            summarize(approved=sum(approved), denied=n()-sum(approved))
        gender
        
        gender<- gender %>% 
            replace(is.na(.),0)
        
        #2. Compute % of Loans Denied and Approved by msa_county_code and lei
        wa_total_gender<- gender %>% group_by(lei, `derived_msa-md`) %>% summarize(total_applic=sum(approved)+sum(denied))
        wa_result_gender<-merge(wa_total_gender, gender, by=c("lei","derived_msa-md")) %>%
            mutate(pct_approval=100*approved/total_applic, pct_denied=100*denied/total_applic)
        
        wa_result_gender
        
        #Select Single LEi
        wa_result_single_gender<-wa_result_gender %>%
            select(lei, `derived_msa-md`, derived_sex, pct_denied) %>%
            pivot_wider(names_from = derived_sex, values_from = pct_denied, values_fill = list(value=0))%>%
            replace(is.na(.), 0)%>%
            #rowwise%>%
            #mutate(Other=sum(c_across(c('Joint','Free Form Text Only', '2 or more minority races', 'Race Not Available')))) %>%
            #select(lei, `derived_msa-md` ) %>%
            pivot_longer(col= c("Female", "Joint", "Male", "Sex Not Available"), names_to= 'gender', values_to = "values")%>%
            filter(lei == lei)
        #filter(`derived_msa-md`==42644 & lei == lei)
        wa_result_single_gender
        
        #Select Multi Lei
        wa_result_multi_gender<-wa_result_multi_gender %>%
            select(lei, `derived_msa-md`, derived_sex, pct_denied) %>%
            pivot_wider(names_from = derived_sex, values_from = pct_denied, values_fill = list(value=0))%>%
            replace(is.na(.), 0)%>%
            #rowwise%>%
            #select(lei, `derived_msa-md` ) %>%
            pivot_longer(col= c("Female", "Joint", "Male", "Sex Not Available"), names_to= 'gender', values_to = "values")%>%
            filter(`derived_msa-md`==42644 & lei %in% c("0S8H5NJFLHEVJXVTQ413", "549300KM40FP4MSQU941"))
        wa_result_multi_gender
        
        total%>%ggplot(aes(x=lei, y=values, text=lei), fill=race)+ geom_bar(position="stack", stat="identity",aes(fill=race))+
            coord_flip() + labs(title="Loan Denial Rates by Race ") + 
            theme(plot.title = element_text(size = 20, color = "orange")) + 
            labs(y=" % pct")
        #ggplotly(race_chart, tooltip = "text")
        
        
        
        rbind(wa_result_single_gender, wa_result_multi_gender) %>% 
            mutate(lei=replace(lei, lei==unique(total_gender$lei)[1], "single")) %>%
            mutate(lei=replace(lei, lei==unique(total_gender$lei)[2], "multiple")) %>%

            ggplot(aes(x=lei, y=values, text=lei), fill=gender)+ geom_bar(position="stack", stat="identity",
                                                                          aes(fill=gender))+
            coord_flip() + 
            labs(title="Loan Denial Rates by Gender ") +
            theme(plot.title = element_text(size = 20, color = "orange")) +
            labs(y=" Percentage")
        
        total_gender<-rbind(wa_result_single_gender, wa_result_multi_gender)
        total_gender
        
        total_gender%>%
            ggplot(aes(x=lei, y=values, text=lei), fill=race)+ geom_bar(position="stack", stat="identity",aes(fill=gender))+
            coord_flip() + 
            labs(title="Loan Denial Rates by Gender ") +
            theme(plot.title = element_text(size = 20, color = "orange")) +
            labs(y=" % pct")
        
    })
    
    output$race <- renderPlot({ 
        lei <- lei_selected()
        
        #race
        
        #msa_codes <- c("WA", 0, 13380, 14740, 28420, 30300, 31020, 34580, 36500,38900, 42644, 44060, 45104, 47460, 48300, 49420, 99999)
        
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
            filter(lei == lei)
        # filter(`derived_msa-md`==42644 & lei == lei)
        wa_result_single
        
        #Select Multi Lei
        wa_result_multi<-wa_result %>% 
            select(lei, `derived_msa-md`, derived_race, pct_denied) %>% 
            pivot_wider(names_from = derived_race, values_from = pct_denied, values_fill = list(value=0))%>%
            replace(is.na(.), 0)%>%rowwise%>%
            mutate(Other=sum(c_across(c('Joint','Free Form Text Only', '2 or more minority races', 'Race Not Available')))) %>% 
            select(lei, `derived_msa-md`, c("White", "Asian", "Black or African American", "American Indian or Alaska Native" , "Native Hawaiian or Other Pacific Islander", "Other") ) %>% 
            pivot_longer(cols = c("White", "Asian", "Black or African American", "American Indian or Alaska Native" , "Native Hawaiian or Other Pacific Islander", "Other"), names_to = "race", values_to = "values")%>% 
            filter(`derived_msa-md`==42644 & lei %in% c("0S8H5NJFLHEVJXVTQ413", "549300KM40FP4MSQU941", "7H6GLXDRUGQFU57RNE97",   "549300AG64NHILB7ZP05", "549300FGXN1K3HLB1R50",  "KB1H1DSPRFMYMCUFXT09"))
        wa_result_multi
        
        total<-rbind(wa_result_single, wa_result_multi)
        
        
        total%>%ggplot(aes(x=lei, y=values), fill=race)+ geom_bar(position="stack", stat="identity",aes(fill=race)) +
            coord_flip() + labs(title="Loan Denial Rates by Race ") + 
            theme(plot.title = element_text(size = 20, color = "orange")) + 
            labs(y=" % pct")
        #ggplotly(total)
        
        
    })
    
}    





shinyApp(ui = ui, server = server)