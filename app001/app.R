library(shiny)
library(tidyverse)
library(shinythemes)


lei_names <- read_csv("data/lei_name.csv")
washington <-read_csv("data/state_WA.csv")
washington <- washington %>% filter(loan_purpose == "1", occupancy_type == "1")  
washington <- left_join(washington, lei_names, by = "lei") 
head(washington, 5)



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
                        tabPanel("Age", plotOutput("age")),
                        tabPanel("Race", plotOutput("race")),
                        tabPanel("Low Income", plotOutput("low_income_combo")),
                        tabPanel("Loans Approved", plotOutput("white_minority_combo_scat"))
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
        lei_single <- lei_selected()
        
        #gender
        gender<-washington%>%
            select(lei,`derived_msa-md`,  derived_sex, action_taken)%>%
            mutate(approved=ifelse(action_taken == 1, 1, 0)) %>% group_by(lei, `derived_msa-md`, derived_sex) %>%
            summarize(approved=sum(approved), denied=n()-sum(approved))
        gender
        
        gender<- gender %>% 
            replace(is.na(.),0)
        
        #2. Compute % of Loans Denied and Approved by msa_county_code and lei
        wa_total_gender<- gender %>% group_by(lei, `derived_msa-md`) %>% 
            summarize(total_applic=sum(approved)+sum(denied))
        
        wa_result_gender<-merge(wa_total_gender, gender, by=c("lei","derived_msa-md")) %>%
            mutate(pct_approval=100*approved/total_applic, pct_denied=100*denied/total_applic)
        
        wa_result_gender
        
        #Select Single LEi
        wa_result_single_gender<-wa_result_gender %>%
            select(lei, `derived_msa-md`, derived_sex, pct_denied) %>%
            pivot_wider(names_from = derived_sex, values_from = pct_denied, values_fill = list(value=0))%>%
            replace(is.na(.), 0)%>%
            pivot_longer(col= c("Female", "Joint", "Male", "Sex Not Available"), names_to= 'gender', values_to = "values")%>%
            filter(`derived_msa-md`==42644 & lei == lei_single)
        wa_result_single_gender
        
        #Select Multi Lei
        wa_result_multi_gender<-wa_result_gender %>%
            select(lei, `derived_msa-md`, derived_sex, pct_denied) %>%
            pivot_wider(names_from = derived_sex, values_from = pct_denied, values_fill = list(value=0))%>%
            replace(is.na(.), 0)%>%
            pivot_longer(col= c("Female", "Joint", "Male", "Sex Not Available"), names_to= 'gender', values_to = "values")%>%
            filter(`derived_msa-md`==42644 & lei %in% c("0S8H5NJFLHEVJXVTQ413", "549300KM40FP4MSQU941", "7H6GLXDRUGQFU57RNE97",  
                                                        "549300AG64NHILB7ZP05", "549300FGXN1K3HLB1R50",  "KB1H1DSPRFMYMCUFXT09"))
        wa_result_multi_gender
        
        
        total_gender<- rbind(wa_result_single_gender, wa_result_multi_gender) #%>% 
        
        
        total_gender%>%
            ggplot(aes(x=lei, y=values, text=lei), fill=gender)+ geom_bar(position="stack", stat="identity",aes(fill=gender))+
            coord_flip() + 
            labs(title="Loan Denial Rates by Gender ") +
            theme(plot.title = element_text(size = 20, color = "orange")) +
            labs(y=" % pct")
        
    })
    
    #age
    output$age <- renderPlot({ 
        lei_single <- lei_selected()
        
        age<-washington%>%
            select(lei,`derived_msa-md`,  applicant_age, action_taken)%>%
            mutate(approved=ifelse(action_taken == 1, 1, 0)) %>% group_by(lei, `derived_msa-md`, applicant_age) %>%
            summarize(approved=sum(approved), denied=n()-sum(approved))
        age
        
        age<- age %>% 
            replace(is.na(.),0)
        
        #2. Compute % of Loans Denied and Approved by msa_county_code and lei
        wa_total_age<- age %>% group_by(lei, `derived_msa-md`) %>% 
            summarize(total_applic=sum(approved)+sum(denied))
        
        wa_result_age<-merge(wa_total_age, age, by=c("lei","derived_msa-md")) %>%
            mutate(pct_approval=100*approved/total_applic, pct_denied=100*denied/total_applic)
        
        wa_result_age
        
        #Select Single LEi
        wa_result_single_age<-wa_result_age %>%
            select(lei, `derived_msa-md`, applicant_age, pct_denied) %>%
            pivot_wider(names_from = applicant_age, values_from = pct_denied, values_fill = list(value=0))%>%
            replace(is.na(.), 0)%>%
            pivot_longer(col= c("<25", "25-34", "35-44", "45-54", "55-64", "65-74", ">74"), names_to= 'age', values_to = "values")%>%
            filter(`derived_msa-md`==42644 & lei == lei_single)
        wa_result_single_age
        
        #Select Multi Lei
        wa_result_multi_age<-wa_result_age %>%
            select(lei, `derived_msa-md`, applicant_age, pct_denied) %>%
            pivot_wider(names_from = applicant_age, values_from = pct_denied, values_fill = list(value=0))%>%
            replace(is.na(.), 0)%>%
            pivot_longer(col= c("<25", "25-34", "35-44", "45-54", "55-64", "65-74", ">74"), names_to= 'age', values_to = "values")%>%
            filter(`derived_msa-md`==42644 & lei %in% c("0S8H5NJFLHEVJXVTQ413", "549300KM40FP4MSQU941", "7H6GLXDRUGQFU57RNE97",  
                                                        "549300AG64NHILB7ZP05", "549300FGXN1K3HLB1R50",  "KB1H1DSPRFMYMCUFXT09"))
        wa_result_multi_age
        
        
        total_age<- rbind(wa_result_single_age, wa_result_multi_age) #%>% 
        
        
        total_age%>%
            ggplot(aes(x=lei, y=values, text=lei), fill=age)+ geom_bar(position="stack", stat="identity",aes(fill=age))+
            coord_flip() + 
            labs(title="Loan Denial Rates by Age ") +
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
        
        
        total%>%
            ggplot(aes(x=lei, y=values), fill=race)+ geom_bar(position="stack", stat="identity",aes(fill=race)) +
            coord_flip() + labs(title="Loan Denial Rates by Race ") + 
            theme(plot.title = element_text(size = 20, color = "orange")) + 
            labs(y=" % pct")
        
    })
    
    #LOW Income Borrowers
    output$low_income_combo <- renderPlot({ 
        lei_single <- lei_selected()
        
        lim_approved<-washington%>%
            select(lei, county_code, `derived_msa-md`, census_tract, derived_race, action_taken, tract_population, tract_minority_population_percent, income, ffiec_msa_md_median_family_income)%>%
            filter(`derived_msa-md`== 42644, income<0.5*ffiec_msa_md_median_family_income) %>% 
            mutate(approved=ifelse(action_taken == 1, 1, 0)) %>%
            group_by(lei) %>% 
            summarize(approved=sum(approved), denied=n()-sum(approved))%>%
            mutate(pct_approval=100*approved/(approved+denied), pct_denied=100*denied/(approved+denied))
        
        
        low_income1<-lim_approved%>%
            filter(lei == lei_single)%>%
            select(lei, pct_approval)
        
        low_income_multi<-lim_approved%>%
            filter(lei %in% c("0K2D5AK28E3O5CC06E35", "213800QUAI2VH5YM6310", "25490018IFQOT83Q7H49"))%>%
            select(lei, pct_approval)
        
        avg_approval<-lim_approved%>% summarize(approved=sum(approved), denied=sum(denied))%>%
            mutate(pct_approval=100*approved/(approved+denied), pct_denied=100*denied/(approved+denied))%>%
            select(pct_approval)%>%.[[1,1]]
        
        low_income1<-low_income1%>%mutate(marker1= 1)
        low_income_multi<-low_income_multi%>%mutate(marker1= 2)
        low_income_combo<-rbind(low_income1, low_income_multi)
        
        
        low_income_combo %>% ggplot(aes(x=lei, y=pct_approval, fill = marker1)) + geom_bar(stat="identity") +  coord_flip() +
            labs(title="Loans Approved to Low Income Borrowers '%") + theme(plot.title = element_text(size = 14, face= "bold")) + 
            labs(y = "Low Income Borrowers %") + theme(legend.position = "none") +
            geom_hline(yintercept= avg_approval)
        
    })
    
    # White_Minority Borrowers
    output$white_minority_combo_scat <- renderPlot({ 
        lei_single <- lei_selected()
        
        #1. To get to Minority Borrowers Calculaions
        wa_race<-washington%>%
            select(lei, county_code, `derived_msa-md`, census_tract, derived_race, action_taken, tract_population,  tract_minority_population_percent)%>%
            mutate(approved=ifelse(action_taken == 1, 1, 0)) %>% group_by(lei, `derived_msa-md`, derived_race) %>%
            summarize(approved=sum(approved), denied=n()-sum(approved))
        head(wa_race, 5)
        
        #2. Compute % of Loans Denied and Approved by msa_code and lei
        wa_race %>% group_by(lei, `derived_msa-md`) %>% summarize(total_applic=sum(approved)+sum(denied))
        wa_result<-merge(wa_total, wa_race, by=c("lei","derived_msa-md")) %>% 
            mutate(pct_approval=100*approved/total_applic, pct_denied=100*denied/total_applic)
        head(wa_result, 5)
        
        #3. Regroup Race by White and Minority by adding all non-White together
        wa_minority<-wa_result %>% 
            mutate(bi_race=ifelse(derived_race=='White', 'White', 'Minority')) %>% 
            group_by(lei, `derived_msa-md` ,bi_race) %>% summarise(approved=sum(approved), denied=sum(denied))
        head(wa_minority, 5)
        
        #4. Compute pct_approved, denied for White and Minority Borrowers
        wa_county<-wa_minority %>% group_by(lei, `derived_msa-md` ) %>% summarise(total_applic=sum(approved) + sum(denied))
        white_minority<-merge(wa_minority, wa_county, by=c("lei","derived_msa-md")) %>% 
            mutate(pct_approval=100*approved/total_applic, pct_denied=100*denied/total_applic)
        
        head(white_minority, 5)
        
        #Select Single LEI
        white_minority_one<-white_minority%>% 
            filter(`derived_msa-md`==42644 & lei==lei_single) %>% 
            select(lei, bi_race, pct_approval) %>%
            pivot_wider(names_from = bi_race, values_from = pct_approval) %>% 
            replace(is.na(.), 0) 
        white_minority_one
        
        #Select Multiple Lei
        white_minority_multi<-white_minority%>% 
            filter(`derived_msa-md`==42644 & lei %in% c("549300KM40FP4MSQU941", "7H6GLXDRUGQFU57RNE97",   "549300AG64NHILB7ZP05", "549300FGXN1K3HLB1R50",  "KB1H1DSPRFMYMCUFXT09")) %>% 
            select(lei, bi_race, pct_approval) %>%
            pivot_wider(names_from = bi_race, values_from = pct_approval) %>% 
            replace(is.na(.), 0) 
        white_minority_multi
        
        #Add Markers
        white_minority_one<- white_minority_one %>% mutate(marker1=1)
        white_minority_multi<-white_minority_multi %>% mutate(marker1=2)
        
        #Combine Multi and Single
        white_minority_combo<-rbind(white_minority_one, white_minority_multi)
        white_minority_combo
        
        # Scatterplot for combined
        white_minority_combo%>%
            ggplot(aes(x=Minority, y=White, text=lei)) + 
            geom_point(alpha=0.8, size=3, colour=white_minority_combo$marker1) + 
            labs(title="Loans Approved to White and Minority Borrowers '%") + 
            theme(plot.title = element_text(size = 14, face= "bold")) + 
            labs(face= "bold") + theme(legend.position = "none")+
            labs(x="Minority Borrowers", y="White Borrowers", size=11, face="bold")
        #ggplotly(white_minority_combo_scat, tooltip = "text")
        
        
    })
    
}    