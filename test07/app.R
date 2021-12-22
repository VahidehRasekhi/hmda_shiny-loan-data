library(shiny)
library(tidyverse)
library(shinythemes)


lei_names <- read_csv("data/lei_name.csv")
washington <-read_csv("data/state_WA.csv")
washington <- washington %>% filter(loan_purpose == "1", occupancy_type == "1")  
washington <- left_join(washington, lei_names, by = "lei") 
washington <- washington %>% mutate(name_trunc = substr(name, 1,20))
# head(washington, 5)



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
        
        lei_multiple <- multi_lei_selected()

        race<-washington%>%
            # if wash selected else all
            filter(if (input$select_msa == "WA") {TRUE} 
                   else {`derived_msa-md` == input$select_msa})  %>% 
            select(lei,`derived_msa-md`,  derived_race, action_taken) %>%
            mutate(approved=ifelse(action_taken == 1, 1, 0)) %>% 
            group_by(lei, derived_race) %>%
            summarize(approved=sum(approved), denied=n()-sum(approved)) %>% 
            mutate(pct_approval = 100 * approved/(approved + denied), 
                   pct_denied = 100 * denied/(approved + denied))
        
        wa_result_single_race <- race %>%
            filter(lei == lei_single) 
        
        wa_result_multi_race <- race %>% 
            filter(lei %in% lei_multiple$lei) %>% 
            group_by(derived_race) %>%
            summarize(approved = sum(approved), denied = sum(denied))  %>% 
            mutate(pct_approval = 100 * approved/(approved + denied), 
                   pct_denied = 100 * denied/(approved + denied),
                   lei = "Aggrage") 
        
        wa_result_multi_race
        
        total_race<- rbind(wa_result_single_race, wa_result_multi_race) #%>% 
        
        total_race%>%
            ggplot(aes(x=derived_race, y=pct_denied, text="Compare"), fill=lei)+ 
            geom_bar(position = "dodge", stat="identity",aes(fill=lei))+
            coord_flip() + 
            labs(title="Loan Denial Rates by Race ") +
            theme(plot.title = element_text(size = 20, color = "orange")) +
            labs(y=" % pct")
        
    })
    
    
    #LOW Income Borrowers
    output$low_income_combo <- renderPlot({ 
        lei_single <- lei_selected()
        
        lei_multiple <- multi_lei_selected()
        
        
        lim_approved<-washington%>%
            select(lei, county_code, `derived_msa-md`, census_tract, derived_race, action_taken, tract_population, tract_minority_population_percent, income, ffiec_msa_md_median_family_income)%>%
            filter(if (input$select_msa == "WA") {TRUE} 
                   else {`derived_msa-md` == input$select_msa})  %>% 
            filter(income<0.5*ffiec_msa_md_median_family_income) %>%
            mutate(approved=ifelse(action_taken == 1, 1, 0)) %>%
            group_by(lei) %>%
            summarize(approved=sum(approved), denied=n()-sum(approved))%>%
            mutate(pct_approval=100*approved/(approved+denied), pct_denied=100*denied/(approved+denied))
        
        
        low_income1<-lim_approved%>%
            filter(lei == lei_single)%>%
            select(lei, pct_approval)
        
        low_income_multi<-lim_approved%>%
            filter(lei %in% lei_multiple$lei) %>% 
            select(lei, pct_approval)
        
        avg_approval<-lim_approved%>% summarize(approved=sum(approved), denied=sum(denied))%>%
            mutate(pct_approval=100*approved/(approved+denied), pct_denied=100*denied/(approved+denied))%>%
            select(pct_approval)%>%.[[1,1]]
        
        low_income1<-low_income1%>%mutate(marker1= 1)
        low_income_multi<-low_income_multi%>%mutate(marker1= 2)
        low_income_combo<-rbind(low_income1, low_income_multi)
        
        # low_income_combo <- left_join(low_income_combo, washington %>% 
        #                                   select(lei, name_trunc), by = "lei") %>%
        #     select(-lei)
        # 
        low_income_combo %>% ggplot(aes(x=lei, y=pct_approval, fill = marker1)) + geom_bar(stat="identity") +  coord_flip() +
            labs(title="Loans Approved to Low Income Borrowers '%") + theme(plot.title = element_text(size = 14, face= "bold")) +
            labs(y = "Low Income Borrowers %") + theme(legend.position = "none") +
            geom_hline(yintercept= avg_approval)
        #ggplotly(low_income_comparison, tooltip = "text")
        
        
    })
    
    # White_Minority Borrowers
    output$white_minority_combo_scat <- renderPlot({ 
        lei_single <- lei_selected()
        
        lei_multiple <- multi_lei_selected()
        
        bi_race<-washington%>%
            # if wash selected else all
            filter(if (input$select_msa == "WA") {TRUE} 
                   else {`derived_msa-md` == input$select_msa})  %>% 
            select(lei,`derived_msa-md`,  derived_race, action_taken) %>%
            mutate(approved=ifelse(action_taken == 1, 1, 0), bi_race=ifelse(derived_race=='White', 'White', 'Minority')) %>% 
            group_by(lei, bi_race) %>%
            summarize(approved=sum(approved), denied=n()-sum(approved)) %>% 
            mutate(pct_approval = 100 * approved/(approved + denied), 
                   pct_denied = 100 * denied/(approved + denied))
        
        ## Change
        bi_race_single<-bi_race %>%
            filter(lei == lei_single) 
        
        bi_race_multi<-bi_race %>% 
            filter(lei %in% lei_multiple$lei) %>% 
            group_by(bi_race) %>%
            summarize(approved = sum(approved), denied = sum(denied))  %>% 
            mutate(pct_approval = 100 * approved/(approved + denied), 
                   pct_denied = 100 * denied/(approved + denied),
                   lei = "Aggragate") 
        
        
        total_bi_race<- rbind(bi_race_single, bi_race_multi) #%>% 
        total_bi_race
        
        
        total_bi_race%>%
            ggplot(aes(x= bi_race, y=pct_approval, text="Compare"), fill=lei)+ 
            geom_bar(position = "dodge", stat="identity",aes(fill=lei))+
            labs(title="Loan Approval Rates to White and Minority Borrowers") +
            theme(plot.title = element_text(size = 20, color = "orange")) +
            labs(y=" % pct", x= NULL)
    })
    
}    

shinyApp(ui = ui, server = server)
