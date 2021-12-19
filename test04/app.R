
library(shiny)
library(tidyverse)

##################### Set variables ##################### 
wash_init <- read.csv("data/wash_init.csv")
lei_names_msa <- read.csv("data/lei_names_msa.csv")
msa_codes <- c("WA", 0, 13380, 14740, 28420, 30300, 31020, 34580, 36500,
               38900, 42644, 44060, 45104, 47460, 48300, 49420, 99999)
names <- lei_names_msa$name %>% unique() %>% sort()
lars_names <- read.csv("data/lars_lookup.csv")

choosen_lei <- c()
choosen_set <- c()

##################### UI ##################### 
ui <- fluidPage(
    # App title ----
    titlePanel("HMDA Comparisons"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            selectInput("select", 
                        label = h3("Choose Institution:"), 
                        choices = lei_names_msa$name  %>% sort(), 
                        selected = 1),
            h3("Select Institutions"),
            selectInput("select_msa", label = h4("Regions (MSA/MD code)"),
                        choices = msa_codes,
                        selected = 1),
            selectInput(inputId = "in_instit",
                        label = h4("Choosen Institutions:"),
                        multiple = TRUE,
                        selectize = FALSE,
                        width = 800,
                        size = 25,
                        choices =  names)
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

##################### Server ##################### 
server <- function(input, output, session) {
    selected_lei <- reactive({
        lei_codes <- lei_names_msa %>% filter(name %in% input$in_instit)
        wash_init %>% filter(lei %in% lei_codes$lei)
        
    })
    
    choosen_lei <- reactive({
        lei_number <- lei_names_msa %>% filter(name == input$select) %>% pull(lei)
        wash_init %>% filter(lei == lei_number)
    })

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
    
    
    output$gender <- renderPlot({
        lei_number <- lei_names_msa %>% 
            filter(name == input$select) %>% 
            pull(lei) %>% head(1)
        lei_codes <- lei_names_msa %>% filter(name %in% input$in_instit)
        
        #gender/approval/denial ratio in county code 53033
        gender_approval_rate<-wash_init%>%
            filter(if (input$select_msa == "WA") {TRUE} else {derived_msa.md==input$select_msa}) %>% 
            select(lei,`derived_msa.md`,county_code, census_tract, derived_sex, action_taken, tract_population, tract_minority_population_percent)%>% 
            mutate(approved=ifelse(action_taken == 1, 1, 0)) %>% 
            group_by(lei, county_code, `derived_msa.md`, derived_sex) %>% 
            summarize(approved=sum(approved), denied=n()-sum(approved)) %>%
            mutate(pct_approval=approved/(approved+denied)) %>% mutate(pct_denied=(1-pct_approval))
        
        
        
        #gender/approval/denial ratio in county code 53033 and lei#1IE8VN30JCEQV1H4R804
        gender_approval_rate1<-wash_init%>%
            filter(if (input$select_msa == "WA") {TRUE} else {derived_msa.md==input$select_msa}) %>% 
            filter (lei == lei_number) %>% 
            select(lei,`derived_msa.md`,county_code, census_tract, derived_sex, action_taken, tract_population, tract_minority_population_percent)%>% 
            mutate(approved=ifelse(action_taken == 1 , 1, 0)) %>% 
            group_by(lei, county_code, `derived_msa.md`, derived_sex) %>% 
            summarize(approved=sum(approved), denied=n()-sum(approved)) %>%
            mutate(pct_approval=approved/(approved+denied)) %>% mutate(pct_denied=(1-pct_approval))
        
        
        #gender/approval/denial ratio in county code 53033 and lei#1IE8VN30JCEQV1H4R804/549300XY701IELCE5Q08
        gender_approval_rate2<-wash_init%>%
            filter(if (input$select_msa == "WA") {TRUE} else {derived_msa.md==input$select_msa}) %>% 
            filter (lei %in% lei_codes$lei) %>% 
            select(lei,`derived_msa.md`,county_code, census_tract, derived_sex, action_taken, tract_population, tract_minority_population_percent)%>% 
            mutate(approved=ifelse(action_taken == 1 , 1, 0)) %>% 
            group_by(lei, county_code, `derived_msa.md`, derived_sex) %>% 
            summarize(approved=sum(approved), denied=n()-sum(approved)) %>%
            mutate(pct_approval=approved/(approved+denied)) %>% mutate(pct_denied=(1-pct_approval))
        
        
        #total application per lender in county code 53033
        gender_total_app<- gender_approval_rate %>% 
            filter(if (input$select_msa == "WA") {TRUE} else {derived_msa.md==input$select_msa}) %>% 
            group_by(`derived_msa.md`,county_code, lei) %>% 
            summarize(total_application=sum(approved)+ sum(denied)) 
        
        
        
        #total application per lender in county code 53033 and lei#1IE8VN30JCEQV1H4R804 
        gender_total_app1<- gender_approval_rate %>% 
            filter(if (input$select_msa == "WA") {TRUE} else {derived_msa.md==input$select_msa}) %>% 
            filter(lei==lei_number) %>% 
            group_by(`derived_msa.md`,county_code, lei) %>% 
            summarize(total_application=sum(approved)+ sum(denied)) 
        
        
        
        #total application per lender in county code 53033 and lei#1IE8VN30JCEQV1H4R804/549300XY701IELCE5Q08 
        gender_total_app2<- gender_approval_rate %>% 
            filter(if (input$select_msa == "WA") {TRUE} else {derived_msa.md==input$select_msa}) %>% 
            filter(lei %in% lei_codes$lei) %>% 
            group_by(`derived_msa.md`,county_code, lei) %>% 
            summarize(total_application=sum(approved)+ sum(denied)) 
        
        
        #total application, percentage of approved/denied acpplicaiton based on gender per lender in county code 53033
        gender_total <- merge(gender_total_app, gender_approval_rate) %>% 
            mutate(pct_approval=100*approved/total_application, pct_denied=100*denied/total_application)
        
        
        #total application, percentage of approved/denied acpplicaiton based on gender per lender in county code 53033 and lei#1IE8VN30JCEQV1H4R804
        gender_total1 <- merge(gender_total_app1, gender_approval_rate1) %>% 
            mutate(pct_approval=100*approved/total_application, pct_denied=100*denied/total_application)
        
        
        #total application, percentage of approved/denied acpplicaiton based on gender per lender in county code 53033 and lei#1IE8VN30JCEQV1H4R804/549300XY701IELCE5Q08
        gender_total2 <- merge(gender_total_app2, gender_approval_rate2) %>% 
            mutate(pct_approval=100*approved/total_application, pct_denied=100*denied/total_application)
        
        
        
        #plotting denial/approval rate based on gender in county_code 53033 and lei#1IE8VN30JCEQV1H4R804  
        gender_total1 %>%
            select(derived_sex, pct_denied, pct_approval) %>%
            pivot_longer(c(pct_denied, pct_approval), names_to = "type", values_to = "value") %>%
            ggplot() + 
            aes(x = derived_sex, y = value, fill = type) + 
            geom_col()
        
        #side by side plotting of denial/approval rate based on gender in county_code 53033 and lei#1IE8VN30JCEQV1H4R804/549300XY701IELCE5Q08 
        total<- rbind(gender_total1, gender_total2)
        
        gender_total1 <- gender_total1 %>%
            select(lei, county_code, derived_sex, pct_denied, pct_approval) %>%
            pivot_longer(c(pct_denied, pct_approval), names_to = "type", values_to = "value") 
        
        genders_total<- gender_total2 %>%
            select(lei, county_code, derived_sex, pct_denied, pct_approval) %>%
            pivot_longer(c(pct_denied, pct_approval), names_to = "type", values_to = "value") 
        
        rbind(gender_total1, genders_total) %>% 
            mutate(lei=replace(lei, lei==unique(total$lei)[1], "single")) %>%
            mutate(lei=replace(lei, lei==unique(total$lei)[2], "multiple")) %>%
            ggplot() + 
            aes(x = lei, y = value, fill = type) + 
            geom_col(position= "stack", stat="identity")+
            facet_wrap(~derived_sex, nrow=1)
        
        
    
    })
    
}

shinyApp(ui = ui, server = server)
