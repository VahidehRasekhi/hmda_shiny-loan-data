
library(shiny)
library(tidyverse)
library(shinythemes)
library (plotly)

washington <-read_csv('data/state_WA.csv')
washington <- washington %>% filter(loan_purpose == "1", occupancy_type == "1")  
washington <- left_join(washington, lei_names, by = "lei") 
washington <- washington %>% mutate(name_trunc = substr(name, 1,20))

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
  filter(lei == lei$single) 

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