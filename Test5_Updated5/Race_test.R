
library(shiny)
library(tidyverse)
library(shinythemes)
library (plotly)

washington <-read_csv('data/state_WA.csv')
washington <- washington %>% filter(loan_purpose == "1", occupancy_type == "1")  
washington <- left_join(washington, lei_names, by = "lei") 
washington <- washington %>% mutate(name_trunc = substr(name, 1,20))

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
  group_by(derived_sex) %>%
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