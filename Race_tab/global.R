
library(shiny)
library(tidyverse)
library(plotly)
washington <-read_csv('data/state_WA.csv')
msa_codes <- c("WA", 0, 13380, 14740, 28420, 30300, 31020, 34580, 36500,
38900, 42644, 44060, 45104, 47460, 48300, 49420, 99999)

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
  filter(`derived_msa-md`==42644 & lei == "0S8H5NJFLHEVJXVTQ413")
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

total%>%ggplot(aes(x=lei, y=values, text=lei), fill=race)+ geom_bar(position="stack", stat="identity",aes(fill=race))+
  coord_flip() + labs(title="Loan Denial Rates by Race ") + 
  theme(plot.title = element_text(size = 20, color = "orange")) + 
  labs(y=" % pct")
#ggplotly(race_chart, tooltip = "text")

