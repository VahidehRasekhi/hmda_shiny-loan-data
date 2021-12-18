library(shiny)
library(tidyverse)
library(httr)

wash_init<- read_csv('./data/wash_init.csv')
lei_names <- read_csv('./data/lei_name.csv')
lars_names<- read_csv('./data/lars_lookup.csv')


#looking at gender in county code 53033
gender<- wash_init %>% 
  filter(county_code==53033) %>% 
  group_by(derived_sex, lei, `derived_msa-md`, action_taken) %>% 
  count(lei, derived_sex) %>% 
  pivot_wider(names_from = derived_sex, values_from = n)  
gender


#looking at gender in county code 53033 and lei# 01KWVG908KE7RKPTNP46 
gender1<- wash_init %>% 
  filter(county_code==53033,lei=='1IE8VN30JCEQV1H4R804') %>% 
  group_by(derived_sex, lei, `derived_msa-md`, action_taken) %>% 
  count(lei, derived_sex) %>% 
  pivot_wider(names_from = derived_sex, values_from = n)  
gender1


#looking at gender in county code 53033 and lei# 01KWVG908KE7RKPTNP46/549300XY701IELCE5Q08  
gender2<- wash_init %>% 
  filter(county_code==53033,lei %in% c('1IE8VN30JCEQV1H4R804', "549300XY701IELCE5Q08")) %>% 
  group_by(derived_sex, lei, `derived_msa-md`, action_taken) %>% 
  count(lei, derived_sex) %>% 
  pivot_wider(names_from = derived_sex, values_from = n)  
gender2


#gender/approval/denial ratio in county code 53033
gender_approval_rate<-wash_init%>%
  filter (county_code==53033) %>% 
  select(lei,`derived_msa-md`,county_code, census_tract, derived_sex, action_taken, tract_population, tract_minority_population_percent)%>% 
  mutate(approved=ifelse(action_taken == 1, 1, 0)) %>% 
  group_by(lei, county_code, `derived_msa-md`, derived_sex) %>% 
  summarize(approved=sum(approved), denied=n()-sum(approved)) %>%
  mutate(pct_approval=approved/(approved+denied)) %>% mutate(pct_denied=(1-pct_approval))
gender_approval_rate


#gender/approval/denial ratio in county code 53033 and lei#1IE8VN30JCEQV1H4R804
gender_approval_rate1<-wash_init%>%
  filter (county_code==53033, lei== "1IE8VN30JCEQV1H4R804") %>% 
  select(lei,`derived_msa-md`,county_code, census_tract, derived_sex, action_taken, tract_population, tract_minority_population_percent)%>% 
  mutate(approved=ifelse(action_taken == 1 , 1, 0)) %>% 
  group_by(lei, county_code, `derived_msa-md`, derived_sex) %>% 
  summarize(approved=sum(approved), denied=n()-sum(approved)) %>%
  mutate(pct_approval=approved/(approved+denied)) %>% mutate(pct_denied=(1-pct_approval))
gender_approval_rate1

#gender/approval/denial ratio in county code 53033 and lei#1IE8VN30JCEQV1H4R804/549300XY701IELCE5Q08
gender_approval_rate2<-wash_init%>%
  filter (county_code==53033, lei %in% c("1IE8VN30JCEQV1H4R804", "549300XY701IELCE5Q08")) %>% 
  select(lei,`derived_msa-md`,county_code, census_tract, derived_sex, action_taken, tract_population, tract_minority_population_percent)%>% 
  mutate(approved=ifelse(action_taken == 1 , 1, 0)) %>% 
  group_by(lei, county_code, `derived_msa-md`, derived_sex) %>% 
  summarize(approved=sum(approved), denied=n()-sum(approved)) %>%
  mutate(pct_approval=approved/(approved+denied)) %>% mutate(pct_denied=(1-pct_approval))
gender_approval_rate2

#total application per lender in county code 53033
gender_total_app<- gender_approval_rate %>% 
  filter(county_code==53033) %>% 
  group_by(`derived_msa-md`,county_code, lei) %>% 
  summarize(total_application=sum(approved)+ sum(denied)) 
gender_total_app


#total application per lender in county code 53033 and lei#1IE8VN30JCEQV1H4R804 
gender_total_app1<- gender_approval_rate %>% 
  filter(county_code==53033, lei=="1IE8VN30JCEQV1H4R804") %>% 
  group_by(`derived_msa-md`,county_code, lei) %>% 
  summarize(total_application=sum(approved)+ sum(denied)) 
gender_total_app1


#total application per lender in county code 53033 and lei#1IE8VN30JCEQV1H4R804/549300XY701IELCE5Q08 
gender_total_app2<- gender_approval_rate %>% 
  filter(county_code==53033, lei %in% c("1IE8VN30JCEQV1H4R804", "549300XY701IELCE5Q08")) %>% 
  group_by(`derived_msa-md`,county_code, lei) %>% 
  summarize(total_application=sum(approved)+ sum(denied)) 
gender_total_app2

#total application, percentage of approved/denied acpplicaiton based on gender per lender in county code 53033
gender_total <- merge(gender_total_app, gender_approval_rate) %>% 
  mutate(pct_approval=100*approved/total_application, pct_denied=100*denied/total_application)
gender_total

#total application, percentage of approved/denied acpplicaiton based on gender per lender in county code 53033 and lei#1IE8VN30JCEQV1H4R804
gender_total1 <- merge(gender_total_app1, gender_approval_rate1) %>% 
  mutate(pct_approval=100*approved/total_application, pct_denied=100*denied/total_application)
gender_total1

#total application, percentage of approved/denied acpplicaiton based on gender per lender in county code 53033 and lei#1IE8VN30JCEQV1H4R804/549300XY701IELCE5Q08
gender_total2 <- merge(gender_total_app2, gender_approval_rate2) %>% 
  mutate(pct_approval=100*approved/total_application, pct_denied=100*denied/total_application)
gender_total2


#plotting denial/approval rate based on gender in county_code 53033 and lei#1IE8VN30JCEQV1H4R804  
gender_total1 %>%
  select(derived_sex, pct_denied, pct_approval) %>%
  pivot_longer(c(pct_denied, pct_approval), names_to = "type", values_to = "value") %>%
  ggplot() + 
  aes(x = derived_sex, y = value, fill = type) + 
  geom_col()

#side by side plotting of denial/approval rate based on gender in county_code 53033 and lei#1IE8VN30JCEQV1H4R804/549300XY701IELCE5Q08 

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

#just putting a comment
