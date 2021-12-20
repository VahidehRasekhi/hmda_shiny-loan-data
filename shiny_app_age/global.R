library(shiny)
library(tidyverse)
library(httr)

wash_init<- read_csv('./data/wash_init.csv')
lei_names <- read_csv('./data/lei_name.csv')
lars_names<- read_csv('./data/lars_lookup.csv')



#looking at age in county code 53033 and lei# 01KWVG908KE7RKPTNP46 
age1<- wash_init %>% 
  filter(applicant_age!= 8888) %>% 
  filter(applicant_age!= 9999) %>% 
  filter(county_code==53033,lei=='1IE8VN30JCEQV1H4R804') %>% 
  group_by(applicant_age, lei, `derived_msa-md`, action_taken) %>% 
  count(lei, applicant_age) %>% 
  pivot_wider(names_from = applicant_age, values_from = n)  
age1


#looking at age in county code 53033 and lei# 01KWVG908KE7RKPTNP46/549300XY701IELCE5Q08  
age2<- wash_init %>% 
  filter(applicant_age!= 8888) %>% 
  filter(applicant_age!= 9999) %>% 
  filter(county_code==53033,lei %in% c('1IE8VN30JCEQV1H4R804', "549300XY701IELCE5Q08")) %>% 
  group_by(applicant_age, lei, `derived_msa-md`, action_taken) %>% 
  count(lei, applicant_age) %>% 
  pivot_wider(names_from = applicant_age, values_from = n)  
age2


#ageapproval/denial ratio in county code 53033 and lei#1IE8VN30JCEQV1H4R804
age_approval_rate1<-wash_init%>%
  filter (county_code==53033, lei== "1IE8VN30JCEQV1H4R804", applicant_age!=888) %>% 
  select(lei,`derived_msa-md`,county_code, census_tract, applicant_age, action_taken, tract_population, tract_minority_population_percent)%>% 
  mutate(approved=ifelse(action_taken == 1 , 1, 0)) %>% 
  group_by(lei, county_code, `derived_msa-md`, applicant_age) %>% 
  summarize(approved=sum(approved), denied=n()-sum(approved)) %>%
  mutate(pct_approval=approved/(approved+denied)) %>% mutate(pct_denied=(1-pct_approval))
age_approval_rate1


#age/approval/denial ratio in county code 53033 and lei#1IE8VN30JCEQV1H4R804/549300XY701IELCE5Q08
age_approval_rate2<-wash_init%>%
  filter (county_code==53033, lei %in% c("1IE8VN30JCEQV1H4R804", "549300XY701IELCE5Q08", 
                                         applicant_age!=888)) %>% 
  select(lei,`derived_msa-md`,county_code, census_tract, applicant_age, action_taken, tract_population, tract_minority_population_percent)%>% 
  mutate(approved=ifelse(action_taken == 1 , 1, 0)) %>% 
  group_by(lei, county_code, `derived_msa-md`, applicant_age) %>% 
  summarize(approved=sum(approved), denied=n()-sum(approved)) %>%
  mutate(pct_approval=approved/(approved+denied)) %>% mutate(pct_denied=(1-pct_approval))
age_approval_rate2


#total application per lender in county code 53033 and lei#1IE8VN30JCEQV1H4R804 
age_total_app1<- age_approval_rate1 %>% 
  filter(county_code==53033, lei=="1IE8VN30JCEQV1H4R804", applicant_age!=888) %>% 
  group_by(`derived_msa-md`,county_code, lei) %>% 
  summarize(total_application=sum(approved)+ sum(denied)) 
age_total_app1


#total application per lender in county code 53033 and lei#1IE8VN30JCEQV1H4R804/549300XY701IELCE5Q08 
age_total_app2<- age_approval_rate2 %>% 
  filter(county_code==53033, lei %in% c("1IE8VN30JCEQV1H4R804", "549300XY701IELCE5Q08", 
                                        applicant_age!=888)) %>% 
  group_by(`derived_msa-md`,county_code, lei) %>% 
  summarize(total_application=sum(approved)+ sum(denied)) 
age_total_app2



#total application, percentage of approved/denied acpplicaiton based on age per lender in county code 53033 and lei#1IE8VN30JCEQV1H4R804
age_total1 <- merge(age_total_app1, age_approval_rate1) %>% 
  filter(applicant_age!=888) %>% 
  mutate(pct_approval=100*approved/total_application, pct_denied=100*denied/total_application)
age_total1


#total application, percentage of approved/denied acpplicaiton based on age per lender in county code 53033 and lei#1IE8VN30JCEQV1H4R804/549300XY701IELCE5Q08
age_total2 <- merge(age_total_app2, age_approval_rate2) %>% 
  filter(applicant_age!=888) %>% 
  mutate(pct_approval=100*approved/total_application, pct_denied=100*denied/total_application)
age_total2


#plotting denial/approval rate based on age in county_code 53033 and lei#1IE8VN30JCEQV1H4R804  
age_total1 %>%
  filter(applicant_age!= 8888) %>% 
  select(applicant_age, pct_denied, pct_approval) %>%
  pivot_longer(c(pct_denied, pct_approval), names_to = "type", values_to = "value") %>%
  ggplot() + 
  aes(x = applicant_age, y = value, fill = type) + 
  geom_col()


#side by side plotting of denial/approval rate based on age in county_code 53033 and lei#1IE8VN30JCEQV1H4R804/549300XY701IELCE5Q08 

age_total1 <- age_total1 %>%
  select(lei, county_code, applicant_age, pct_denied, pct_approval) %>%
  pivot_longer(c(pct_denied, pct_approval), names_to = "type", values_to = "value") 

ages_total<- age_total2 %>%
  select(lei, county_code, applicant_age, pct_denied, pct_approval) %>%
  pivot_longer(c(pct_denied, pct_approval), names_to = "type", values_to = "value") 

rbind(age_total1, ages_total) %>% 
  mutate(lei=replace(lei, lei==unique(total$lei)[1], "single")) %>%
  mutate(lei=replace(lei, lei==unique(total$lei)[2], "multiple")) %>%
  filter(applicant_age!=888) %>% 
  ggplot() + 
  aes(x = lei, y = value, fill = type) + 
  geom_col(position= "stack", stat="identity")+
  facet_wrap(~applicant_age, nrow=1)

