library(shiny)
library(tidyverse)
library(plotly)
washington <-read_csv('data/state_WA.csv')

#1. Total count if approved and denied loans by county_code, lei
wa_race<-washington%>%
  select(lei, county_code, census_tract, derived_race, action_taken, tract_population, tract_minority_population_percent)%>%
  mutate(approved=ifelse(action_taken == 1, 1, 0)) %>% group_by(lei, county_code, derived_race) %>%
  summarize(approved=sum(approved), denied=n()-sum(approved))

#2. Compute % of Loans Denied and Approved by county_code and lei
wa_total <- wa_race %>% group_by(lei, county_code) %>% summarize(total_applic=sum(approved)+sum(denied))
wa_result <- merge(wa_total, wa_race, by=c("lei","county_code")) %>% 
  mutate(pct_approval=100*approved/total_applic, pct_denied=100*denied/total_applic)

#3. Regroup Race by White and Minority by adding all non-White together
wa_minority <- wa_result %>% 
  mutate(bi_race=ifelse(derived_race=='White', 'White', 'Minority')) %>% 
  group_by(lei, county_code,bi_race) %>% summarise(approved=sum(approved), denied=sum(denied))

#4. Compute pct_approved, denied for White and Monority Borrowers
wa_county <- wa_minority %>% group_by(lei, county_code) %>% summarise(total_applic=sum(approved) + sum(denied))
white_minority<-merge(wa_minority, wa_county, by=c("lei","county_code")) %>% 
mutate(pct_approval=100*approved/total_applic, pct_denied=100*denied/total_applic)

#5. Scaterplot to Show % of Loans approved to Minority and White Borrowers per county:
scatterplot<-white_minority%>% 
  filter(county_code==53001) %>% 
  select(lei, bi_race, pct_approval) %>%
  pivot_wider(names_from = bi_race, values_from = pct_approval) %>% 
  replace(is.na(.), 0) %>%
  ggplot(aes(x=Minority, y=White, text=lei)) + geom_point(alpha=0.8, colour = "#51A0D5", size=2) + 
  labs(title="Loans Approved to White and Minority Borrowers '%") + theme(plot.title = element_text(size = 14, face= "bold")) + labs(face= "bold")
  ggplotly(scatterplot, tooltip = "text")
  
#6. Tract_Minority_Pop_Pct
washington%>%select(county_code, tract_minority_population_percent)%>%
   group_by(county_code)%>%
   distinct(county_code, tract_minority_population_percent)
 

#7. Table that shows % of Loan Originated to Borrowers By Race per County per Lei
#originations_by_race<-wa_result %>% 
  #select(lei, county_code, derived_race, pct_approval) %>% 
  #pivot_wider(names_from = derived_race, values_from = pct_approval, values_fill = list(value=0))%>%
  #replace(is.na(.), 0)%>%
  #rowwise%>%
  #mutate(Other=sum(c_across(c('Joint','Free Form Text Only', '2 or more minority races', 'Race Not Available')))) %>% 
  #select(lei, county_code, c("White", "Asian", "Black or African American", "American Indian or Alaska Native" , "Native Hawaiian or Other Pacific Islander", "Other") )

#8.#Denial Rates Table + Work on colorcoding of pct_denied based on a condition
#wa_result %>% 
  #select(lei, county_code, derived_race, pct_denied) %>% 
  #pivot_wider(names_from = derived_race, values_from = pct_denied, values_fill = list(value=0))%>%
  #replace(is.na(.), 0)%>%rowwise%>%mutate(Other=sum(c_across(c('Joint','Free Form Text Only', '2 or more minority races', 'Race Not Available')))) %>% 
  #select(lei, county_code, c("White", "Asian", "Black or African American", "American Indian or Alaska Native" , "Native Hawaiian or Other Pacific Islander", "Other") )


  
#9 Low Income Data and Chart
  by_income_count<-washington%>% 
    select(lei, county_code, census_tract, derived_race, action_taken, tract_population, tract_minority_population_percent, income, ffiec_msa_md_median_family_income) %>% 
    mutate(approved=ifelse(action_taken == 1, 1, 0), low_income=ifelse(income<0.5*ffiec_msa_md_median_family_income, "low", "normal")) %>% 
    group_by(lei, county_code, low_income) %>% 
    summarize(approved=sum(approved), denied=n()-sum(approved))
  
  
  by_income_pct<-merge(wa_total, by_income_count, by=c("lei","county_code")) %>% 
    mutate(pct_approval=100*approved/total_applic, pct_denied=100*denied/total_applic) %>% 
    filter(low_income=="low")%>%group_by(county_code) %>% 
    mutate(avg_pct_approval_rate=mean(pct_approval)) %>% arrange(desc(total_applic))
  
  test<-by_income_pct%>%
    pivot_wider(names_from = low_income, values_from = pct_approval, values_fill = list(value=0))%>%
    replace(is.na(.), 0) %>% filter(county_code %in% c(53005, 53009, 53011, 53049, 53063)) %>% 
    select(lei, low, avg_pct_approval_rate, total_applic, approved) %>% head(n=20)
  
  test<-by_income_pct%>%
    pivot_wider(names_from = low_income, values_from = pct_approval, values_fill = list(value=0))%>%
    replace(is.na(.), 0) %>% filter(county_code == 53029) %>% 
    select(lei, low, avg_pct_approval_rate, total_applic, approved) %>% head(n=20)%>% 
    ggplot(aes(x=lei, y=low)) + geom_bar(stat="identity", fill="steelblue") + 
    labs(title="Loans Approved to Low Income Borrowers '%") + theme(plot.title = element_text(size = 14, face= "bold")) + 
    labs(y="pct %" , face= "bold")+ coord_flip()
    #+ geom_hline(yintercept= by_income_pct[1,4])
  ggplotly(test, tooltip = c("total_applic", "approved"))
  
  