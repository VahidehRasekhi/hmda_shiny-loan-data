library(tidyverse)

washington <- read.csv("data/state_WA.csv")

wash_init <- washington %>% filter(loan_purpose == "1", occupancy_type == "1") %>%  
  wash_init %>% select(lei, derived_msa.md, derived_sex, action_taken) %>% 
  filter(lei == "549300MGPZBLQDIL7538", derived_msa.md == "38900") %>% 
  filter(action_taken %in% c(1,3)) %>% 
  count(action_taken, derived_sex) %>% 
  ggplot() + 
  aes(x = derived_sex, y = n, fill = action_taken) + 
  geom_col(position= "stack", stat="identity")


+
  facet_wrap(~derived_sex, nrow=1)

lei_names <- read.csv("data/lei_name.csv")
washington <-read_csv('data/state_WA.csv')
washington <- washington %>% filter(loan_purpose == "1", occupancy_type == "1")  
washington <- left_join(washington, lei_names, by = "lei") 
washington <- washington %>% mutate(name_trunc = substr(name, 1,20))

w5 <- washington %>% head(5)
substr(w5, 1,20)
w5
w5 <- unique(washington$name) %>% head(5)
washington$name_trunc

w5 <- w5 %>% select(lei)
w5
left_join(w5, washington %>% select(lei, name_trunc), by = "lei")
















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

low_income_combo <- left_join(low_income_combo, washington %>% 
                                select(lei, name_trunc), by = "lei") %>%
  select(-lei)

low_income_combo %>% ggplot(aes(x=name_trunc, y=pct_approval, fill = marker1)) + 
  geom_bar(stat="identity") +  coord_flip() +
  labs(title="Loans Approved to Low Income Borrowers '%") + 
  theme(plot.title = element_text(size = 14, face= "bold")) + 
  labs(y = "Low Income Borrowers %") + theme(legend.position = "none") +
  geom_hline(yintercept= avg_approval)