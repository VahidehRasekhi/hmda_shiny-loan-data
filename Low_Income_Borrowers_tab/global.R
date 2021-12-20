library(shiny)
library(tidyverse)
library(plotly)
washington <-read_csv('data/state_WA.csv')
#To get to computations for Low Income
wa_race<-washington%>%
  select(lei, county_code, `derived_msa-md`, census_tract, derived_race, action_taken, tract_population, tract_minority_population_percent)%>%
  mutate(approved=ifelse(action_taken == 1, 1, 0)) %>% group_by(lei, `derived_msa-md`, derived_race) %>%
  summarize(approved=sum(approved), denied=n()-sum(approved))
#2. Compute % of Loans Denied and Approved by county_code and lei
wa_total <- wa_race %>% group_by(lei, `derived_msa-md`) %>% summarize(total_applic=sum(approved)+sum(denied))
# Low Income Data and Chart
by_income_count<-washington%>%
  select(lei, county_code, `derived_msa-md`, census_tract, derived_race, action_taken, tract_population, tract_minority_population_percent, income, ffiec_msa_md_median_family_income) %>%
  mutate(approved=ifelse(action_taken == 1, 1, 0), low_income=ifelse(income<0.5*ffiec_msa_md_median_family_income, "low", "normal")) %>%
  group_by(lei, `derived_msa-md`, low_income) %>%
  summarize(approved=sum(approved), denied=n()-sum(approved))
head(by_income_count)
by_income_pct<-merge(wa_total, by_income_count, by=c("lei","derived_msa-md")) %>%
  mutate(pct_approval=100*approved/total_applic, pct_denied=100*denied/total_applic) %>%
  filter(low_income=="low")%>%group_by(`derived_msa-md`) %>%
  mutate(avg_pct_approval_rate=mean(pct_approval)) %>% arrange(desc(total_applic))
head(by_income_pct)
#Selecting Single LEI
low_income1<-by_income_pct%>%
  pivot_wider(names_from = low_income, values_from = pct_approval, values_fill = list(value=0))%>%
  replace(is.na(.), 0)  %>% group_by(lei, `derived_msa-md`) %>%
  filter(lei == "549300TUSRLWD8ETNR90" & `derived_msa-md`==42644)%>%
  select(lei, low, avg_pct_approval_rate, total_applic, approved)
head(low_income1)
#Selecting Mutiple Lei
low_income_multi<-by_income_pct%>%
  pivot_wider(names_from = low_income, values_from = pct_approval, values_fill = list(value=0))%>%
  replace(is.na(.), 0) %>% group_by(lei, `derived_msa-md`) %>%
  filter(lei %in% c("549300KM40FP4MSQU941", "7H6GLXDRUGQFU57RNE97",   "549300AG64NHILB7ZP05", "549300FGXN1K3HLB1R50",  "KB1H1DSPRFMYMCUFXT09") & `derived_msa-md`==42644) %>%
  select(lei, low, avg_pct_approval_rate, total_applic, approved)
head(low_income_multi)
#Combining
low_income1<-low_income1%>%mutate(marker1= 1)
low_income_multi<-low_income_multi%>%mutate(marker1= 2)
low_income_combo<-rbind(low_income1, low_income_multi)
#Comparison BarChart
low_income_comparison<-low_income_combo %>% ggplot(aes(x=lei, y=low, fill = marker1, text=total_applic)) + geom_bar(stat="identity") +  coord_flip() +
  geom_hline(yintercept= by_income_pct$avg_pct_approval_rate[1]) +
  labs(title="Loans Approved to Low Income Borrowers '%") + theme(plot.title = element_text(size = 14, face= "bold")) +
  labs(y = "Low Income Borrowers %") + theme(legend.position = "none") #+
ggplotly(low_income_comparison, tooltip = "text")