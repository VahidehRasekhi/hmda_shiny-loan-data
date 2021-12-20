
  library(shiny)
  library(tidyverse)
  library(plotly)
  washington <-read_csv('data/state_WA.csv')
  msa_codes <- c("WA", 0, 13380, 14740, 28420, 30300, 31020, 34580, 36500,
  38900, 42644, 44060, 45104, 47460, 48300, 49420, 99999)
  
  lim_approved<-washington%>%
    select(lei, county_code, `derived_msa-md`, census_tract, derived_race, action_taken, tract_population, tract_minority_population_percent, income, ffiec_msa_md_median_family_income)%>%
    filter(`derived_msa-md`== 42644, income<0.5*ffiec_msa_md_median_family_income) %>% 
    mutate(approved=ifelse(action_taken == 1, 1, 0)) %>%
    group_by(lei) %>% 
    summarize(approved=sum(approved), denied=n()-sum(approved))%>%
    mutate(pct_approval=100*approved/(approved+denied), pct_denied=100*denied/(approved+denied))
  
  
  low_income1<-lim_approved%>%
    filter(lei == "1IE8VN30JCEQV1H4R804")%>%
    select(lei, pct_approval)
  
  low_income_multi<-lim_approved%>%
    filter(lei %in% c("0K2D5AK28E3O5CC06E35", "213800QUAI2VH5YM6310", "25490018IFQOT83Q7H49"))%>%
    select(lei, pct_approval)
  
  avg_approval<-lim_approved%>% summarize(approved=sum(approved), denied=sum(denied))%>%
    mutate(pct_approval=100*approved/(approved+denied), pct_denied=100*denied/(approved+denied))%>%
    select(pct_approval)%>%.[[1,1]]
  
  low_income1<-low_income1%>%mutate(marker1= 1)
  low_income_multi<-low_income_multi%>%mutate(marker1= 2)
  low_income_combo<-rbind(low_income1, low_income_multi)
  
  
  low_income_combo %>% ggplot(aes(x=lei, y=pct_approval, fill = marker1)) + geom_bar(stat="identity") +  coord_flip() +
    labs(title="Loans Approved to Low Income Borrowers '%") + theme(plot.title = element_text(size = 14, face= "bold")) + 
    labs(y = "Low Income Borrowers %") + theme(legend.position = "none") +
    geom_hline(yintercept= avg_approval)
  #ggplotly(low_income_comparison, tooltip = "text")

