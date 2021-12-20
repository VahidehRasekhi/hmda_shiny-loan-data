#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
 
  output$barPlot <- renderPlotly({ 
    
    #Selecting Single LEI
    
    if(input$select_msa != "WA") {
      
      lim_approved<-washington%>%
        select(lei, county_code, `derived_msa-md`, census_tract, derived_race, action_taken, tract_population, tract_minority_population_percent, income, ffiec_msa_md_median_family_income)%>%
        filter(`derived_msa-md`== input$select_msa, income<0.5*ffiec_msa_md_median_family_income) %>% 
        mutate(approved=ifelse(action_taken == 1, 1, 0)) %>%
        group_by(lei) %>% 
        summarize(approved=sum(approved), denied=n()-sum(approved))%>%
        mutate(pct_approval=100*approved/(approved+denied), pct_denied=100*denied/(approved+denied))
      
      
   
    }
    
    else {
      
      
      lim_approved<-washington%>%
        select(lei, county_code, `derived_msa-md`, census_tract, derived_race, action_taken, tract_population, tract_minority_population_percent, income, ffiec_msa_md_median_family_income)%>%
        filter(income<0.5*ffiec_msa_md_median_family_income) %>% 
        mutate(approved=ifelse(action_taken == 1, 1, 0)) %>%
        group_by(lei) %>% 
        summarize(approved=sum(approved), denied=n()-sum(approved))%>%
        mutate(pct_approval=100*approved/(approved+denied), pct_denied=100*denied/(approved+denied))
      
      
    }
      
    
    low_income1<-lim_approved%>%
      filter(lei == input$lei)%>%
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
    
    
      })
   
  
})
