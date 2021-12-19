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
  
  output$scatPlot <- renderPlotly({
    
    #cat(file = stderr(), "drawing histogram", input$bins, "bins", "\n")
    
scatterplotly<-white_minority%>% 
      filter(county_code==input$county_code) %>% 
      select(lei, bi_race, pct_approval) %>%
      pivot_wider(names_from = bi_race, values_from = pct_approval) %>% 
      replace(is.na(.), 0) %>%
      ggplot(aes(x=Minority, y=White, text=lei)) + geom_point(alpha=0.5, colour = "#51A0D5") + 
      labs(title="Loans Approved to White and Minority Borrowers '%") + theme(plot.title = element_text(size = 14, face= "bold")) + 
      labs(face= "bold")
      ggplotly(scatterplot, tooltip = "text")  

  })
  
  output$barPlot <- renderPlotly({ 
    
test<-by_income_pct%>%
      pivot_wider(names_from = low_income, values_from = pct_approval, values_fill = list(value=0))%>%
      replace(is.na(.), 0) %>% filter(county_code == input$county_code) %>% 
      select(lei, low, avg_pct_approval_rate, total_applic, approved) %>% head(n=20)%>% 
      ggplot(aes(x=lei, y=low, text=total_applic)) + geom_bar(stat="identity", fill="steelblue") + 
      labs(title="Loans Approved to Low Income Borrowers '%") + theme(plot.title = element_text(size = 14, face= "bold")) + 
      labs(y="pct %" , face= "bold") + coord_flip()+
      geom_hline(yintercept= by_income_pct$avg_pct_approval_rate[1]) 
      ggplotly(test, tooltip = "text")
    
    
    })
   
  
})
