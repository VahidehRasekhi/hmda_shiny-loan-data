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
            
            low_income_comparison<-low_income_combo %>% ggplot(aes(x=lei, y=low, fill = marker1, text=total_applic)) + geom_bar(stat="identity") +  coord_flip() +
                geom_hline(yintercept= by_income_pct$avg_pct_approval_rate[1]) +
                labs(title="Loans Approved to Low Income Borrowers '%") + theme(plot.title = element_text(size = 14, face= "bold")) +
                labs(y = "Low Income Borrowers %") + theme(legend.position = "none") #+
            ggplotly(low_income_comparison, tooltip = "text")
            
            
        })    

})