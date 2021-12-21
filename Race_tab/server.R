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
    
   total%>%ggplot(aes(x=lei, y=values), fill=race)+ geom_bar(position="stack", stat="identity",aes(fill=race)) +
      coord_flip() + labs(title="Loan Denial Rates by Race ") + 
      theme(plot.title = element_text(size = 20, color = "orange")) + 
    labs(y=" % pct")
    #ggplotly(total)
    
    
    })
   
  
})
