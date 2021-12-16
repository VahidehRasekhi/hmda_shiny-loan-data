#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram

shinyServer(function(input, output) {
  
  observeEvent(input$debug, {
    browser()
  })
 
}
  
#need to add logic for select input
  
  )
  output$scatPlot <- renderPlot({
    
    white_minority%>% 
      filter(county_code==53001) %>% 
      select(lei, bi_race, pct_approval) %>%
      pivot_wider(names_from = bi_race, values_from = pct_approval) %>% 
      ggplot(aes(x=Minority, y=White)) + geom_point(color='red', size=3) + 
      ggtitle("Originations Per Lender to White and Minority Borrowers '%'")
  })
  
})