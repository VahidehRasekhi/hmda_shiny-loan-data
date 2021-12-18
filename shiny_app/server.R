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
  
  output$barPlot <- renderPlot({
    
    rbind(gender_total1, genders_total) %>% 
      mutate(lei=replace(lei, lei==unique(total$lei)[1], "single")) %>%
      mutate(lei=replace(lei, lei==unique(total$lei)[2], "multiple")) %>%
      ggplot() + 
      aes(x = lei, y = value, fill = type) + 
      geom_col(position= "stack", stat="identity")+
      facet_wrap(~derived_sex, nrow=1)
    
  })
  
})    
    
    
 

