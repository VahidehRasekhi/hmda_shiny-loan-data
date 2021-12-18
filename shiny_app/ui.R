#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    # Application title
    titlePanel("HMDA Loan Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput("select", label = h3("Select Lender"), 
                        choices = lei_names$name, 
                        selected = 1),            
            selectInput("derived_ethnicity", label = h3("Select Ethnicity"), 
                        choices = c("All", lars_names %>% 
                                        filter(lars_names == "derived_ethnicity") %>% pull(value)),
                        selected = 1),
            selectInput("derived_sex", label=h3("Select Gender"),
                        choices=c("All", lars_names %>% 
                                      filter(lars_names=="derived_sex") %>% pull(value)),
                        selected=1)
        ),
        mainPanel(
            plotOutput("barPlot")
        )
        )
))
            
         