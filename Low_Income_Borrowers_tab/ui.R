#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(plotly)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("HDMA Analysis"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      
      selectInput("select_msa", label = h4("Regions (MSA/MD code)"),
                        choices = msa_codes,
                        selected = 1
                  ),
      
      selectInput("lei",
                  "Select Lender:",
                  choices = washington %>% pull(lei)%>%unique()%>%sort())
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("barPlot")
      
      
    )
  )
))

