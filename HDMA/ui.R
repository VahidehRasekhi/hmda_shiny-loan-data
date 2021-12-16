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
  titlePanel("HMDA Analysis"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
  
    selectInput(inputId="countyCode", label="Enter County Code", choices=washington$county_code), 
    selectInput(inputId="lei", label="Enter LEI", choices=washington$lei),
    numericInput(inputId='result', label="Number of Results to view", value=20)
    
   ),
   
  mainPanel(
        fluidRow(
        plotOutput("scatPlot", height="400px")
                 )
      ), 
      fluidRow(
        plotOutput("barPlot", height="300px")
      )
      
    )
  ))


