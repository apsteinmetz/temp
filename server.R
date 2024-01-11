
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output,session) {
  # server
  
  updateSelectizeInput(session = session, inputId = 'mylist', choices = selectList, server = TRUE)
  output$chosen<-renderText(input$mylist)
    
})
