
library(shiny)
fluidPage(
  selectizeInput(
    inputId = 'mylist', label = 'Select Something',
    choices = NULL,
    multiple= TRUE,
    options = list(placeholder="Pick a Band")
  ),
  mainPanel({
    textOutput("chosen")
    
  })
  
)
