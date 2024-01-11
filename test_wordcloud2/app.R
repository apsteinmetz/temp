library(shiny)

# Define UI for dataset viewer app ----
app <- shinyApp(
  ui = fluidPage(
    
    # App title ----
    titlePanel("Selectize Quirk"),
    
    sidebarLayout(
      sidebarPanel(
        selectInput('selection', 
                       choices = state.name, 
                       label= "Choose selectInput")
      ),
      
      # Main panel
      mainPanel(
        textOutput("my_state")
      )
    )
  ),
  
  # Define server logic to summarize and view selected dataset ----
  server = function(input, output,session) {
    output$my_state <- renderText(input$selection)
  }
)
