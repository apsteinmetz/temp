library("shiny")
library(dplyr)
# mylist
#selectList <- sapply(1:15000, function(x) paste0(x, "_", paste(sample(letters, 10), collapse = '')))
load("~/R Projects/wfmu_explorer/playlists.Rdata")
selectList<-playlists %>% 
  ungroup() %>% 
  select(ArtistToken) %>% 
  unique() %>% 
  pull(ArtistToken)
# ui
ui <- fluidPage(
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
# server
server <- function(input, output, session) {
  
  updateSelectizeInput(session = session, inputId = 'mylist', choices = selectList, server = TRUE)
  output$chosen<-renderText(input$mylist)
}
# app
shinyApp(ui = ui, server = server)