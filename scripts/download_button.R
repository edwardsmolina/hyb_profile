library(tidyverse)
library(shiny)
library(DT)

species <- unique(iris$Species)

# shiny app 
ui <- fluidPage(
  h1("Data Download Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("Species", label = "Choose species", 
                  species, 
                  multiple = TRUE, selected = species), 
      downloadButton("download1","Download Table as csv")
    ),
    mainPanel(h4("Table 1: Iris"),
              dataTableOutput("iris_dto")
    )
  ))

server <- function(input, output, session) {
  
  thedata <- reactive({
    iris %>% 
      filter(Species == input$Species)
  })
  
  output$iris_dto <- renderDataTable({
    thedata()  %>% 
      datatable(extensions = 'Buttons',
                options = list(
                  dom = 'lfrtipB',
                  buttons = c("copy", "csv", "pdf")),
                filter = list(position = 'top'),
                rownames = FALSE)
  })
  
  
  output$download1 <- downloadHandler(
    filename = function() {
      paste("iris_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(thedata(), file)
    }
  )
}

shinyApp(ui, server)
