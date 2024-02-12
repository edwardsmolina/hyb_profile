# df <- read.table(h=T, 
# text="A    B    C
#       1    2    3
#       4    5    6")
# 
# vec <- c("B", "C", "A")
# df[vec]


library(DT)
library(shiny)
library(htmltools)
library(data.table)
library(dplyr)

data <- 
  data.frame(
    ID = c(1,1,1,2,2,2,3,3,3),
    Period = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
    Values = c(5, 10, 15, 0, 2, 4, 3, 6, 9),
    State = c("X0","X1","X2","X0","X2","X0", "X2","X1","X0")
  )

numTransit <- function(x, from=1, to=3){
  setDT(x)
  unique_state <- unique(x$State)
  all_states <- setDT(expand.grid(list(from_state = unique_state, to_state = unique_state)))
  dcast(x[, .(from_state = State[from], 
              to_state = State[to]), 
          by = ID]
        [,.N, c("from_state", "to_state")]
        [all_states,on = c("from_state", "to_state")], 
        to_state ~ from_state, value.var = "N"
  )
}

ui <- fluidPage(
  tags$head(tags$style(".datatables .display {margin-left: 0;}")), # < left-align the table
  h4(strong("Base data frame:")), 
  tableOutput("data"),
  h4(strong("Transition table inputs:")),
  numericInput("transFrom", "From period:", 1, min = 1, max = 3),
  numericInput("transTo", "To period:", 2, min = 1, max = 3),
  h4(strong("Output transition table:")), 
  DTOutput("resultsDT"),
)

server <- function(input, output, session) {
  results <- 
    reactive({
      results <- numTransit(data, input$transFrom, input$transTo) %>% 
        replace(is.na(.), 0) %>%
        bind_rows(summarise_all(., ~(if(is.numeric(.)) sum(.) else "Sum")))
      results <- cbind(results, Sum = rowSums(results[,-1]))
    })
  
  output$data <- renderTable(data)
  
  output$resultsDT <- renderDT(server=FALSE, {
    req(results())
    datatable(
      data = results(),
      rownames = FALSE,
      filter = 'none',
      container = tags$table(
        class = 'display',
        tags$thead(
          tags$tr(
            tags$th(colspan = 1, ''),
            tags$th(colspan = 10, sprintf('From state where initial period is = %s', input$transFrom))
          ),
          tags$tr(
            lapply(colnames(results()), tags$th)
          )
        )
      ),
      options = list(scrollX = F
                     , dom = 'ft'
                     , lengthChange = T
                     , pagingType = "numbers"  # hides Next and Previous buttons
                     , autoWidth = T
                     , info = FALSE #  hide the "Showing 1 of 2..." at bottom of table
                     , searching = FALSE  # removes search box
      ),
      class = "display"
    )
  })
  
}

shinyApp(ui, server)
