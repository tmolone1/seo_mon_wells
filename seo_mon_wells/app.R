library(shiny)
library(DT)

# Define UI for data download app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Downloading Data"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Choose dataset ----
      selectInput("filter1", "Apply a filter:",
                  choices = c("rock", "pressure", "cars")),
      
      # Button
      downloadButton("downloadData", "Download")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      DT::dataTableOutput("table")
      
    )
    
  )
)

# Define server logic to display and download selected file ----
server <- function(input, output) {
  
  # Reactive value for selected dataset ----
  datasetInput <- reactive({
    switch(input$dataset,
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars)
  })
  
  # Table of selected dataset ----
  output$table <- DT::renderDT(server=FALSE, {
    DT::datatable(dattbl,
                  extensions = 'Buttons',
                  options = list(
                    dom = 'Bfrtip',
                    pageLength= 100,
                    buttons =
                      list(
                        list(
                          extend = 'csv',
                          buttons = c('csv'),
                          exportOptions = list(
                            modifiers = list(page = "current")
                          )
                        ),
                        list(extend = "csv", text = "Download Full Results", filename = "data",
                             exportOptions = list(
                               modifier = list(page = "all")
                             ))
                                ))
                    )
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
  
}

# Create Shiny app ----
shinyApp(ui, server)