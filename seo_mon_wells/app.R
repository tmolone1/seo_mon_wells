library(shiny)
library(dplyr)
library(DT)
library(googledrive)

location<-googlesheets4::read_sheet(drive_get("~/Projects/seo_mon_wells/location")$id)
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
                  choices = names(dattbl),
                  selected="monitor_category"),
      
      
      uiOutput("filterUI"),
      
      
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
  
  # UI for filter
  output$filterUI<-renderUI({
    checkboxGroupInput("filter2",  "records to include", 
                       choices=unique(dattbl[,input$filter1]), 
                       selected=unique(dattbl[,input$filter1]))
  })
  
  re <- reactive({
    location |> 
      filter(.data[[input$filter1]] %in% input$filter2)
  })
  
  # Table of selected dataset ----
  output$table <- DT::renderDT(server=FALSE, {
    DT::datatable(re(),
                  extensions = 'Buttons',
                  style = 'bootstrap',
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
      paste(input$filter1, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(re(), file, row.names = FALSE)
    }
  )
  
}

# Create Shiny app ----
shinyApp(ui, server)