library(shiny)
library(dplyr)
library(DT)
library(janitor)
library(readr)
library(shinycssloaders)
library(cowplot)
library(ggplot2)

longitude<-read_csv("https://seoflow.wyo.gov/Data/ExportList?interval=Latest&parameters[0]=24&value=Location_9&type=Location&subValue=null&subValueType=null&refPeriod=&legend=null&utcOffset=420&date=2023-02-16&endDate=null&calendar=1&wkid=4326&exportType=csv&filter=~&sort=Sequence-desc", 
                    skip=1) |> clean_names() |> dplyr::select("location","value") |> rename(longitude=value) |> unique() |> tibble()
latitude<-read_csv("https://seoflow.wyo.gov/Data/ExportList?interval=Latest&parameters[0]=24&value=Location_8&type=Location&subValue=null&subValueType=&refPeriod=&legend=null&utcOffset=420&date=2023-02-16&endDate=null&calendar=1&wkid=4326&exportType=csv&filter=~&sort=Sequence-desc", 
                   skip=1) |> clean_names() |> dplyr::select("location","value") |> rename(latitude=value) |> unique() |> tibble()
elevation<-read_csv("https://seoflow.wyo.gov/Data/ExportList?interval=Latest&parameters[0]=24&value=Location_3&type=Location&subValue=null&subValueType=null&refPeriod=&legend=null&utcOffset=360&date=2023-03-17&endDate=null&calendar=1&wkid=4326&exportType=csv&filter=~&sort=Sequence-desc", 
                   skip=1) |> clean_names() |> dplyr::select("location","value") |> rename(elevation=value) |> unique() |> tibble()
locations<-read_csv("https://seoflow.wyo.gov/Data/Export_Folder/?folder=61&utcoffset=420&exportType=csv&filter=~&sort=Identifier-asc", 
                    skip=1) |> clean_names() |> tibble()
data_sets<-read_csv("https://seoflow.wyo.gov/Data/ExportList?interval=Latest&parameters[0]=24&value=Location_1&type=Location&subValue=null&subValueType=null&refPeriod=&legend=null&utcOffset=420&date=2023-02-16&endDate=null&calendar=1&wkid=4326&exportType=csv&filter=~&sort=Sequence-desc", 
                    skip=1) |> clean_names() |> tibble()
record_dates<-data_sets |> group_by(location) |> summarize(start_rec=min(start_of_record), end_rec=max(end_of_record))

locations<-left_join(locations,longitude, by="location") 
locations<-left_join(locations,latitude, by="location")
locations<-left_join(locations,elevation, by="location")
locations<-left_join(locations,record_dates, by="location") |> tibble()

data_sets <- data_sets |> mutate(meas_type="NA")

# this set of statements "guesses" which dataset is the field measurements and which is the transducer data
data_sets$meas_type[grep("well",data_sets$data_set_id)] <- "recorded"
data_sets$meas_type[grep("pth.Well",data_sets$data_set_id)] <- "recorded"
data_sets$meas_type[grep("epth.Depth",data_sets$data_set_id)] <- "recorded"
data_sets$meas_type[grep("depth below LSD",data_sets$data_set_id)] <- "recorded"
data_sets$meas_type[grep("isits",data_sets$data_set_id)] <- "field"

cols<-c("black","royalblue3")
shapes<-c(0,16)
sized<-c(3.3,1.2)


# Define UI for data download app ----
ui <- fluidPage("Wyoming State Engineer's Office Ground Water Monitoring Wells",
  
  # App title ----
  titlePanel("Hydrograph"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Choose dataset ----
      selectInput("filter1", "Location folder:",
                  choices = locations |> dplyr::select("location_folder") |> unique() |> pull(),
                  selected=NULL),
      
      selectInput("datasets","Choose a data set to view:",
                  choices = c("recorded", "field", "both"),
                  selected="both"),
      
      
      withSpinner(uiOutput("dateUI")),
      withSpinner(uiOutput("ylimUI")),
      
      actionLink("resetdaterange","Reset Date Range to All Data"),
      
      uiOutput("filterUI"),
      
      # Button
      downloadButton("downloadData", "Download")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(
        tabPanel("Plot",
                 # Button
                 downloadButton("exportplot", "Download Plot as PDF"),
                 withSpinner(plotOutput("plot"))
                 ),
        tabPanel("Table",
      DT::dataTableOutput("table"),
      textOutput('string'),
      withSpinner(DT::dataTableOutput("table2")))
      )
    )
    
  )
)

# Define server logic to display and download selected file ----
server <- function(session, input, output) {
  
  # UI for filter
  output$filterUI<-renderUI({
    radioButtons("filter2",  "records to include", 
                       choices=locations |> filter(location_folder==input$filter1) |> dplyr::select(location) |> unique() |> pull(), 
                       selected=NULL)
  })
  
  # UI for dateRange
  output$dateUI<-renderUI({
    dateRangeInput("dateRange","Choose a date range (defaults to all available data):",
                   start=location() |> select(start_rec) |> pull(),
                   end = location() |> select(end_rec) |> pull(),
                   min = min(na.omit(data_sets$start_of_record)),
                   max = max(na.omit(data_sets$end_of_record)))
  })
  
  # UI for dateRange
  output$ylimUI<-renderUI({
    sliderInput("ylims","Adjust y axis limits (defaults to all available data):",
                   value = range(plotdata() |> select(value_ft) |> pull()),
                   min = -1,
                   max = ceiling(max(plotdata() |> select(value_ft) |> pull())))
  })
  
  
  re <- reactive({
      data_sets |> filter(location %in% input$filter2)
  })
  
  re2 <-reactive({
    if (input$datasets=="both") {re()}
    else {re() |> filter(meas_type==input$datasets)}
  })
  
  recorded_query_string <- reactive({
    re2() |> filter(meas_type=="recorded") |> select(data_set_id) |> pull() |> URLencode()
  })
  
  recorded <- reactive({
    read_csv(paste0("https://seoflow.wyo.gov/Export/DataSet?DataSet=",
                    recorded_query_string(),
                    "&ExportFormat=csv&Compressed=false&RoundData=False&Unit=&Timezone=0"),
             skip=4, col_types = c("T","d")) |> clean_names() |> tibble() |> mutate(measurement_type="recorded")
  })
  
  field_query_string <- reactive({
    re2() |> filter(meas_type=="field") |> select(data_set_id) |> pull() |> URLencode()
  })
  
  field <- reactive({
    read_csv(paste0("https://seoflow.wyo.gov/Export/DataSet?DataSet=",
                    field_query_string(),
                    "&ExportFormat=csv&Compressed=false&RoundData=False&Unit=&Timezone=0"),
             skip=4, col_types = c("T","d")) |> clean_names() |> tibble() |> mutate(measurement_type="field")
    
  })
  
  viewtbl<-reactive({
    if (input$datasets=="both") {recorded() |> bind_rows(field()) |> filter(!is.na(value_ft))}
    else if (input$datasets=="recorded") {recorded()|> filter(!is.na(value_ft))}
    else {field()|> filter(!is.na(value_ft))}
  })
  
  plotdata<-reactive({
    viewtbl() |> mutate(timestamp_utc=as.Date(timestamp_utc), value_ft=as.numeric(value_ft))
  })
  
  location<-reactive({
    locations |> filter(location==input$filter2)
  })
  
  draw<- reactive({
    ggdraw() + draw_plot(ggplot(plotdata() , aes(x = timestamp_utc, y = value_ft)) + 
                           geom_point(aes(color=measurement_type, shape=measurement_type, size=measurement_type)) + 
                           xlab("") +
                           ylab("Depth below ground surface (ft)") +
                           labs(title=input$filter2, 
                                subtitle = paste0("Station number: ",location() |> select(identifier) |> pull(),"     Latitude: ",location() |> select(latitude) |> pull(),"     Longitude: ",location() |> select(longitude) |> pull()," \n Period of record: ",format(location() |> select(start_rec) |> pull(),"%m/%d/%Y")," - ", format(location() |> select(end_rec) |> pull(),"%m/%d/%Y")),
                                caption = paste0("Date prepared:     ",format(Sys.time(),'%B %d, %Y %H:%M')),
                                color = "Measurement type:", 
                                shape = "Measurement type:",
                                size = "Measurement type:") +
                           theme_classic() +
                           theme(
                             plot.title = element_text(hjust = 0.5, face="bold"),
                             plot.subtitle = element_text(hjust = 0.5),
                             plot.caption = element_text(hjust=0, color = "ivory4"),
                             axis.text.x=element_text(angle=60, hjust=1),
                             plot.margin = margin(1,1,1,1,unit = "in"),
                             legend.position="bottom",
                             legend.justification = "left",
                             panel.grid.major = element_line(color = "ivory4",
                                                             size = 0.4,
                                                             linetype = 3),
                             panel.border = element_rect(color="azure4", fill=NA)
                           ) + 
                           coord_cartesian(ylim=rev(input$ylims)) + 
                           scale_y_reverse() + 
                           scale_x_date(date_breaks = "1 years", date_labels = "%Y", limits=input$dateRange) + 
                           scale_shape_manual(values=shapes, labels=c("Field measurement", "Recorded water level")) + 
                           scale_color_manual(values=cols, labels=c("Field measurement", "Recorded water level")) + 
                           scale_size_manual(values=sized, labels=c("Field measurement", "Recorded water level"))) +
      draw_image("https://github.com/tmolone1/seo_mon_wells/raw/main/Hydrographs/data/SEO_logo_BW.png", x = 0.37, y= -0.36, scale =.11, vjust=0,hjust=0)
  })
  
  # Table of selected dataset ----
  output$table <- DT::renderDT(server=FALSE, {
    DT::datatable(re2(),
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
  
  output$string<-renderText(recorded_query_string())
  
  output$table2<-DT::renderDT(viewtbl())
  output$plot<-renderPlot(draw(), width = 980, height= 720)
  
  output$plotsimple<-renderPlot(ggplot(plotdata(), aes(x = timestamp_utc, y = value_ft)) +
    geom_point(aes(color=measurement_type, shape=measurement_type, size=measurement_type)))
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$filter2, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(viewtbl(), file, row.names = FALSE)
    }
  )
  
  output$exportplot <- downloadHandler(
    filename = function() {
      paste(input$filter2, ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file, family = "sans",width=11,height=8.5,pagecentre = TRUE)
      print(draw())
      dev.off()
    }
  )
  
  observe({
    if(input$resetdaterange == 0) return(NULL) 
    else if (input$resetdaterange%%2 == 0)
    {
      updateDateRangeInput(session,"dateRange","Choose a date range (defaults to all available data):",
                     start=location() |> select(start_rec) |> pull(),
                     end = location() |> select(end_rec) |> pull(),
                     min = min(na.omit(data_sets$start_of_record)),
                     max = max(na.omit(data_sets$end_of_record)))    }
    else
    {
      updateDateRangeInput(session,"dateRange","Choose a date range (defaults to all available data):",
                           start=location() |> select(start_rec) |> pull(),
                           end = location() |> select(end_rec) |> pull(),
                           min = min(na.omit(data_sets$start_of_record)),
                           max = max(na.omit(data_sets$end_of_record)))     }
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)