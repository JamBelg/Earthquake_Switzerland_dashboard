#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dotenv)
library(aws.s3)
library(DT)
library(plotly)
library(dplyr)
library(leaflet)
library(RColorBrewer)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  tags$head(tags$style(HTML(".dataTables_wrapper table.dataTable th, .dataTables_wrapper table.dataTable td {text-align:center;}"))),
  # Application title
  fluidRow(style='background-color:#C8E4B2;',
           column(width=1, style='display: flex;align-items: center;margin-right: 10px;',
                  tags$img(src = "earthquake(1).png", width='60px', height='60px')),
           column(width=8, titlePanel("Recorded earthquakes in Switzerland"))),
  fluidRow(style='height:30px;'),
  fluidRow(
    column(width = 7,
           fluidRow(column(width=12, DTOutput("Table")))),
    column(width = 5,
           fluidRow(column(width=12, leafletOutput("Map"))),
           fluidRow(style="height:20px;"),
           fluidRow(column(width=12, plotlyOutput("Histogram")))),
    column(width=12,
           fluidRow(style='background-color:#9ED2BE;',
                    column(width=12,
                           "Author: Jamel Belgacem")),
           fluidRow(style='background-color:#9ED2BE;',
                    column(width = 12,
                           "This dashboard was developped with Shiny R.")),
           fluidRow(style='background-color:#9ED2BE;',
                    column(width=12,
                           "Data from ", tags$a(href="http://seismo.ethz.ch/en/home/","Swiss Seismological Service"),".")),
           fluidRow(style='background-color:#9ED2BE;',
                    column(width=12,
                           Sys.Date())))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # Read data
  read_data <- reactive({
    browser()
    dotenv::load_dot_env(file=".env")
    AWS_ACCESS_KEY_ID = Sys.getenv("aws_access_key_id")
    AWS_SECRET_ACCESS_KEY = Sys.getenv("aws_secret_access_key")
    AWS_DEFAULT_REGION <- "us-east-1"
    Sys.setenv(
      "AWS_ACCESS_KEY_ID" = AWS_ACCESS_KEY_ID,
      "AWS_SECRET_ACCESS_KEY" = AWS_SECRET_ACCESS_KEY,
      "AWS_DEFAULT_REGION" = AWS_DEFAULT_REGION
    )
    bucket_name <- "earthquakedb"
    s3_file <- "data_etl.csv"
    data <- s3read_using(FUN = read.csv, bucket = bucket_name, object = s3_file) %>%
      mutate('Date UTC'=as.Date(paste0(Year, ' ', UTC_Time), format="%Y %b %d")) %>%
      select('Date UTC', Location, Magnitude, Depth, Latitude, Longitude) %>%
      filter(Magnitude>0)
    return(data)
  })
  
  # Table
  output$Table <- renderDT({
    data <- read_data() %>%
      select('Date UTC', Location, Magnitude)
    datatable(data, rownames=F, options = list(
      columnDefs=list(list(className='dt-center', targets=0:2)),
      pageLength=15))
  })
  
  # Histogram
  output$Histogram <- renderPlotly({
    plot <- read_data() %>%
      plot_ly(x=~Magnitude, type = "histogram")
    plot %>%
      layout(title="Earthquake's magnitude distribution")
  })
  
  # Map plot
  output$Map <- renderLeaflet({
    data <- read_data()
    pal <- colorNumeric(palette= brewer.pal(9,"Reds")[3:9], domain=data$Magnitude)
    leaflet(data) %>% addTiles() %>%
      fitBounds(lng1=~min(Longitude),lat1=~min(Latitude),
                lng2=~max(Longitude),lat2=~max(Latitude)) %>%
      addCircles(radius= ~9^Magnitude+2500, weight=1, color=~pal(Magnitude),
                 fillColor=~pal(Magnitude),fillOpacity = 0.7,
                 popup=~paste("Date:",`Date UTC`,
                              " / Location: ",Location,
                              " / Magnitude: ",Magnitude)) %>%
      addLegend(position="bottomright",pal=pal,values=~Magnitude)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
