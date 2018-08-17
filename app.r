library(shiny)
library(shinydashboard)
library(leaflet)
library(data.table)
library(ggplot2)

#ui element
ui <- pageWithSidebar(
  headerPanel("CSV Viewer"),
  #create sidbar
  sidebarPanel(
    #create box for file input
    fileInput('file1', 'Choose CSV File',
              accept=c('text/csv','text/comma-separated-values,text/plain','.csv')),
    tags$hr(),
    checkboxInput('header', 'Header', TRUE),
    #create check boxes for major and minor component functions
    fluidRow(
      column(6,checkboxGroupInput("majorComps","Select Major Components:", c("1"="1","2"="2"))),
      column(6,checkboxGroupInput("minorComps","Select Minor Components:", c("1"="1","2"="2")))
    ),
    #select delimiter (used for file import)
    radioButtons('sep', 'Separator',
                 c(Comma=',', Semicolon=';',Tab='\t'), ','),
    #select Quote (used for file import)
    radioButtons('quote', 'Quote',
                 c(None='','Double Quote'='"','Single Quote'="'"),'"'),
    #create action button used to run anomaly detection algorithm
    actionButton(inputId = "Run", label = "Run"),
    uiOutput("choose_columns")
  ),
  mainPanel(
    #create tabs on main panel
    tabsetPanel(
      tabPanel("Data",tableOutput("contents")),#data tab
      tabPanel("Anomalies", tableOutput('anomolies'))#anomalies tab
    )
  )
)
#server function
server <- function(input, output,session) {
  options(shiny.maxRequestSize=150*1024^2)#allow files up to 150 MB
  dsnames <- c()
  #function to map the selected .csv file to data_set
  data_set <- reactive({
    inFile <- input$file1
    
    if (is.null(inFile)){
      return(NULL)
    }
    data_set<-read.csv(inFile$datapath, header=input$header, 
                       sep=input$sep, quote=input$quote)
  })
  
  #create table of entire data in the data tab
  output$contents <- renderTable({data_set()})
  #update check boxes
  observe({
    dsnames <- names(data_set())
    cb_options <- list()
    cb_options[ dsnames] <- dsnames
    updateCheckboxGroupInput(session, "majorComps",
                             label = "Major Components",
                             choices = cb_options,
                             selected = "")
    updateCheckboxGroupInput(session, "minorComps",
                             label = "Minor Components",
                             choices = cb_options,
                             selected = "")
  })
  output$choose_dataset <- renderUI({
    selectInput("dataset", "Data set", as.list(data_sets))
  })
  #execute algorithm when "Run" is pressed
  observeEvent(input$Run, {
    
    majCompSS <- function(myData, majComps) {
      #myData: dataframe to perform the anomaly detection on
      #majComps: array with the column number of the major components
      #calulates the score for the major components
      MajPCAcolSum <- NA
      MajSumSquare <- 0
      eig <- eigen(cov(myData[,majComps]))#eigenvalues
      #sum(y_i^2/lambda_i)
      for(n in 1:nrow(myData)){
        count <- 1
        for(c in majComps){
          MajSumSquare <- MajSumSquare + (myData[n,c]^2 / eig$values[count])
          count <- count + 1
        }
        MajPCAcolSum[n] <- MajSumSquare
        MajSumSquare <- 0
      }
      return(MajPCAcolSum)
    }
    
    minCompSS <- function(myData, minComps){
      #myData: dataframe to perform the anomaly detection on
      #minComps: array with the column number of the minor components
      #calulates the score for the minor components
      MinPCAcolSum <- NA
      MinSumSquare <- 0
      eig <- eigen(cov(myData[,minComps]))#eigenvalues
      #sum(y_i^2/lambda_i)
      for(n in 1:nrow(myData)){
        count <- 1
        for(c in minComps){
          MinSumSquare <- MinSumSquare + (myData[n,c]^2 / eig$values[count])
          count <- count + 1
        }
        MinPCAcolSum[n] <- MinSumSquare
        MinSumSquare <- 0
      }
      return(MinPCAcolSum)
    }
    
    makePrediction <- function(colSums, threshold, pred) {
      #function to classify transaction
      #colSums: result of either majCompSS or minCompSS
      #threshold: values above threshold classified as fraud
      #list of same length as colSums
      for(i in 1:length(colSums)){
        if(colSums[i] > threshold){
          pred[i] <- 1
        } 
      }
      return(pred)
    }
    
    detectAnomoly <- function(myData, majComp, majThresh, minComp, minThresh){
      #runs anomaly detection algorithm
      #myData: data frame to classify
      #majComp: columns of major components
      #majThresh: threshold for major components
      #minComp: columns of minor components
      #minThresh: threshold for minor components
      maj <- majCompSS(myData, majComp)
      min <- minCompSS(myData, minComp)
      pred <- rep(0, nrow(myData))
      pred <- makePrediction(maj, majThresh, pred)
      pred <- makePrediction(min, minThresh, pred)
      return(pred)
    }
    #run detection algorithm
    anoms <- detectAnomoly(data_set(), input$majorComps, 100, input$minorComps, 40)
    #create data frame of anomalous values
    fraud <- data_set()
    fraud$anomolies <- anoms
    fraud <- fraud[fraud$anomolies == 1,]
    fraud <- subset(fraud, select = -c(anomolies, Class))
    #create table of anomalous values in the Anomalies tab
    output$anomolies <- renderTable({
      if(is.null(fraud)){
        return(NULL)
      }
      fraud
    })
  })
  
  
  output$choose_columns <- renderUI({
    
    if(is.null(input$dataset))
      return()
    colnames <- names(contents)
    checkboxGroupInput("columns", "Choose columns", 
                       choices  = colnames,
                       selected = colnames)
  }) 
}
shinyApp(ui, server)