## server.r

source("../../functions/static_data_load.R")
staticDataFetcher() 

shinyServer(function(input, output, session) {
  
  metadata.file <- NULL
  studyData <- NULL
  longitudinal.file <- NULL
  current <- NULL
  facetInfo <- NULL
  facet2Info <- NULL
  groupInfo <- NULL
  attributes.file <- NULL
  propUrl <- NULL
  properties <- NULL
  longitudinal1 <- NULL
  longitudinal2 <- NULL
  project.id <- NULL
  isParticipant <- NULL
  getMyY <- reactiveValues()
  getMyGroups <- reactiveValues()
  getMyFacet <- reactiveValues()
  getMyFacet2 <- reactiveValues()
  legendTitle <- NULL
  prtcpntView <- reactiveValues()
  prtcpntView$val <- TRUE
  metadata.classes <- NULL
  contLongitudinal <- FALSE
  datasetName <- NULL
  longitudinalText <- NULL
  facetText <- NULL
  facet2Text <- NULL
  groupsText <- NULL
  yaxisStp2Text <- NULL

  source("../../functions/reactive_data_load.R", local = TRUE) 
 
  #ui stuffs
  output$title <- renderText({
    withProgress(message = 'Loading... May take a minute', value = 0, style = "old", {
      if (is.null(studyData)) {
        reactiveDataFetcher()
      } 
      incProgress(.45)
      timelineInit()
      incProgress(.15)
      groupInit()
      incProgress(.25)
      facetInit()
      facet2Init()
      incProgress(.15)
    })
    c("Data Summaries")
  })

  source("server/plotParams.R", local = TRUE)   
 
    aggKey <- reactive({
      myPrtcpntView <- prtcpntView$val
      
      if (myPrtcpntView == TRUE) {
        aggKey <- c("Participant_Id")
      } else {
        #aggKey <- c("Observation_Id")
        aggKey <- c("Participant_Id", longitudinal1)
      }
      
      return(aggKey)
    })
    
    source("server/individualPlot.R", local = TRUE)   
 
    source("server/plotGrid.R", local = TRUE) 

    source("server/summaryStats.R", local = TRUE) 

    source("server/plotData.R", local = TRUE) 
})
