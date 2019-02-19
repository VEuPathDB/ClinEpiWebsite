## server.r

source("../../functions/static_data_load.R")
staticDataFetcher()

shinyServer(function(input, output, session) {
  
  prevFacet <- NULL
  current <- NULL
  facetInfo <- NULL
  facet2Info <- NULL
  xaxisInfo <- NULL
  attributes.file <- NULL  
  propUrl <- NULL
  properties <- NULL
  getMyX <- reactiveValues()
  getMyFacet <- reactiveValues()
  getMyFacet2 <- reactiveValues()
  prtcpntView <- reactiveValues()
  prtcpntView$val <- TRUE
  longitudinalText <- NULL
  facetText <- NULL
  facet2Text <- NULL
  datasetName <- NULL
  studyData <- NULL
  metadata.file <- NULL
  longitudinal.file <- NULL
  longitudinal1 <- NULL
  longitudinal2 <- NULL
  attributes.file <- NULL
  isParticipant <- NULL

  source("../../functions/reactive_data_load.R", local = TRUE)

  output$title <- renderText({
    withProgress(message = 'Loading... May take a minute', value = 0, style = "old", {
      if (is.null(studyData)) {
	message("reactive data fetcher")
	 reactiveDataFetcher()
      }
      incProgress(.45)
      timelineInit()
      incProgress(.15)
      xaxisInit()
      incProgress(.25)
      facetInit()
      facet2Init() 
      incProgress(.15)
    })
    c("Data Distributions")
  })

  source("server/plotParams.R", local = TRUE)

  aggKey <- reactive({
    myPrtcpntView <- prtcpntView$val
    
    if (myPrtcpntView == TRUE) {
      aggKey <- c("Participant_Id")
    } else {
      #apps currently assume one col will be participant_id, so not using obs_id for now
      #assumes the two are equivalent (no more than one observation for a participant on some date longitudinal1)  
      aggKey <- c("Participant_Id", longitudinal1)
    }
    
    return(aggKey)
  })
    

  source("server/individualPlot.R", local = TRUE)
  #consolidate code between two plots
  source("server/plotGrid.R", local = TRUE)
    
  source("server/summaryStats.R", local = TRUE) 
   
  source("server/plotData.R", local = TRUE)
 
})
