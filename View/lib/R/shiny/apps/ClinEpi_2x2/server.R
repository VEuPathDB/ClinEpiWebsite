# server.r

source("../../functions/static_data_load.R")
staticDataFetcher()

shinyServer(function(input, output, session) {
 
  attributes.file <- NULL
  metadata.file <- NULL
  studyData <- NULL
  longitudinal.file <- NULL
  current <- NULL
  attrInfo <- NULL
  outInfo <- NULL
  facetInfo <- NULL
  facet2Info <- NULL 
  attributes.file <- NULL
  propUrl <- NULL
  properties <- NULL
  longitudinal1 <- NULL
  longitudinal2 <- NULL
  project.id <- NULL
  isParticipant <- NULL
  model.prop <- NULL
  getMyAttr <- reactiveValues()
  getMyOut <- reactiveValues()
  prtcpntView <- reactiveValues()
  prtcpntView$val <- TRUE
  getMyFacet <- reactiveValues()
  getMyFacet2 <- reactiveValues()
  metadata.classes <- NULL
  datasetName <- NULL
  attrText <- NULL
  outText <- NULL
  facetText <- NULL
  facet2Text <- NULL
  longitudinalText <- NULL

  source("../../functions/reactive_data_load.R", local = TRUE)
 
  output$title <- renderText({
    withProgress(message = 'Loading... May take a minute', value = 0, style = "old", {
      if (is.null(studyData)) {
        reactiveDataFetcher()
      }
      incProgress(.45)
      timelineInit()
      incProgress(.15)
      attrInit()
      outInit()
      incProgress(.25)
      facetInit()
      facet2Init()
      incProgress(.15)
    })
    c("Contingency Tables")
  })
   
  source("server/plotParams.R", local = TRUE)
 
    aggKey <- reactive({
      myPrtcpntView <- prtcpntView$val
      
      if (prtcpntView$val == TRUE) {
        aggKey <- c("Participant_Id")
      } else {
        #aggKey <- c("Observation_Id")
        aggKey <- c("Participant_Id", longitudinal1)
      }
      
      return(aggKey)
    })
    
    source("server/individualPlot.R", local = TRUE)   
 
    source("server/plotGrid.R", local = TRUE) 
    
    source("server/contTable.R", local = TRUE)
  
    source("server/summaryStats.R", local = TRUE)  
 
    source("server/plotData.R", local = TRUE)

})
