# server.r

#source("../../functions/static_data_load.R")
#staticDataFetcher()

lon1DataList <- list()
lon2DataList <- list()
hlon1DataList <- list()
hlon2DataList <- list()

shinyServer(function(input, output, session) {

  observeEvent(input$timeOut, { 
    print(paste0("Session (", session$token, ") timed out at: ", Sys.time()))
    showModal(modalDialog(
      title = "Timeout",
      paste("Session timeout due to", input$timeOut, "inactivity -", Sys.time()),
      footer = NULL
    ))
    session$close()
  })
 
  attributes.file <- NULL
  metadata.file <- NULL
  longitudinal.file <- NULL
  current <- NULL
  attrInfo <- reactive({ list("group" = NULL, "group_stp1" = NULL, "group_stp2" = NULL, "group_stp3" = NULL, "group_stp4" = NULL) })
  outInfo <- reactive({ list("group" = NULL, "group_stp1" = NULL, "group_stp2" = NULL, "group_stp3" = NULL, "group_stp4" = NULL) })
  facetInfo <- reactive({ list("group" = NULL, "group_stp1" = NULL, "group_stp2" = NULL, "group_stp3" = NULL, "group_stp4" = NULL) })
  facet2Info <- reactive({ list("group" = NULL, "group_stp1" = NULL, "group_stp2" = NULL, "group_stp3" = NULL, "group_stp4" = NULL) })
  attributes.file <- NULL
  propUrl <- NULL
  properties <- NULL
  longitudinal1 <- NULL
  longitudinal2 <- NULL
  lon2Data <- NULL
  lon1Data <- NULL
  hlongitudinal1 <- NULL
  hlongitudinal2 <- NULL
  hlon2Data <- NULL
  hlon1Data <- NULL
  project.id <- NULL
  isParticipant <- NULL
  model.prop <- NULL
  prtcpntView <- reactiveValues()
  prtcpntView$val <- TRUE
  metadata.classes <- NULL
  datasetName <- NULL
  datasetDigest <- NULL  
  attrText <- NULL
  outText <- NULL
  facetText <- NULL
  facet2Text <- NULL
  longitudinalText <- NULL

  source("../../functions/reactive_data_load.R", local = TRUE)
 
  output$title <- renderText({
    withProgress(message = 'Loading... May take a minute', value = 0, style = "old", {
      if (is.null(attributes.file)) {
        reactiveDataFetcher()
      }
      incProgress(.45)
      #timelineInit()
      incProgress(.15)
      #attrInit()
      #outInit()
      incProgress(.25)
      #facetInit()
      #facet2Init()
      incProgress(.15)
    })
    c("Contingency Tables")
  })
   
  source("server/plotParams.R", local = TRUE)
 
    aggKey <- reactive({
      myPrtcpntView <- prtcpntView$val
      
      if (prtcpntView$val == TRUE) {
        aggKey <- c("PARTICIPANT_ID")
      } else {
        #aggKey <- c("Observation_Id")
        aggKey <- c("PARTICIPANT_ID", longitudinal1)
      }
      
      return(aggKey)
    })
    
    source("server/individualPlot.R", local = TRUE)   
 
    source("server/plotGrid.R", local = TRUE) 
    
    source("server/contTable.R", local = TRUE)
  
    source("server/summaryStats.R", local = TRUE)  
 
    source("server/plotData.R", local = TRUE)

})
