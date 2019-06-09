## server.r

#source("../../functions/static_data_load.R")
#staticDataFetcher()

lon1DataList <- list()
lon2DataList <- list()
hlon1DataList <- list()
hlon2DataList <- list()

dbDrv <- dbDriver("Oracle")
dbCon <- NULL

shinyServer(function(input, output, session) {

  observeEvent(input$timeOut, { 
    message("Session (", session$token, ") timed out at: ", Sys.time())
    showModal(modalDialog(
      title = "Timeout",
      paste("Session timeout due to", input$timeOut, "inactivity -", Sys.time()),
      footer = NULL
    ))
    session$close()
  })
  
  prevFacet <- NULL
  current <- NULL
  facetInfo <- reactive({ list("group" = NULL, "group_stp1" = NULL, "group_stp2" = NULL, "group_stp3" = NULL, "group_stp4" = NULL) })
  facet2Info <- reactive({ list("group" = NULL, "group_stp1" = NULL, "group_stp2" = NULL, "group_stp3" = NULL, "group_stp4" = NULL) })
  xaxisInfo <- reactive({ list("group" = NULL, "group_stp1" = NULL, "group_stp2" = NULL, "group_stp3" = NULL, "group_stp4" = NULL) })
  attributes.file <- NULL  
  propUrl <- NULL
  properties <- NULL
  prtcpntView <- reactiveValues()
  prtcpntView$val <- TRUE
  longitudinalText <- NULL
  facetText <- NULL
  facet2Text <- NULL
  datasetName <- NULL
  datasetDigest <- NULL
  metadata.file <- NULL
  longitudinal.file <- NULL
  longitudinal1 <- NULL
  longitudinal2 <- NULL
  lon1Data <- NULL
  lon2Data <- NULL
  hlongitudinal1 <- NULL
  hlongitudinal2 <- NULL
  hlon1Data <- NULL
  hlon2Data <- NULL
  attributes.file <- NULL
  isParticipant <- NULL

  source("../../functions/reactive_data_load.R", local = TRUE)

  output$title <- renderText({
	#TODO move withProgess to reactive data loader now that modules are initialized differently
    withProgress(message = 'Loading... May take a minute', value = 0, style = "old", {
      if (is.null(attributes.file)) {
	message("reactive data fetcher")
	 reactiveDataFetcher()
      }
      incProgress(.45)
      #timelineInit()
      incProgress(.15)
      #xaxisInit()
      incProgress(.25)
      #facetInit()
      #facet2Init() 
      incProgress(.15)
    })
    c("Data Distributions")
  })

  source("server/plotParams.R", local = TRUE)

  aggKey <- reactive({
    myPrtcpntView <- prtcpntView$val
    
    if (myPrtcpntView == TRUE) {
      aggKey <- c("PARTICIPANT_ID")
    } else {
      #apps currently assume one col will be participant_id, so not using obs_id for now
      #assumes the two are equivalent (no more than one observation for a participant on some date longitudinal1)  
      aggKey <- c("PARTICIPANT_ID", longitudinal1)
    }
    
    return(aggKey)
  })
    

  source("server/individualPlot.R", local = TRUE)
  #TODO consolidate code between two plots
  source("server/plotGrid.R", local = TRUE)
    
  source("server/summaryStats.R", local = TRUE) 
   
  #all roads lead to rome
  source("server/plotData.R", local = TRUE)
 
})
