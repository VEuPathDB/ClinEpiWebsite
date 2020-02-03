## server.r

lon1DataList <- list()
lon2DataList <- list()
hlon1DataList <- list()
hlon2DataList <- list()

model.prop <- NULL
dbDrv <- dbDriver("Oracle")
dbCon <- NULL

shinyServer(function(input, output, session) {

  observeEvent(input$timeOut, { 
    message("\n", Sys.time(), "ClinEpi_EventDist/server.R: Session (", session$token, ") timed out")
    showModal(modalDialog(
      title = NULL,
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
  #test for completion of loading
  timelineInit <- reactiveValues()
  timelineInit$val <- 0
  timelineInit$done <- FALSE
  facet1Init <- reactiveValues()
  facet1Init$val <- 0
  facet1Init$done <- FALSE
  xaxisInit <- reactiveValues()
  xaxisInit$val <- 0
  xaxisInit$done <- FALSE

  source("../../functions/reactive_data_load.R", local = TRUE)

  progress <- Progress$new(session, min = 0, max = 1, style = "notification")
  progress$set(message = "Loading data for this new session ...", value = .10)

  output$title <- renderText({
    if (is.null(attributes.file)) {
      message(Sys.time(), " Starting reactive data fetcher for new session ", session$token)
      reactiveDataFetcher()
    }  
    progress$inc(.45)
    #Sys.sleep(5)

    message(Sys.time(), " timelineInit value: ", isolate(timelineInit$val))
    if (timelineInit$val == 2 & !timelineInit$done) {
      progress$inc(.15, "Timeline done...")
      timelineInit$done <<- TRUE
    } else { message("******Distributions-server.R: timeline still unfinished or was done" )}

    message(Sys.time(), " facet1Init value: ", isolate(facet1Init$val))
    if (facet1Init$val == 1 & !facet1Init$done) {
      progress$inc(.15, "Strata done...")
      facet1Init$done <<- TRUE
    } else {message("******Distributions-server.R: facet1 still unfinished or was done" )}

    message(Sys.time(), " xaxisInit value: ", isolate(xaxisInit$val))
    if (xaxisInit$val == 1 & !xaxisInit$done) {
      progress$inc(.15, "X-Axis done ...")
      xaxisInit$done <<- TRUE
    } else {message("******Distributions-server.R: xaxis unfinished or was done" )}

    if (timelineInit$done & facet1Init$done & xaxisInit$done) {
      progress$inc(.5) 
      message("******Distributions-server.R: DONE with three parameters" )
      progress$close()
    } else {message("******Distributions-server.R: STILL NOT FINISHED" )}

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


