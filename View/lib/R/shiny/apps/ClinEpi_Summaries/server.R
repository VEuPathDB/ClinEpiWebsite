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
    message("Session (", session$token, ") timed out at: ", Sys.time())
    showModal(modalDialog(
      title = NULL,
      paste("Session timeout due to", input$timeOut, "inactivity -", Sys.time()),
      footer = NULL
    ))
    session$close()
  })
  
  metadata.file <- NULL
  longitudinal.file <- NULL
  current <- NULL
  facetInfo <- reactive({ list("group" = NULL, "group_stp1" = NULL, "group_stp2" = NULL, "group_stp3" = NULL, "group_stp4" = NULL) })
  facet2Info <- reactive({ list("group" = NULL, "group_stp1" = NULL, "group_stp2" = NULL, "group_stp3" = NULL, "group_stp4" = NULL) })
  groupInfo <- reactive({ list("group" = NULL, "group_stp1" = NULL, "group_stp2" = NULL, "group_stp3" = NULL, "group_stp4" = NULL) })
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
  legendTitle <- NULL
  getMyY <- reactiveValues()
  prtcpntView <- reactiveValues()
  prtcpntView$val <- TRUE
  metadata.classes <- NULL
  contLongitudinal <- FALSE
  datasetName <- NULL
  datasetDigest <- NULL
  longitudinalText <- NULL
  facetText <- NULL
  facet2Text <- NULL
  groupsText <- NULL
  yaxisStp2Text <- NULL
  numXBins <- reactiveValues()
  numXBins$val <- NULL
  #test for completion of loading
  timelineInit <- reactiveValues()
  timelineInit$val <- 0
  timelineInit$done <- FALSE
  facet1Init <- reactiveValues()
  facet1Init$val <- 0
  facet1Init$done <- FALSE

  source("../../functions/reactive_data_load.R", local = TRUE) 

  progress <- Progress$new(session, min = 0, max = 1, style = "notification") 
  progress$set(message = "Loading data for this new session ...", value = 0)

  output$title <- renderText({
    if (is.null(attributes.file)) {
      message(Sys.time(), " Starting reactive data fetcher for new session ", session$token)
      reactiveDataFetcher()
    } 
    progress$inc(.50)

    message("timelineInit value: ", isolate(timelineInit$val))
    if (timelineInit$val == 2 & !timelineInit$done) {
      progress$inc(.25, "Timeline done ... ")
      timelineInit$done <<- TRUE
    }

    message("facet1Init value: ", isolate(facet1Init$val))
    if (facet1Init$val == 1 & !facet1Init$done) {
      progress$inc(.15, "Strata done ...")
      facet1Init$done <<- TRUE
    }

    if (timelineInit$done & facet1Init$done) {
      progress$close()
    }

    c("Data Summaries")
  })

  source("server/plotParams.R", local = TRUE)   
 
    aggKey <- reactive({
      myPrtcpntView <- prtcpntView$val
      
      if (myPrtcpntView == TRUE) {
        aggKey <- c("PARTICIPANT_ID")
      } else {
        #aggKey <- c("Observation_Id")
        aggKey <- c("PARTICIPANT_ID", longitudinal1)
      }
      
      return(aggKey)
    })
    
    source("server/individualPlot.R", local = TRUE)   
 
    source("server/plotGrid.R", local = TRUE) 

    source("server/summaryStats.R", local = TRUE) 

    source("server/plotData.R", local = TRUE) 
})
