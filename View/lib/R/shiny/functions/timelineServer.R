validateAndDebounceTimeline <- debounce(reactive({
  test <- input$`timeline-subset`
  test <- input$`timeline-range1`
  test <- input$`timeline-range2`

  message("\n", Sys.time(), " functions/timelineServer.R: validated timeline inputs:")
  message("current$subset: ", current$subset)
  message("current$range1: ", current$range1)
  message("current$range2: ", current$range2)

  if (!timelineInit$done) {
    timelineInit$val <<- isolate(timelineInit$val) + 1 
  }
  list(mySubset = current$subset,
       myTimeframe1 = current$range1,
       myTimeframe2 = current$range2)
}), 1000)

timelineData <- function(mySubset, myTimeframe1, myTimeframe2, data, longitudinal1, longitudinal2, lon1Data, lon2Data){

  myAggKey <- aggKey()
  if (longitudinal1 %in% names(data) & !(longitudinal1 %in% myAggKey)) {
    myAggKey <- c(myAggKey, longitudinal1)
  }
  if (is.null(longitudinal2)) {
    #TODO figure if we ever need hlonData here, cached it just in case
    if (!is.null(lon1Data)) {
      data <- unique(merge(data, lon1Data, by=myAggKey, all.x = TRUE))
    }
  } else {
    if (!is.null(lon2Data)) {
      myLon2Data <- lon2Data
      #for case where user selection matches second timeline variable
      if (longitudinal2 %in% names(data)) {
        myLon2Data <- unique(myLon2Data[, -longitudinal2, with=FALSE])
      }
      data <- unique(merge(data, myLon2Data, by = myAggKey, all.x = TRUE))
    }
  }

  if (!is.null(longitudinal1)) {
    #should never have both subset and timeframes..

    if (!is.null(mySubset)) {
      data <- subsetDataFetcher(keep = mySubset, myData = data, col = longitudinal1)
      message(Sys.time(), " functions/timelineServer.R: timelineData: subsetting data by non-continuous longitudinal variable..")
      if (nrow(data) == 0) {
        message(Sys.time(), " functions/timelineServer.R: timelineData: subset failed, returning")
        return()
      }
    }
    if (!is.null(myTimeframe1)) {
      data <- subsetDataFetcher(min = myTimeframe1[1], max = myTimeframe1[2], myData = data, col = longitudinal1)
      message(Sys.time(), " functions/timelineServer.R: timelineData: subsetting data by first longitudinal variable..")
      if (nrow(data) == 0) {
        message(Sys.time(), " functions/timelineServer.R: timelineData: subset failed, returning")
        return()
      }
    }
    if (!is.null(longitudinal2)) {
      if (!is.null(myTimeframe2)) {
message("data to subset: ", names(data))  
      data <- subsetDataFetcher(min = myTimeframe2[1], max = myTimeframe2[2], myData = data, col = longitudinal2)
        message(Sys.time(), " functions/timelineServer.R: timelineData: subsetting data by second longitudinal variable..")
        if (nrow(data) == 0) {
          message(Sys.time(), " functions/timelineServer.R: timelineData: subset failed, returning")
          return()
        }
      }
    }
  }

  #longitudinalText <<- longitudinalText(mySubset, myTimeframe1, myTimeframe2)

  data
}
