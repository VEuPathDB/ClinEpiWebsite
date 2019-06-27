validateAndDebounceTimeline <- debounce(reactive({
  test <- input$`timeline-subset`
  test <- input$`timeline-range1`
  test <- input$`timeline-range2`
  print("validated timeline inputs")
  list(mySubset = current$subset,
       myTimeframe1 = current$range1,
       myTimeframe2 = current$range2)
}), 1000)

timelineData <- function(mySubset, myTimeframe1, myTimeframe2, data, longitudinal1, longitudinal2){

  if (!all(c(longitudinal1, longitudinal2) %in% colnames(data))) { return(data) }

  if (!is.null(longitudinal1)) {
    #should never have both subset and timeframes..
    if (!is.null(mySubset)) {
      data <- subsetDataFetcher(keep = mySubset, myData = data, col = longitudinal1)
      message("subsetting data by non-continuous longitudinal variable..")
      if (nrow(data) == 0) {
        message("subset failed, returning")
        return()
      }
    }
    if (!is.null(myTimeframe1)) {
      data <- subsetDataFetcher(min = myTimeframe1[1], max = myTimeframe1[2], myData = data, col = longitudinal1)
      message("subsetting data by first longitudinal variable..")
      if (nrow(data) == 0) {
        message("subset failed, returning")
        return()
      }
    }
    if (!is.null(longitudinal2)) {
      if (!is.null(myTimeframe2)) {
        data <- subsetDataFetcher(min = myTimeframe2[1], max = myTimeframe2[2], myData = data, col = longitudinal2)
        message("subsetting data by second longitudinal variable..")
        if (nrow(data) == 0) {
          message("subset failed, returning")
          return()
        }
      }
    }
  }

  longitudinalText <<- longitudinalText(mySubset, myTimeframe1, myTimeframe2)

  data
}
