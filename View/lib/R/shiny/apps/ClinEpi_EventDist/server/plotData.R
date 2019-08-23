source("../../functions/timelineServer.R", local = TRUE)

source("../../functions/facetServer.R", local = TRUE)

validateAndDebounceAxes <- debounce(reactive({
  #hack to force reactivity. idk maybe its a shiny bug, but reactlog cant find the module inputs for this specific case without referencing directly
  test2 <- input$`group-group`
  myX <- xaxisInfo()$group

  if (is.null(myX)) {
    if (is.null(selectedGroup())) {
      return()
    } else {
      myX <- selectedGroup()
    }
  }
  
  list(myX = myX)
}), 1000)

xQuery <- reactive({
  if (is.null(validateAndDebounceAxes())) {
    return()
  }
  myInputs <- validateAndDebounceAxes()
  myX <- myInputs$myX

  dbCon <<- manageOracleConnection(dbDrv, dbCon, model.prop)
  data <- queryTermData(dbCon, myX, attributes.file, datasetDigest, metadata.file, longitudinal1, longitudinal2, lon2Data, lon1Data, hlongitudinal1, hlongitudinal2, hlon2Data, hlon1Data)
  if (is.null(data)) { return() }

  strings <- getStrings(metadata.file)
  myCols <- c(aggKey(), myX)
  outData <- data[, myCols, with = FALSE]

  if (myX %in% strings$SOURCE_ID) {
    if (any(grepl("|", outData[[myX]], fixed=TRUE))) {
      outData <- separate_rows(outData, myX, sep = "[|]+")
    }
  }
  
  unique(outData)
})

xAxis <- reactive({
  if (is.null(xQuery())) {
    return()
  }
  if (is.null(validateAndDebounceTimeline())) {
    return()
  }
  myInputs <- validateAndDebounceTimeline()
  
  mySubset <- myInputs$mySubset
  myTimeframe1 <- myInputs$myTimeframe1
  myTimeframe2 <- myInputs$myTimeframe2

  data <- xQuery()  
  data <- timelineData(mySubset, myTimeframe1, myTimeframe2, data, longitudinal1, longitudinal2)
 
  unique(data)
})


plotData <- reactive({
      if (is.null(xAxis())) {
        return()
      }
  
      #first thing is to save properties 
      text <- paste0("input\tselected\n",
                     longitudinalText,
                     facetText,
                     facet2Text,
                     "xaxisInfo()$group\t", validateAndDebounceAxes()$myX, "\n",
                     "input$facetType\t", input$facetType, "\n",
                     "input$facet2Type\t", input$facet2Type, "\n",
                     "input$xaxis\t", input$xaxis
                     #"input$individualPlot_stp1\t", input$individualPlot_stp1, "\n",
                     #"input$individualPlot_stp2\t", input$individualPlot_stp2 
                    )

      PUT(propUrl, body = "")
      PUT(propUrl, body = text)

      xData <- xAxis()
      facetData <- facet1()
      facet2Data <- facet2()

      if (!is.null(facetData)) {
        data <- merge(xData, facetData, by = aggKey())
      } else {
        data <- xData
      }
      if (!is.null(facet2Data)) {
        data <- merge(data, facet2Data, by = aggKey())
      } else {
      }
      
      unique(data)
    })
