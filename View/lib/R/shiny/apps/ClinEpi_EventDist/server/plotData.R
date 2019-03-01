source("../../functions/timelineServer.R", local = TRUE)

source("../../functions/facetServer.R", local = TRUE)



xAxis <- reactive({
  if (is.null(input$xaxis)) {
    return()
  }

  xType <- input$xaxis
  myX <- input$xaxis  
  if (myX == "direct" | myX == "makeGroups") {
    if (is.null(getMyX$val)) {
      return()
    } else {
      myX <- getMyX$val
    }
  }
 
  data <- timelineData()
  strings <- getStrings(metadata.file)
  myCols <- c(aggKey(), myX)
  outData <- data[, myCols, with = FALSE]

  if (myX %in% strings$source_id) {
    if (any(grepl("|", outData[[myX]], fixed=TRUE))) {
      outData <- separate_rows(outData, myX, sep = "[|]+")
    }
  }
  
  #TODO check this still works, even though were limiting the number of cols before splitting the pipes into their own rows
  unique(outData)
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
                     "xaxisInfo$group\t", getMyX$val, "\n",
                     "input$facetType\t", input$facetType, "\n",
                     "input$facet2Type\t", input$facet2Type, "\n",
                     "input$xaxis\t", input$xaxis
                     #"input$individualPlot_stp1\t", input$individualPlot_stp1, "\n",
                     #"input$individualPlot_stp2\t", input$individualPlot_stp2 
                    )

      PUT(propUrl, body = "")
      PUT(propUrl, body = text)

      xData <- xAxis()
      if (is.null(xData)) {
	return()
      }
      facetData <- facet1()
      facet2Data <- facet2()

      if (!is.null(facetData)) {
        data <- merge(xData, facetData, by = aggKey())
      } else {
        data <- xData
      }
      if (!is.null(facet2Data)) {
        data <- merge(data, facet2Data, by = aggKey())
      }
      #print(nrow(data))
      #data <- unique(data)

      #consider if xaxis and facet are the same?? TODO
      #  if (facetType == "makeGroups") {
      #    facetCol = "FACET"
      #  } else if (myX == myFacet | myFacet == "none" | myFacet == "") {
      #    facetCol = c()
      #  }  else {
      #    facetCol = myFacet
      #  }
      #  if (facet2Type == "makeGroups") {
      #    facet2Col = "FACET2"
      #  } else if (myX == myFacet2 | myFacet2 == "none" | myFacet2 == "") {
      #    facet2Col = c()
      #  } else {
      #    facet2Col = myFacet2
      #  }
      data

    })
