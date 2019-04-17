facet1 <- reactive({
  if (is.null(input$facetType)) {
    return()
  }
  facetType <- input$facetType
  if (facetType == "none") {
    return()
  } else {
    myFacet <- facetInfo()$group
    if (is.null(myFacet)) { return() }
    if (length(myFacet) == 0) { return() }
    if (is.na(myFacet)) { return() }
    if (myFacet == "none") { return() }
  }
  facet_stp1 <- facetInfo()$group_stp1
  facet_stp3 <- facetInfo()$group_stp3
  facet_stp4 <- facetInfo()$group_stp4
  facet_stp2 <- facetInfo()$group_stp2

    if (facetType == "makeGroups") {
      if (is.null(facet_stp1)) {
        return()
      } else {
        if (facet_stp1 == 'any' | facet_stp1 == 'all') {
          if (is.null(facet_stp2)) {
            return()
          } else {
            if (facet_stp2 %in% c("lessThan", "greaterThan", "equals")) {
              if (is.null(facet_stp3)) {
                return()
              }
            }
          }
        }
      }
    }
  mySubset <- current$subset
  myTimeframe1 <- current$range1
  myTimeframe2 <- current$range2
  nums <- getNums(metadata.file)
  dates <- getDates(metadata.file)
 
  data <- dataFromServiceQuery(myFacet, attributes.file, datasetDigest, metadata.file, longitudinal1, longitudinal2, lon2Data, lon1Data, hlongitudinal1, hlongitudinal2, hlon2Data, hlon1Data)
  if (is.null(data)) { return() }
  data <- timelineData(mySubset, myTimeframe1, myTimeframe2, data, longitudinal1, longitudinal2)

  if (facetType == "direct") {
        outData <- data
        if (myFacet %in% nums$SOURCE_ID | myFacet %in% dates$SOURCE_ID) {
          outData[[myFacet]] <- rcut_number(outData[[myFacet]],4)
        }
        displayLabel <- metadata.file$PROPERTY[metadata.file$SOURCE_ID == myFacet]
        outData[[myFacet]] <- paste0(displayLabel, ": ", outData[[myFacet]])
        myCols <- c(aggKey(), myFacet)
        outData <- unique(outData[, myCols, with=FALSE])
      } else if (facetType == "makeGroups") {
        numeric <- c("lessThan", "greaterThan", "equals")
        anthro <- c("percentDays", "delta", "direct")
            if (facet_stp1 %in% numeric) {
              if (is.null(facet_stp2)) {
                return()
              }
            }
            if (facet_stp1 %in% anthro) {
              if (facet_stp1 == "percentDays") {
                if (is.null(facet_stp4)) {
                  return()
                }
              } else {
                if (is.null(facet_stp3)) {
                  return()
                }
              }
            }
          aggKey <- aggKey()
          outData <- makeGroups(data, metadata.file, myFacet, facet_stp1, facet_stp2, facet_stp3, facet_stp4, aggKey)
	  if (is.null(outData)) {
	    return()
	  }
          observations <- metadata.file$SOURCE_ID[metadata.file$CATEGORY == "Observation"]
          label <- makeGroupLabel(myFacet, metadata.file, facet_stp1, facet_stp2, facet_stp3, facet_stp4, event.list = observations)
          #add makeGroups data to df and return
          colnames(outData) <- c(aggKey, "FACET")
          outData <- transform(outData, "FACET" = ifelse(as.numeric(FACET) == 0, label[2], label[1]))
          outData <- unique(outData)
      }

  facetText <<- groupText("facetInfo", myFacet, facet_stp1, facet_stp2, facet_stp3, facet_stp4)
  outData
})



facet2 <- reactive({
  if (is.null(input$facet2Type)) {
    return()
  }

  facet2Type <- input$facet2Type
  
  if (input$facet2Type == "none") {
    return()
  } else {
    myFacet2 <- facet2Info()$group
    if (is.null(myFacet2)) { return() }
    if (length(myFacet2) == 0) { return() }
    if (is.na(myFacet2)) { return() }
    if (myFacet2 == "none") { return() }
  }
  facet2_stp1 <- facet2Info()$group_stp1
  facet2_stp3 <- facet2Info()$group_stp3
  facet2_stp2 <- facet2Info()$group_stp2
  facet2_stp4 <- facet2Info()$group_stp4

    if (facet2Type == "makeGroups") {
      if (is.null(facet2_stp1)) {
        return()
      } else {
        if (facet2_stp1 == 'any' | facet2_stp1 == 'all') {
          if (is.null(facet2_stp2)) {
            return()
          } else {
            if (facet2_stp2 %in% c("lessThan", "greaterThan", "equals")) {
              if (is.null(facet2_stp3)) {
                return()
              }
            }
          }
        }
      }
    }
  mySubset <- current$subset
  myTimeframe1 <- current$range1
  myTimeframe2 <- current$range2

  nums <- getNums(metadata.file)
  dates <- getDates(metadata.file)

  data <- dataFromServiceQuery(myFacet2, attributes.file, datasetDigest, metadata.file, longitudinal1, longitudinal2, lon2Data, lon1Data, hlongitudinal1, hlongitudinal2, hlon2Data, hlon1Data)
  if (is.null(data)) { return() }
  data <- timelineData(mySubset, myTimeframe1, myTimeframe2, data, longitudinal1, longitudinal2)

  if (facet2Type == "direct") {
        outData <- data
        if (myFacet2 %in% nums$SOURCE_ID | myFacet2 %in% dates$SOURCE_ID) {
          outData[[myFacet2]] <- rcut_number(outData[[myFacet2]],4)
        }
        displayLabel <- metadata.file$PROPERTY[metadata.file$SOURCE_ID == myFacet2]
        outData[[myFacet2]] <- paste0(displayLabel, ": ", outData[[myFacet2]])
        myCols <- c(aggKey(), myFacet2)
        outData <- unique(outData[, myCols, with=FALSE])
      } else if (facet2Type == "makeGroups") {
        numeric <- c("lessThan", "greaterThan", "equals")
        anthro <- c("percentDays", "delta", "direct")
            if (facet2_stp1 %in% numeric) {
              if (is.null(facet2_stp2)) {
                return()
              }
            }
            if (facet2_stp1 %in% anthro) {
              if (facet2_stp1 == "percentDays") {
                if (is.null(facet2_stp4)) {
                  return()
                }
              } else {
                if (is.null(facet2_stp3)) {
                  return()
                }
              }
            }
aggKey <- aggKey()
          outData <- makeGroups(data, metadata.file, myFacet2, facet2_stp1, facet2_stp2, facet2_stp3, facet2_stp4, aggKey)
	  if (is.null(outData)) { return() }
          observations <- metadata.file$SOURCE_ID[metadata.file$CATEGORY == "Observation"]
          label <- makeGroupLabel(myFacet2, metadata.file, facet2_stp1, facet2_stp2, facet2_stp3, facet2_stp4, event.list = observations)
          #add makeGroups data to df and return
          colnames(outData) <- c(aggKey, "FACET2")
          outData <- transform(outData, "FACET2" = ifelse(as.numeric(FACET2) == 0, label[2], label[1]))
          outData <- unique(outData)
      }

  facet2Text <<- groupText("facet2Info", myFacet2, facet2_stp1, facet2_stp2, facet2_stp3, facet2_stp4)
  outData
})

