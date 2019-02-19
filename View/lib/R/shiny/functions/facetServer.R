facet1 <- reactive({
  if (is.null(input$facetType)) {
    return()
  }
  facetType <- input$facetType
  if (facetType == "none") {
    myFacet <- "none"
  } else {
    myFacet <- getMyFacet$val
  }
  facet_stp1 <- facetInfo$group_stp1
  facet_stp3 <- facetInfo$group_stp3
  facet_stp4 <- facetInfo$group_stp4
  facet_stp2 <- facetInfo$group_stp2

  if (is.null(facetType)) {
    return()
  } else {
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
    } else if (facetType == "direct") {
      if (is.null(myFacet)) {
        return()
      } else if (length(myFacet) == 0) {
	return()
      }
      if (myFacet == "none") {
        return()
      }
    }
  }
	message("have all facet inputs")
  nums <- getNums(metadata.file)
  dates <- getDates(metadata.file)
  data <- timelineData()
  if (facetType == "direct") {
        outData <- data
        if (myFacet %in% nums$source_id | myFacet %in% dates$source_id) {
          outData[[myFacet]] <- rcut_number(outData[[myFacet]],4)
        }
        displayLabel <- metadata.file$property[metadata.file$source_id == myFacet]
        outData[[myFacet]] <- paste0(displayLabel, ": ", outData[[myFacet]])
        myCols <- c(aggKey(), myFacet)
        outData <- unique(outData[, myCols, with=FALSE])
      } else if (facetType == "makeGroups") {
        numeric <- c("lessThan", "greaterThan", "equals")
        anthro <- c("percentDays", "delta", "direct")
        if (facetType != "none") {
          if (is.null(facet_stp1)) {
            return()
          } else {
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
          }
          aggKey <- aggKey()
          outData <- makeGroups(data, metadata.file, myFacet, facet_stp1, facet_stp2, facet_stp3, facet_stp4, aggKey)
	  if (is.null(outData)) {
	    return()
	  }
          observations <- metadata.file$source_id[metadata.file$category == "Observation"]
          observations <- observations[observations %in% colnames(studyData)]
          label <- makeGroupLabel(myFacet, metadata.file, facet_stp1, facet_stp2, facet_stp3, facet_stp4, event.list = observations)
          #add makeGroups data to df and return
          colnames(outData) <- c(aggKey, "FACET")
          outData <- transform(outData, "FACET" = ifelse(as.numeric(FACET) == 0, label[2], label[1]))
          outData <- unique(outData)
        }
      } else {
        return()
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
    myFacet2 <- "none"
  } else {
    myFacet2 <- getMyFacet2$val
  }
  facet2_stp1 <- facet2Info$group_stp1
  facet2_stp3 <- facet2Info$group_stp3
  facet2_stp4 <- facet2Info$group_stp4
  facet2_stp2 <- facet2Info$group_stp2

  if (is.null(facet2Type)) {
    return()
  } else {
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
    } else if (facet2Type == "direct") {
      if (is.null(myFacet2)) {
        return()
      } else if (length(myFacet2) == 0) {
        return()
      }
      if (myFacet2 == "none") {
        return()
      }
    }
  }

  data <- timelineData()
  if (facet2Type == "direct") {
        outData <- data
        if (myFacet2 %in% nums$source_id | myFacet2 %in% dates$source_id) {
          outData[[myFacet2]] <- rcut_number(outData[[myFacet2]],4)
        }
        displayLabel <- metadata.file$property[metadata.file$source_id == myFacet2]
        outData[[myFacet2]] <- paste0(displayLabel, ": ", outData[[myFacet2]])
        myCols <- c(aggKey(), myFacet2)
        outData <- unique(outData[, myCols, with=FALSE])
      } else if (facet2Type == "makeGroups") {
        numeric <- c("lessThan", "greaterThan", "equals")
        anthro <- c("percentDays", "delta", "direct")
        if (facet2Type != "none") {
          if (is.null(facet2_stp1)) {
            return()
          } else {
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
          }
aggKey <- aggKey()
          outData <- makeGroups(data, metadata.file, myFacet2, facet2_stp1, facet2_stp2, facet2_stp3, facet2_stp4, aggKey)
	  if (is.null(outData)) { return() }
          observations <- metadata.file$source_id[metadata.file$category == "Observation"]
     #TODO consider removing studyData here.. better way
          observations <- observations[observations %in% colnames(studyData)]
          label <- makeGroupLabel(myFacet2, metadata.file, facet2_stp1, facet2_stp2, facet2_stp3, facet2_stp4, event.list = observations)
          #add makeGroups data to df and return
          colnames(outData) <- c(aggKey, "FACET2")
          outData <- transform(outData, "FACET2" = ifelse(as.numeric(FACET2) == 0, label[2], label[1]))
          outData <- unique(outData)
        }
      } else {
        return()
      }

  facet2Text <<- groupText("facet2Info", getMyFacet2$val, facet2_stp1, facet2_stp2, facet2_stp3, facet2_stp4)
  outData
})

