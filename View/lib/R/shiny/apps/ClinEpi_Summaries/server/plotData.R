source("../../functions/timelineServer.R", local = TRUE)

source("../../functions/facetServer.R", local = TRUE)

validateAndDebounceGroup <- debounce(reactive({
  if (is.null(input$groupsType)) {
    return()
  }
  
  groupsType <- input$groupsType
  test2 <- input$`group-group`
  test2 <- input$`group-group_stp1`
  test2 <- input$`group-group_stp2`
  test2 <- input$`group-group_stp3`
  test2 <- input$`group-group_stp4`
  myGroup <- groupInfo()$group
  group_stp1 <- groupInfo()$group_stp1
  group_stp3 <- groupInfo()$group_stp3
  group_stp4 <- groupInfo()$group_stp4
  group_stp2 <- groupInfo()$group_stp2
  
  if (groupsType == "makeGroups") {
    if (is.null(group_stp1)) {
      return()
    } else {
      if (group_stp1 == 'any' | group_stp1 == 'all') {
        if (is.null(group_stp2)) {
          return()
        } else {
          if (group_stp2 %in% c("lessThan", "greaterThan", "equals")) {
            if (is.null(group_stp3)) {
              return()
            }
          }
        }
      }
    }
  } else if (groupsType == "direct") {
    if (is.null(myGroup)) {
      if (is.null(properties)) {
        myGroup = selectedGroup()
      } else {
        myGroup = properties$selected[properties$input == "groupInfo$group"]
      }
    }
  } else {
    myGroup <- "none"
  }

  message(Sys.time(), " validated group inputs:")
  message("input$groupsType: ", groupsType)
  message("myGroup: ", myGroup)
  message("group_stp1: ", group_stp1)      
  message("group_stp2: ", group_stp2)
  message("group_stp3: ", group_stp3)
  message("group_stp4: ", group_stp4)  

  list(groupsType = groupsType, 
       myGroups = myGroup, 
       groups_stp1 = group_stp1, 
       groups_stp2 = group_stp2,
       groups_stp3 = group_stp3,
       groups_stp4 = group_stp4)
}), 1000)

groupQuery <- reactive({
  if (is.null(validateAndDebounceGroup())) {
    return()
  }
  myInputs <- validateAndDebounceGroup()
  myGroups <- myInputs$myGroups

  message(Sys.time(), " Initiating query for groups data")
  dbCon <<- manageOracleConnection(dbDrv, dbCon, model.prop)
  data <- queryTermData(dbCon, myGroups, attributes.file, datasetDigest, metadata.file, longitudinal1, longitudinal2, lon2Data, lon1Data, hlongitudinal1, hlongitudinal2, hlon2Data, hlon1Data)


  if (is.null(data)) { return() }

  data
})

group <- reactive({
  if (is.null(validateAndDebounceGroup()) | is.null(validateAndDebounceTimeline())) {
    return()
  }
  myInputs <- c(validateAndDebounceGroup(), validateAndDebounceTimeline())
  
  groupsType <- myInputs$groupsType
  myGroups <- myInputs$myGroups
  groups_stp1 <- myInputs$groups_stp1
  groups_stp3 <- myInputs$groups_stp3
  groups_stp2 <- myInputs$groups_stp2
  groups_stp4 <- myInputs$groups_stp4
  mySubset <- myInputs$mySubset
  myTimeframe1 <- myInputs$myTimeframe1
  myTimeframe2 <- myInputs$myTimeframe2

  if (contLongitudinal) {
    varName <- "variable for 'Stratify Line'"
  } else {
    varName <- "X-axis variable"
  }

  data <- groupQuery()
  if (is.null(data)) { return() }
  data <- timelineData(mySubset, myTimeframe1, myTimeframe2, data, longitudinal1, longitudinal2, lon1Data, lon2Data)
message("data after timeline filter: ", names(data))
  if (all(is.na(data[, myGroups, with=FALSE]))) {
    showNotification(paste0("The ", varName , " has no data for the timepoint(s) selected, please select another."), duration = NULL, type = "warning")
    return()
  }

  nums <- getNums(metadata.file)
  dates <- getDates(metadata.file)
  aggKey <- aggKey()
  message("aggKey: ", aggKey)
  message("group query data: ", colnames(data))
  if (groupsType == "direct") {
    myCols <- c(aggKey, myGroups)
    outData <- data[, myCols, with=FALSE]
    outData <- unique(outData)
    colnames(outData) <- c(aggKey, "GROUPS")

    if (myGroups %in% nums$SOURCE_ID | myGroups %in% dates$source_id) {
      if (length(levels(as.factor(outData$GROUPS))) >= 4) {
        outData$GROUPS <- rcut_number(outData$GROUPS)
      } else {
        outData$GROUPS <- as.factor(outData$GROUPS)
      }
    }
  } else if (groupsType == "makeGroups") {
    outData <- makeGroups(data, metadata.file, myGroups, groups_stp1, groups_stp2, groups_stp3, groups_stp4, aggKey)
	  if (is.null(outData)) { return() }
    observations <- metadata.file$SOURCE_ID[metadata.file$CATEGORY == "Observation"]
    label <- makeGroupLabel(myGroups, metadata.file, groups_stp1, groups_stp2, groups_stp3, groups_stp4, event.list = observations)
    #add makeGroups data to df and return
    outData <- transform(outData, "GROUPS" = ifelse(as.numeric(GROUPS) == 0, label[2], label[1]))
  } else {
    myCols <- c(aggKey)
    outData <- data[, myCols, with=FALSE]
	  outData$GROUPS <- "All"
	}
  outData <- unique(outData)



  #groupsText <<- groupText("groupInfo", myGroups, groups_stp1, groups_stp2, groups_stp3, groups_stp4)
  unique(outData)
})

validateAndDebounceAxes <- debounce(reactive({
  myY <- getMyY$val
  yaxis_stp1 <- input$yaxis_stp1
  yaxis_stp2 <- input$yaxis_stp2
  if (prtcpntView$val != TRUE) {
    yaxis_stp2 <- input$yaxis_stp1
    yaxis_stp1 <- "any"
  }
  yaxis_stp3 <- input$yaxis_stp3
  yaxis_stp1 <- input$yaxis_stp1
  
  longitudinal <- longitudinal1
  if (!is.null(input$xaxisVar)) {
    if (input$xaxisVar == "ageVar") {
      longitudinal <- longitudinal2
    }
  }
  
  strings <- getStrings(metadata.file)
  if (is.null(myY)) {
    return()
  } else {
    if (myY %in% strings$SOURCE_ID) {
      if (is.null(yaxis_stp2)) {
        return()
      }
    }
  }
  if (is.null(yaxis_stp3)) {
    return()
  }
  
  message(Sys.time(), " validated axes inputs:")
  message("myY: ", myY)
  message("yaxis_stp1: ", yaxis_stp1)
  message("yaxis_stp2: ", yaxis_stp2)      
  message("yaxis_stp3: ", yaxis_stp3)
  message("myX: ", input$xaxisVar)
  message("xaxis_bins: ", input$xaxis_stp2)

  list(myY = myY, 
       yaxis_stp1 = yaxis_stp1, 
       yaxis_stp2 = yaxis_stp2,
       yaxis_stp3 = yaxis_stp3,
       longitudinal = longitudinal,
       xaxisVar = input$xaxisVar,
       xaxis_bins = input$xaxis_stp2)
}), 1000)

axesQuery <- reactive({
  if (is.null(validateAndDebounceAxes())) {
    return()
  }
  myInputs <- validateAndDebounceAxes()
  myY <- myInputs$myY

  message(Sys.time(), " Initiating query for axes data")
  dbCon <<- manageOracleConnection(dbDrv, dbCon, model.prop)
  data <- queryTermData(dbCon, myY, attributes.file, datasetDigest, metadata.file, longitudinal1, longitudinal2, lon2Data, lon1Data, hlongitudinal1, hlongitudinal2, hlon2Data, hlon1Data)
  if (is.null(data)) { 
    return() 
  }

  data
})

axes <- reactive({
  if (is.null(validateAndDebounceAxes()) | is.null(validateAndDebounceTimeline())) {
    return()
  }
  myInputs <- c(validateAndDebounceAxes(), validateAndDebounceTimeline())
  myY <- myInputs$myY
  yaxis_stp1 <- myInputs$yaxis_stp1
  yaxis_stp2 <- myInputs$yaxis_stp2
  yaxis_stp3 <- myInputs$yaxis_stp3
  longitudinal <- myInputs$longitudinal
  xaxis_bins <- myInputs$xaxis_bins
  mySubset <- myInputs$mySubset
  myTimeframe1 <- myInputs$myTimeframe1
  myTimeframe2 <- myInputs$myTimeframe2

  if (!is.null(hlongitudinal1)) {
    if (myY == hlongitudinal1) { 
      myY <- longitudinal1
    }
  }
  
  data <- axesQuery()
  if (is.null(data)) { return() }
  data <- timelineData(mySubset, myTimeframe1, myTimeframe2, data, longitudinal1, longitudinal2, lon1Data, lon2Data)

  if (all(is.na(data[, myY, with=FALSE]))) {
    showNotification("the Y-axis variable has no data for the timepoint(s) selected, please select another.", duration = NULL, type = "error")
    return()
  }

  aggKey <- aggKey()
  if (contLongitudinal) {
    myCols <- c(aggKey, myY, longitudinal)
    tempData <- data[, myCols, with=FALSE]
    colnames(tempData) <- c(aggKey, "YAXIS", "XAXIS")
  } else {
    myCols <- c(aggKey, myY)
    tempData <- data[, myCols, with=FALSE]
    tempData <- unique(tempData)
    colnames(tempData) <- c(aggKey, "YAXIS")
  }

  if (contLongitudinal) {
    tempData <- tempData[!is.na(tempData$XAXIS) ,]
    tempData$XAXIS <- rcut(tempData$XAXIS, xaxis_bins)
    #hackish way to force reactive update if user keeps trying to change the param back to higher val
    tmp <- uniqueN(tempData$XAXIS)
    if (xaxis_bins != tmp) {
      numXBins$val <<- xaxis_bins
      numXBins$val <<- tmp
    } 
  }
  unique(tempData)
})


   #all the work will be done here in prepping data

tableData <- reactive({
  if (is.null(validateAndDebounceAxes()) | is.null(validateAndDebounceTimeline()) | is.null(validateAndDebounceGroup()) | is.null(validateAndDebounceFacet()) | is.null(validateAndDebounceFacet2())) {
    return()
  }
  myInputs <- c(validateAndDebounceAxes(), validateAndDebounceTimeline(), validateAndDebounceGroup(), validateAndDebounceFacet(), validateAndDebounceFacet2())

  myY <- myInputs$myY
  yaxis_stp1 <- myInputs$yaxis_stp1
  yaxis_stp2 <- myInputs$yaxis_stp2
  yaxis_stp3 <- myInputs$yaxis_stp3
  xaxisVar <- myInputs$xaxisVar
  xaxis_bins <- myInputs$xaxis_bins

  groupsType <- myInputs$groupsType
  myGroups <- myInputs$myGroups
  groups_stp1 <- myInputs$groups_stp1
  groups_stp3 <- myInputs$groups_stp3
  groups_stp2 <- myInputs$groups_stp2
  groups_stp4 <- myInputs$groups_stp4

  mySubset <- myInputs$mySubset
  myTimeframe1 <- myInputs$myTimeframe1
  myTimeframe2 <- myInputs$myTimeframe2

  facetType <- myInputs$facetType
  myFacet <- myInputs$myFacet
  facet_stp1 <- myInputs$facet_stp1
  facet_stp3 <- myInputs$facet_stp3
  facet_stp2 <- myInputs$facet_stp2
  facet_stp4 <- myInputs$facet_stp4

  facet2Type <- myInputs$facet2Type
  myFacet2 <- myInputs$myFacet2
  facet2_stp1 <- myInputs$facet2_stp1
  facet2_stp3 <- myInputs$facet2_stp3
  facet2_stp2 <- myInputs$facet2_stp2
  facet2_stp4 <- myInputs$facet2_stp4

  if (length(yaxis_stp2) > 1) {
    yaxisStp2Text <<- ""
    for (i in seq(length(yaxis_stp2))) {
      yaxisStp2Text <<- paste0(yaxisStp2Text,
      "input$yaxis_stp2\t", yaxis_stp2[i], "\n")
    }
  } else {
    yaxisStp2Text <<- paste0("input$yaxis_stp2\t", yaxis_stp2, "\n")
  }

  groupsText <- groupText("groupInfo", myGroups, groups_stp1, groups_stp2, groups_stp3, groups_stp4)
  longitudinalText <- longitudinalText(mySubset, myTimeframe1, myTimeframe2)
  facetText <- groupText("facetInfo", myFacet, facet_stp1, facet_stp2, facet_stp3, facet_stp4)
  facet2Text <- groupText("facet2Info", myFacet2, facet2_stp1, facet2_stp2, facet2_stp3, facet2_stp4)

        text <- paste0("input\tselected\n",
                       longitudinalText,
                       facetText,
                       facet2Text,
                       groupsText,
                       "input$xaxisVar\t", xaxisVar, "\n",
                       "input$xaxis_stp2\t", xaxis_bins, "\n",
                       "input$groupsType\t", groupsType, "\n",
                       "input$facetType\t", facetType, "\n",
                       "input$facet2Type\t", facet2Type, "\n",
                       "input$yaxis\t", myY, "\n",
                       "input$yaxis_stp1\t", yaxis_stp1, "\n",
                       yaxisStp2Text,
                       "input$yaxis_stp3\t", yaxis_stp3
                      # "input$individualPlot_stp1\t", input$individualPlot_stp1, "\n",
                      # "input$individualPlot_stp2\t", input$individualPlot_stp2
                   )
message("\n", Sys.time(), " ClinEpi_Summaries/server/plotData.R: tableData: writing properties in propUrl:", propUrl, "\n", text)  

        PUT(propUrl, body = "")
        PUT(propUrl, body = text)

  aggKey <- aggKey()

  axesData <- axes()
  if (is.null(axesData)) {
    return()
  }

  tempData <- axesData
  groupData <- group()
  if (!is.null(groupData)) {
    if (contLongitudinal) {
      tempData <- merge(tempData, groupData, by = aggKey, all.x = TRUE)
    } else {
      tempData <- merge(tempData, groupData, by = aggKey, all = TRUE)
    }
  } else {
    tempData$GROUPS <- "All"
  }

  facetData <- facet1()
  if (!is.null(facetData)) {
	  colnames(facetData) <- c(aggKey, "FACET")
    tempData <- merge(tempData, facetData, by = aggKey, all.x = TRUE)
  }

  facetData2 <- facet2()
  if (!is.null(facetData2) && myFacet != myFacet2) {
	  colnames(facetData2) <- c(aggKey, "FACET2")
    tempData <- merge(tempData, facetData2, by = aggKey, all.x = TRUE)
  }
 
  unique(tempData)
})

plotData <- reactive({
  plotData <- tableData()
  if (is.null(plotData)) { return() } 
  if (is.null(validateAndDebounceAxes())) {
    return()
  }
  myInputs <- c(validateAndDebounceAxes())
  myY <- myInputs$myY
  yaxis_stp1 <- myInputs$yaxis_stp1
  yaxis_stp2 <- myInputs$yaxis_stp2
  yaxis_stp3 <- myInputs$yaxis_stp3

  strings <- getStrings(metadata.file)
  aggKey <- aggKey()
  #prepare for return
  #determine necessary column id vectors before start
  if (any(colnames(plotData) %in% "FACET") | any(colnames(plotData) %in% "FACET2")) {
    if (any(colnames(plotData) %in% "FACET")) {
      if (any(colnames(plotData) %in% "FACET2")) {
        facetCols <- c("FACET", "FACET2")
      } else {
        facetCols <- c("FACET")
      }
    } else {
      if (any(colnames(plotData) %in% "FACET2")) {
        facetCols <- c("FACET2")
      }
    }
    facetStr <- paste(facetCols, collapse = " + ")
    aggStr2 <- paste("PARTICIPANT_ID ~ GROUPS + ", facetStr)
    sumCols <- c("GROUPS", facetCols, "SUM")
    mergeBy <- c("GROUPS", facetCols)
    dropCols <- c(aggKey, "YAXIS")
    if (any(colnames(plotData) %in% "XAXIS")) {
      aggStr1 <- paste("YAXIS ~ GROUPS + XAXIS + ", facetStr)
      aggStr4 <- paste("SIZE ~ GROUPS + XAXIS + ", facetStr)
      mergeBy2 <- c("GROUPS", "XAXIS", facetCols)
    } else {
      aggStr1 <- paste("YAXIS ~ GROUPS + ", facetStr)
      aggStr4 <- paste("SIZE ~ GROUPS + ", facetStr)
      mergeBy2 <- c("GROUPS", facetCols)
    }
  } else {
    aggStr2 <- "PARTICIPANT_ID ~ GROUPS"
    sumCols <- c("GROUPS", "SUM")
    mergeBy <- c("GROUPS")
    dropCols <- c(aggKey, "YAXIS")
    if (any(colnames(plotData) %in% "XAXIS")) {
      aggStr1 <- "YAXIS ~ GROUPS + XAXIS"
      aggStr4 <- "SIZE ~ GROUPS + XAXIS"
      mergeBy2 <- c("GROUPS", "XAXIS")
    } else {
      aggStr1 <- "YAXIS ~ GROUPS"
      aggStr4 <- "SIZE ~ GROUPS"
      mergeBy2 <- c("GROUPS")
    }
  }

  myPrtcpntView <- prtcpntView$val
  if (myPrtcpntView == TRUE) {
    aggStr3 <- aggStr1
    aggStr1 <- paste0(aggStr1, " + PARTICIPANT_ID")
    countFun <- function(x) {length(unique(x))}
  } else {
    aggStr3 <- aggStr1
    aggStr1 <- paste0(aggStr1, " + " , paste(aggKey, collapse = " + "))
    countFun <- function(x) {length(x)}
  }
  if (myY %in% strings$SOURCE_ID) {
    mergeData <- NULL
    if (yaxis_stp1 == "any" | prtcpntView$val == FALSE) {
      for (i in seq(length(yaxis_stp2))) {
        tempData <- transform(plotData, "YAXIS" = ifelse(YAXIS == yaxis_stp2[i], 1, 0))        
        groupSize <- aggregate(as.formula(aggStr1), tempData, length)
        names(groupSize)[length(names(groupSize))] <- "SIZE"
        tempData <- aggregate(as.formula(aggStr1), tempData, sum)
        tempData <- transform(tempData, "YAXIS"=ifelse(YAXIS >= 1, 1, 0))
        tempData <- merge(tempData, groupSize, by = c(aggKey(), mergeBy2))
        if (is.null(mergeData)) {
          mergeData <- tempData
        } else {
          names(tempData)[names(tempData) == "YAXIS"] <- "prevY"
          cols <- c(aggKey, "prevY")
          tempData <- tempData[, cols]
	message("merging to prev axes data..")
          mergeData <- merge(mergeData, tempData, by = aggKey)
          mergeData <- transform(mergeData, "YAXIS" = ifelse(prevY == 1 | YAXIS == 1, 1, 0))
          mergeData$prevY <- NULL
	message("recalculating yaxis")
          mergeData <- aggregate(as.formula(aggStr1), mergeData, sum)
          mergeData <- transform(mergeData, "YAXIS"=ifelse(YAXIS >= 1, 1, 0))
        }
      }
    } else {
        mergeData <- aggregate(as.formula(aggStr1), plotData, FUN = function(x){ ifelse(uniqueN(x) == length(yaxis_stp2), all(sort(levels(as.factor(x))) == sort(yaxis_stp2)), FALSE) }) 
        mergeData <- transform(mergeData, "YAXIS" = ifelse(YAXIS == TRUE, 1, 0))
    }
    if ("SIZE" %in% colnames(mergeData)) {
      groupSize <- aggregate(as.formula(aggStr4), mergeData, sum)
    } else {
      groupSize <- aggregate(as.formula(aggStr3), mergeData, length)
      names(groupSize)[length(names(groupSize))] <- "SIZE"
    }
    mergeData <- aggregate(as.formula(aggStr3), mergeData, sum)
    mergeData <- merge(mergeData, groupSize, by = mergeBy2)
    if (yaxis_stp3 == "proportion") {
      mergeData$YAXIS <- mergeData$YAXIS / mergeData$SIZE
      mergeData$SIZE <- NULL
    } else {
      mergeData$SIZE <- NULL
    }
  }
  if (yaxis_stp3 == "smooth" | yaxis_stp3 == "mean") {
    plotData$PARTICIPANT_ID <- NULL
    plotData <- unique(plotData)
  } else {
    plotData <- plotData[, -dropCols, with=FALSE]
    plotData <- unique(plotData)
    plotData <- merge(plotData, mergeData, by = mergeBy2)
  }
  plotData <- unique(plotData)
  if (all(as.numeric(levels(as.factor(plotData$GROUPS))) %in% c(1,0))) {
    plotData <- transform(plotData, "GROUPS" = ifelse(GROUPS == 1, "Positive", "Negative"))
  }
  plotData
})
