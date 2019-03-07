source("../../functions/timelineServer.R", local = TRUE)

source("../../functions/facetServer.R", local = TRUE)

group <- reactive({
      groupsType <- input$groupsType
	if (is.null(groupsType)) { return() }

      if (groupsType == 'none') {
	myGroups <- "none"
      } else {
	myGroups <- getMyGroups$val
      }
      #grab optional inputs
      groups_stp1 <- groupInfo$group_stp1
      groups_stp3 <- groupInfo$group_stp3
      groups_stp4 <- groupInfo$group_stp4
      groups_stp2 <- groupInfo$group_stp2

        if (groupsType == "makeGroups") {
          if (is.null(groups_stp1)) {
            return()
          } else {
            if (groups_stp1 == 'any' | groups_stp1 == 'all') {
              if (is.null(groups_stp2)) {
                return()
              } else {
                if (groups_stp2 %in% c("lessThan", "greaterThan", "equals")) {
                  if (is.null(groups_stp3)) {
                    return()
                  }
                }
              }
            }
          }
        } else if (groupsType == "direct") {
          if (is.null(myGroups)) {
            return()
          } else if (length(myGroups) == 0) {
	    return()
	  }
          if (myGroups == "none") {
            return()
          }
        }

	nums <- getNums(metadata.file)
	dates <- getDates(metadata.file)
	aggKey <- aggKey()
	data <- timelineData()
        if (groupsType == "direct") {
          myCols <- c(aggKey, myGroups)
          outData <- data[, myCols, with=FALSE]
          outData <- unique(outData)
          colnames(outData) <- c(aggKey, "GROUPS")

          if (myGroups %in% nums$source_id | myGroups %in% dates$source_id) {
            if (length(levels(as.factor(outData$GROUPS))) >= 4) {
              outData$GROUPS <- rcut_number(outData$GROUPS)
            } else {
              outData$GROUPS <- as.factor(outData$GROUPS)
            }
          }
        } else if (groupsType == "makeGroups") {
          numeric <- c("lessThan", "greaterThan", "equals")
          anthro <- c("percentDays", "delta", "direct")
          if (groupsType != "none") {
            if (is.null(groups_stp1)) {
              message("groups stp1 is null... returning")
              return()
            } else {
              if (groups_stp1 %in% numeric) {
                if (is.null(groups_stp2)) {
                  return()
                }
              }
              if (groups_stp1 %in% anthro) {
                if (groups_stp1 == "percentDays") {
                  if (is.null(groups_stp4)) {
                    return()
                  }
                } else {
                  if (is.null(groups_stp3)) {
                    return()
                  }
                }
              }
            }
            outData <- makeGroups(data, metadata.file, myGroups, groups_stp1, groups_stp2, groups_stp3, groups_stp4, aggKey)
	    if (is.null(outData)) { return() }
            observations <- metadata.file$source_id[metadata.file$category == "Observation"]
            observations <- observations[observations %in% colnames(studyData)]
            label <- makeGroupLabel(myGroups, metadata.file, groups_stp1, groups_stp2, groups_stp3, groups_stp4, event.list = observations)
            #add makeGroups data to df and return
            outData <- transform(outData, "GROUPS" = ifelse(as.numeric(GROUPS) == 0, label[2], label[1]))
          } else {
            myCols <- c(aggKey)
            outData <- data[, myCols, with=FALSE]
            outData <- unique(outData)
            colnames(outData) <- c(aggKey)
            if (prtcpntView$val == TRUE) {
	      outData <- cbind(outData, "GROUPS" = "All Participants")
            } else {
              outData <- cbind(outData, "GROUPS" = "All Observations")
            }
          }
        }
        outData <- unique(outData)

        groupsText <<- groupText("groupInfo", myGroups, groups_stp1, groups_stp2, groups_stp3, groups_stp4)

  outData
})


axes <- reactive({
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
        print(yaxis_stp1)
        if (myY %in% strings$source_id) {
          if (is.null(yaxis_stp2)) {
            return()
          }
        }
      }
      if (is.null(yaxis_stp3)) {
        return()
      }

        #this needs revision since stp2 could have multiple values
        if (length(yaxis_stp2) > 1) {
          yaxisStp2Text <<- ""
          for (i in seq(length(yaxis_stp2))) {
            yaxisStp2Text <<- paste0(yaxisStp2Text,
            "input$yaxis_stp2\t", yaxis_stp2[i], "\n")
          }
        } else {
          yaxisStp2Text <<- paste0("input$yaxis_stp2\t", yaxis_stp2, "\n")
        }

	aggKey <- aggKey()
	data <- timelineData()
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

xaxis_bins <- input$xaxis_stp2
        if (contLongitudinal) {
          tempData$XAXIS <- cut(tempData$XAXIS, xaxis_bins)
        }

unique(tempData)
})


   #all the work will be done here in prepping data

    tableData <- reactive({

	groupsType <- input$groupsType
        if (is.null(groupsType)) { return() }
	myY <- getMyY$val
        yaxis_stp1 <- input$yaxis_stp1
        yaxis_stp2 <- input$yaxis_stp2
        if (prtcpntView$val != TRUE) {
          yaxis_stp2 <- input$yaxis_stp1
          yaxis_stp1 <- "any"
        }
        yaxis_stp3 <- input$yaxis_stp3
        yaxis_stp1 <- input$yaxis_stp1
	if (is.null(input$facet2Type)) { return() }
	facet2Type <- input$facet2Type
	if (is.null(input$facetType)) { return() }
	facetType <- input$facetType

        text <- paste0("input\tselected\n",
                       longitudinalText,
                       facetText,
                       facet2Text,
                       groupsText,
                       "input$xaxisVar\t", input$xaxisVar, "\n",
                       "input$xaxis_stp2\t", input$xaxis_stp2, "\n",
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

        PUT(propUrl, body = "")
        PUT(propUrl, body = text)

        aggKey <- aggKey()

  #TODO figure out tempData
        axesData <- axes()
        if (is.null(axesData)) {
          return()
	}
	tempData <- axesData

        groupData <- group()
        if (is.null(groupData)) {
	  return()
	}
        tempData <- merge(tempData, groupData, by = aggKey)

        facetData <- facet1()
        if (!is.null(facetData)) {
	  colnames(facetData) <- c(aggKey, "FACET")
          tempData <- merge(tempData, facetData, by = aggKey)
        }

        facetData2 <- facet2()
        if (!is.null(facetData2)) {
	  colnames(facetData2) <- c(aggKey, "FACET2")
          tempData <- merge(tempData, facetData2, by = aggKey)
        }

      unique(tempData)
    })




    plotData <- reactive({
        plotData <- tableData()
        if (is.null(plotData)) {
          return()
        } else {
          #collecting inputs .. i think these are the only ones i need here.. well see
          myY <- getMyY$val
          if (is.null(input$yaxis_stp3)) {
            return()
          } else {
            yaxis_stp3 <- input$yaxis_stp3
          }
          yaxis_stp1 <- input$yaxis_stp1
          yaxis_stp2 <- input$yaxis_stp2
          if (prtcpntView$val != TRUE) {
            yaxis_stp2 <- input$yaxis_stp1
            yaxis_stp1 <- "any"
          }

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
          aggStr2 <- paste("Participant_Id ~ GROUPS + ", facetStr)
          sumCols <- c("GROUPS", facetCols, "SUM")
          mergeBy <- c("GROUPS", facetCols)
          dropCols <- c(aggKey, "YAXIS")
          if (any(colnames(plotData) %in% "XAXIS")) {
            aggStr1 <- paste("YAXIS ~ GROUPS + XAXIS + ", facetStr)
            mergeBy2 <- c("GROUPS", "XAXIS", facetCols)
          } else {
            aggStr1 <- paste("YAXIS ~ GROUPS + ", facetStr)
            mergeBy2 <- c("GROUPS", facetCols)
          }
        } else {
          aggStr2 <- "Participant_Id ~ GROUPS"
          sumCols <- c("GROUPS", "SUM")
          mergeBy <- c("GROUPS")
          dropCols <- c(aggKey, "YAXIS")
          if (any(colnames(plotData) %in% "XAXIS")) {
            aggStr1 <- "YAXIS ~ GROUPS + XAXIS"
            mergeBy2 <- c("GROUPS", "XAXIS")
          } else {
            aggStr1 <- "YAXIS ~ GROUPS"
            mergeBy2 <- c("GROUPS")
          }
        }

        myPrtcpntView <- prtcpntView$val
        if (myPrtcpntView == TRUE) {
          aggStr3 <- aggStr1
          aggStr1 <- paste0(aggStr1, " + Participant_Id")
          countFun <- function(x) {length(unique(x))}
        } else {
          aggStr3 <- aggStr1
          aggStr1 <- paste0(aggStr1, " + " , paste(aggKey, collapse = " + "))
          countFun <- function(x) {length(x)}
        }

        if (myY %in% strings$source_id) {
          mergeData <- NULL
          if (yaxis_stp1 == "any" | prtcpntView$val == FALSE) {
            #will have to replace all instances of myY with 1 and all else with 0 before can sum
            for (i in seq(length(yaxis_stp2))) {
              tempData <- transform(plotData, "YAXIS" = ifelse(YAXIS == yaxis_stp2[i], 1, 0))
              #the following to get proportions of prtcpnts with matching observatio rather than proportion of matching observations.
              tempData <- aggregate(as.formula(aggStr1), tempData, sum)
              tempData <- transform(tempData, "YAXIS"=ifelse(YAXIS >= 1, 1, 0))
              #tempData <- aggregate(as.formula(paste0(aggStr1, " + Participant_Id")), plotData, FUN = function(x){ if(yaxis_stp2[[i]] %in% x) {1} else {0} })
              if (is.null(mergeData)) {
                mergeData <- tempData
              } else {
                names(tempData)[names(tempData) == "YAXIS"] <- "prevY"
                print(colnames(tempData))
                cols <- c(aggKey, "prevY")
                tempData <- tempData[, cols]
                print(unique(tempData$prevY))
                print(unique(mergeData$YAXIS))
                mergeData <- merge(mergeData, tempData, by = aggKey)
                print(unique(mergeData$YAXIS))
                mergeData <- transform(mergeData, "YAXIS" = ifelse(prevY == 1 | YAXIS == 1, 1, 0))
                print(unique(mergeData$YAXIS))
                mergeData$prevY <- NULL
                #the following to get proportions of prtcpnts with matching observatio rather than proportion of matching observations.
                mergeData <- aggregate(as.formula(aggStr1), mergeData, sum)
                mergeData <- transform(mergeData, "YAXIS"=ifelse(YAXIS >= 1, 1, 0))
              }
            }
          } else {
              mergeData <- aggregate(as.formula(aggStr1), plotData, FUN = function(x){ ifelse(length(levels(as.factor(x))) == length(yaxis_stp2), all(sort(levels(as.factor(x))) == sort(yaxis_stp2)), FALSE) })
              mergeData <- transform(mergeData, "YAXIS" = ifelse(YAXIS == TRUE, 1, 0))
          }
          mergeData <- aggregate(as.formula(aggStr3), mergeData, sum)
          print(head(mergeData))
          if (yaxis_stp3 == "proportion") {
            print(unique(mergeData$YAXIS))
            groupSum <- as.data.table(aggregate(as.formula(aggStr2), plotData, FUN = countFun))
            print(unique(mergeData$YAXIS))
            colnames(groupSum) <- sumCols
            mergeData <- as.data.table(mergeData)
            mergeData <- merge(mergeData, groupSum, by = mergeBy)
            proportion <- mergeData$YAXIS / mergeData$SUM
            mergeData$YAXIS <- proportion
            mergeData$SUM <- NULL
          }
        }
        if (yaxis_stp3 == "smooth" | yaxis_stp3 == "mean") {
          plotData$Participant_Id <- NULL
          plotData <- unique(plotData)
        } else {
          plotData <- plotData[, -dropCols, with=FALSE]
          plotData <- unique(plotData)
          print(unique(mergeData$YAXIS))
          print(mergeBy2)
          plotData <- merge(plotData, mergeData, by = mergeBy2)
        }
        plotData <- unique(plotData)
        if (all(as.numeric(levels(as.factor(plotData$GROUPS))) %in% c(1,0))) {
          plotData <- transform(plotData, "GROUPS" = ifelse(GROUPS == 1, "Positive", "Negative"))
        }
        plotData
        }

    })

