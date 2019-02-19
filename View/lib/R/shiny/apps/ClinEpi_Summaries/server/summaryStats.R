   observeEvent(tableData(), {
      plotData <- tableData()
      if (is.null(plotData)) {
        return()
      }
      if (input$facetType == "none") {
        myFacet <- "none"
      } else {
        myFacet <- "FACET"
      }
      if (input$facet2Type == "none") {
        myFacet2 <- "none"
      } else {
        myFacet2 <- "FACET2"
      }
     
      myPrtcpntView <- prtcpntView$val
      
      if (myFacet2 != "none") {
        if (myFacet != "none") {
          dt_len <- uniqueN(plotData[, myFacet2, with=FALSE])
          dt_list <- unique(plotData[, myFacet2, with=FALSE]) 
        } else {
          myFacet <- myFacet2
          myFacet2 <- "none"
          dt_len <- 1
          dt_list <- NULL
        }
      } else {
        dt_len <- 1
        dt_list <- NULL
      }

      nums <- getNums(metadata.file)
      dates <- getDates(metadata.file)

      createUI <- function(id, data, facets) {

        if (myPrtcpntView == TRUE) {
          countFun <- function(x){ length(unique(x)) }
          colName <- "# Participants: "
        } else {
          countFun <- function(x){ length(x) }
          colName <- "# Observations: "
        }
        #and still need to remove duplicates across time.
        if (any(colnames(data) %in% "FACET")) {
          data <- reshape(aggregate(Participant_Id ~ FACET + GROUPS, data, FUN = countFun ),
                          timevar = "FACET", idvar = "GROUPS", v.names = "Participant_Id", direction = "wide")
          colnames(data)[1] <- "Line"
          colnames(data) <- gsub("Participant_Id.", colName, colnames(data))
          #give totals
          if (length(data) > 2) {
            data[, "Totals"] <- rowSums(data[, -1], na.rm=TRUE)
          }
          rownames(data) <- data[,1]
          data[,1] <- NULL
          data["Totals" ,] <- colSums(data, na.rm=TRUE)
          data <- cbind("Line" = rownames(data), data)
        } else {
          data <- aggregate(Participant_Id ~ GROUPS, data, countFun )
          colnames(data) <- c("Line", colName)
          #totals
          levels(data$Line) <- c(levels(as.factor(data$Line)),"Totals")
          data <- rbind(data, c("Totals", sum(data[, -1])))
        }

        #fix custom groups if necessary
        if (all(unique(data$Line[-nrow(data)]) %in% c(1,0))) {
          data <- as.data.table(data)
          data <- transform(data, "Line" = ifelse(Line == 1, "Positive", "Negative"))
          #data$Group <- data$temp
          #data <- data[, -"temp"]
          data$Group[nrow(data)] <- "Totals"
        }
        longitudinal <- longitudinal1
        if (!is.null(input$xaxisVar)) {
          if (input$xaxisVar == "ageVar") {
            longitudinal <- longitudinal2
          }
        }
        #temp placeholder for checking if data has time vars for x axis
        if (!contLongitudinal) {
          names(data)[names(data) == 'Line'] <- 'X-Axis'
        }

        if (length(facets) > 0) {
          myCaption <- paste("Strata:",paste(facets, collapse = " and "))
        } else {
          myCaption <- ""
        }

        output[[id]] <- DT::renderDataTable(datatable(data,
                                                      caption = myCaption,
                                                      width = '100%',
                                                      rownames = FALSE
                                                      #options = list(
                                                      #  columnDefs = list(list(className = 'dt-right', targets = myTargets))
                                                      #)
        ))
      }
      output$table <- renderUI({
        lapply(1:dt_len, function(i) {
          id <- paste0("table", i)
          if (!is.null(dt_list)) {
            keep <- c(plotData[, myFacet2, with=FALSE] == c(dt_list[i]))
            facets <- c(dt_list[i])
            data <- plotData[keep,]
          } else {
            data <- plotData
            facets <- c()
          }
          createUI(id, data, facets)
        })
      })
    })

