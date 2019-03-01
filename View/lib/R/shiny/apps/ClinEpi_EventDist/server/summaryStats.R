    observeEvent(plotData(), {
      plotData <- plotData()
      if (is.null(plotData)) {
        return()
      }
      if (input$facetType == "none") {
        myFacet <- "none"
      } else {
        myFacet <- getMyFacet$val
      }
      if (input$facet2Type == "none") {
        myFacet2 <- "none"
      } else {
        myFacet2 <- getMyFacet2$val
      }
      myX <- getMyX$val
      if ("FACET" %in% colnames(plotData)) {
        myFacet <- "FACET"
      }
      if ("FACET2" %in% colnames(plotData)) {
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
      countFun <- function(x) {length(unique(x))}
      colVal <- length(unique(data[, aggKey(), with=FALSE]))
message(colVal)
message(class(data))
        if (myPrtcpntView == TRUE) {
          colLabel <- "# Participants"
        } else {
          colLabel <- "# Observations"
        }

        if (myFacet == "none") {
          tableData <- data.table("Strata" = "All", colLabel = colVal)
          if (myX %in% nums) {
            tableData <- cbind(tableData, "Mean" = round(mean(data[[myX]], na.rm = TRUE),4))
            tableData <- cbind(tableData, "Median" = median(data[[myX]], na.rm = TRUE))
            tableData <- cbind(tableData, "Range" = paste(min(data[[myX]], na.rm = TRUE), "-", max(data[[myX]], na.rm = TRUE)))
            tableData <- cbind(tableData, "SD" = round(sd(data[[myX]], na.rm = TRUE),4))
            tableData <- cbind(tableData, "IQR" = round(IQR(data[[myX]], na.rm = TRUE),4))
          }
        } else {
          aggStr <- paste0("aggCol ~ ", myFacet)
          aggStr2 <- paste0(myX, " ~ ", myFacet)
          data$aggCol <- do.call(paste0, data[, aggKey(), with=FALSE])
          tableData <- aggregate(as.formula(aggStr), data, FUN = countFun)
          if (myX %in% nums) {
            mean <- aggregate(as.formula(aggStr2), data, FUN = function(x){round(mean(x),4)})
            tableData <- merge(tableData, mean, by = myFacet)
            median <- aggregate(as.formula(aggStr2), data, median)
            tableData <- merge(tableData, median, by = myFacet)
            range <- aggregate(as.formula(aggStr2), data, FUN = function(x){paste(min(data[[myX]], na.rm = TRUE), "-", max(data[[myX]], na.rm = TRUE))})
            tableData <- merge(tableData, range, by = myFacet)
            mySD <- aggregate(as.formula(aggStr2), data, FUN = function(x){round(sd(x),4)})
            tableData <- merge(tableData, mySD, by = myFacet)
            myIQR <- aggregate(as.formula(aggStr2), data, FUN = function(x){round(IQR(x),4)})
            tableData <- merge(tableData, myIQR, by = myFacet)
            colnames(tableData) <- c("Strata", colLabel, "Mean", "Median", "Range", "SD", "IQR")
          } else {
            colnames(tableData) <- c("Strata", colLabel)
          }
        }
        if (length(colnames(tableData)) == 7) {
          myTargets <- c(1,2,3,4,5,6)
        } else {
          myTargets <- c(1)
        }

        if (length(facets) > 0) {
          myCaption <- paste("Strata:",paste(facets, collapse = " and "))
        } else {
          myCaption <- ""
        }

        output[[id]] <- DT::renderDataTable(datatable(tableData,
                                                      caption = myCaption,
                                                      width = '100%',
                                                      rownames = FALSE,
                                                      options = list(
                                                        columnDefs = list(list(className = 'dt-right', targets = myTargets))
                                                      )
        ))
      }

      output$dt <- renderUI({
        lapply(1:dt_len, function(i) {
          id <- paste0("dt", i)
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
