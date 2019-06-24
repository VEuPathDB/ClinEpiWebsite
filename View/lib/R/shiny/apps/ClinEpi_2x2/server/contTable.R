    observeEvent(plotData(), {
      plotData <- as.data.table(plotData())
      if (is.null(plotData)) {
        return()
      }
      if (is.null(validateAndDebounceFacet()) | is.null(validateAndDebounceFacet2())) { return() }
      myInputs <- c(validateAndDebounceFacet(), validateAndDebounceFacet2())
      
      facetType <- myInputs$facetType
      facet2Type <- myInputs$facet2Type
      myFacet <- myInputs$myFacet
      myFacet2 <- myInputs$myFacet2
      if ("FACET" %in% colnames(plotData)) {
        myFacet <- "FACET"
      }
      if ("FACET2" %in% colnames(plotData)) {
        myFacet2 <- "FACET2"
      }
      myPrtcpntView <- prtcpntView$val
      if (!(myFacet %in% colnames(plotData)) & myFacet != "none") {
        return()
      }
      if (!(myFacet2 %in% colnames(plotData)) & myFacet2 != "none") {
        return()
      }

      if (myFacet2 != "none") {
        if (myFacet != "none") {
          dt_len <- uniqueN(plotData[, c(myFacet, myFacet2), with=FALSE])
          dt_list <- unique(plotData[, c(myFacet, myFacet2), with=FALSE])
        } else {
          dt_len <- uniqueN(plotData[, myFacet2, with=FALSE])
          dt_list <- unique(plotData[, myFacet2, with=FALSE])
          myFacet <- myFacet2
        }
      } else {
        if (myFacet != "none") {
          dt_len <- uniqueN(plotData[, myFacet, with=FALSE])
          dt_list <- unique(plotData[, myFacet, with=FALSE])
        } else {
          dt_len <- 1
          dt_list <- NULL
        }
      }

      createTableUI <- function(id, data, facets) {
        #get or
        #TODO double check i've pulled these out right !!!!
        APOP <- data$Proportion[data$Variable1 == "Attribute+" & data$Variable2 == "Outcome+"]
        AFOP <- data$Proportion[data$Variable1 == "Attribute-" & data$Variable2 == "Outcome+"]
        APOF <- data$Proportion[data$Variable1 == "Attribute+" & data$Variable2 == "Outcome-"]
        AFOF <- data$Proportion[data$Variable1 == "Attribute-" & data$Variable2 == "Outcome-"]

        APtotal <- APOP + APOF
        AFtotal <- AFOP + AFOF
        OPtotal <- APOP + AFOP
        OFtotal <- APOF + AFOF
        total <- APOP + APOF + AFOP + AFOF

        OP <- c(APOP, AFOP, OPtotal)
        OF <- c(APOF, AFOF, OFtotal)
        totals <- c(APtotal, AFtotal, total)

        OPLabel <- data$Outcome[data$Variable2 == "Outcome+"][1]
        OFLabel <- data$Outcome[data$Variable2 == "Outcome-"][1]
        APLabel <- data$Exposure[data$Variable1 == "Attribute+"][1]
        AFLabel <- data$Exposure[data$Variable1 == "Attribute-"][1]
        tableData <- data.table(col1 = OP, col2 = OF, "Totals" = totals)
        colnames(tableData) <- c(as.vector(OPLabel), as.vector(OFLabel), "Totals")
        rownames(tableData) <- c(as.vector(APLabel), as.vector(AFLabel), "Totals")

        if (all(sort(colnames(tableData)) == c("No", "Totals", "Yes"))) {
          setcolorder(tableData, c("Yes", "No", "Totals"))
        }
        if (all(sort(rownames(tableData)) == c("No", "Totals", "Yes"))) {
          setroworder(tableData, c(2,1,3))
          rownames(tableData) <- c("Yes", "No", "Totals")
        }

        if (length(facets) > 0) {
          myCaption <- paste("Strata:",paste(facets, collapse = " and "))
        } else {
          myCaption <- ""
        }

        output[[id]] <- DT::renderDataTable(datatable(tableData,
                                                      caption = myCaption,
                                                      width = '100%',
                                                      options = list(
                                                      sDom = '<"top"><"bottom">',
                                                      autoWidth = TRUE,
                                                      columnDefs = list(list(width = '250px', className = 'dt-right', targets = c(1,2,3)))
                                                      )
        ))
      }

      output$table <- renderUI({
        lapply(1:dt_len, function(i) {
          id <- paste0("table", i)
          facets <- c()
          if (!is.null(dt_list)) {
            if (length(dt_list) == 1) {
              keep <- c(plotData[, myFacet, with=FALSE] == c(dt_list[i]))
              facets <- c(dt_list[i])
              data <- plotData[keep,]
            } else {
              keep1 <- c(plotData[, myFacet, with=FALSE] == c(dt_list[i][, myFacet, with=FALSE]))
              keep2 <- c(plotData[, myFacet2, with=FALSE] == c(dt_list[i][, myFacet2, with=FALSE]))
              keep <- keep1 & keep2
              facets <- c(c(dt_list[i][, myFacet, with=FALSE]), c(dt_list[i][, myFacet2, with=FALSE]))
              data <- plotData[keep,]
            }
          } else {
            data <- plotData
            facets <- c()
          }
          createTableUI(id, data, facets)
        })
      })
    })
