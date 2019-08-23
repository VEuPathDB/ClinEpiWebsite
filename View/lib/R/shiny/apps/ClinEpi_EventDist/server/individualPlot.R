    output$individualPlot_stp1 <- renderUI({
      myInputs <- validateAndDebounceFacet()
      if (is.null(myInputs)) { return() }
      myFacet <- myInputs$myFacet
      facetType <- myInputs$facetType

      if (myFacet == "none") {
        return()
      }

      df <- plotData()

      if (is.null(df)) {
        return()
      }

      if (facetType == "makeGroups") {
        myFacet <- "FACET"
      }
      facetVals <- unique(df[,myFacet, with=FALSE])
      facetVals <- unlist(facetVals)
      names(facetVals) <- facetVals

      if (is.null(properties)) {
        mySelected <- c("abc")
      } else {
        mySelected <- properties$selected[properties$input == 'input$individualPlot_stp1']
      }

      selectInput(inputId = "individualPlot_stp1",
                  label = "Stratify Plot (1) value:",
                  choices = facetVals,
                  selected = mySelected)
    })

    output$individualPlot_stp2 <- renderUI({
      myInputs <- validateAndDebounceFacet2()
      if (is.null(myInputs)) { return() }
      myFacet2 <- myInputs$myFacet2
      facet2Type <- myInputs$facet2Type

      if (myFacet2 == "none") {
        return()
      }

      #TODO try to save this globally and reactively have it update?? cause right now its called in three different places..
      df <- plotData()
      if (facet2Type == "makeGroups") {
        myFacet2 <- "FACET2"
      }
      facet2Vals <- unique(df[,myFacet2, with=FALSE])
      facet2Vals <- unlist(facet2Vals)
      names(facet2Vals) <- facet2Vals

      if (is.null(properties)) {
        mySelected <- c("abc")
      } else {
        mySelected <- properties$selected[properties$input == 'input$individualPlot_stp2']
      }

      selectInput(inputId = "individualPlot_stp2",
                  label = "Stratify Plot (2) value:",
                  choices = facet2Vals,
                  selected = mySelected)
    })

    output$individual_distribution <- renderPlotly({
      myInputs <- c(validateAndDebounceAxes(), validateAndDebounceFacet(), validateAndDebounceFacet2())
      if (is.null(validateAndDebounceAxes()) | is.null(validateAndDebounceFacet()) | is.null(validateAndDebounceFacet2())) { return() }

      myX <- myInputs$myX
      myFacet <- myInputs$myFacet
      if (myFacet != "none") {
        if (is.null(input$individualPlot_stp1)) {
          return()
        }
      }
      facetType <- myInputs$facetType
      myFacet2 <- myInputs$myFacet2
      if (myFacet2 != "none") {
        if (is.null(input$individualPlot_stp2)) {
          return()
        }
      }
      facet2Type <- myInputs$facet2Type
      iPlot_stp1 <- input$individualPlot_stp1
      iPlot_stp2 <- input$individualPlot_stp2

      if (facetType == "makeGroups") {
        myFacet <- "FACET"
      }
      if (facet2Type == "makeGroups") {
        myFacet2 <- "FACET2"
      }

      nums <- getNums(metadata.file)
      dates <- getDates(metadata.file)
      if (myX == 'ageDays') {
        xlab <- "Age in Days"
      } else if (myX == 'zscore') {
        xlab <- "Z-score"
      } else {
        xlab <- subset(metadata.file, metadata.file$SOURCE_ID %in% myX)
        xlab <- as.character(xlab[1,2])
      }

      df <- plotData()
      df <- completeDT(df, myX)

      if (myFacet != "none" & !is.null(iPlot_stp1)) {
        keep <- c(df[, myFacet, with=FALSE] == iPlot_stp1)
        df <- df[keep,]
      }
      if (myFacet2 != "none" & !is.null(iPlot_stp2)) {
        keep2 <- c(df[, myFacet2, with=FALSE] == iPlot_stp2)
        df <- df[keep2,]
      }

      myPlot <- ggplot(data = subset(df, !is.na(get(myX))), aes_string(x = myX))
      myPlot <- myPlot + theme_bw()
      myPlot <- myPlot + labs(y = "", x = "")

      if ((myX %in% nums$SOURCE_ID | myX %in% dates$SOURCE_ID) & myX != myFacet) {
        myPlot <- myPlot + geom_histogram(aes(text = paste0("Count: ", ..count..)), stat = "bin", fill = viridis(1, end = .25, direction = -1))
      } else {
        myPlot <- myPlot + geom_histogram(aes(text = paste0("Count: ", ..count..)), stat = "count", fill = viridis(1, end = .25, direction = -1))
        myPlot <- myPlot + theme(axis.text.x = element_text(angle = 90, hjust = 1))
      }

      x_list <- list(
        title = paste0(c(rep("\n", 3),
                         rep(" ", 10),
                         xlab,
                         rep(" ", 10)),
                       collapse = ""),
        size = 14
      )
      y_list <- list(
        title = paste0(c(rep(" ", 10),
                         "Count",
                         rep(" ", 10),
                         "\n"),
                       collapse = ""),
        size = 14
      )

      myPlotly <- ggplotly(myPlot, tooltip = c("text"), , width = (0.70*as.numeric(input$dimension[1])), height = as.numeric(input$dimension[2]))
      myPlotly <- plotly:::config(myPlotly, displaylogo = FALSE)
      myPlotly <- layout(myPlotly, margin = list(l = 70, r = 50, b = 200, t = 40),
                         xaxis = x_list,
                         yaxis = y_list,
                         legend = list(x = 100, y = .5),
                         autosize=TRUE)

      myPlotly
    })

