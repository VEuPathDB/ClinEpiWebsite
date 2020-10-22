output$distribution <- renderPlotly({
      myInputs <- c(validateAndDebounceAxes(), validateAndDebounceFacet(), validateAndDebounceFacet2())
      if (is.null(validateAndDebounceAxes()) | is.null(validateAndDebounceFacet()) | is.null(validateAndDebounceFacet2())) { return() }
      myX <- myInputs$myX
      myFacet <- myInputs$myFacet
      facetType <- myInputs$facetType
      myFacet2 <- myInputs$myFacet2
      facet2Type <- myInputs$facet2Type
      df <- plotData()

      if (is.null(df)) {
        return()
      }

      if (!is.null(hlongitudinal1)) {
        if (myX == hlongitudinal1) {
          myX <- longitudinal1
        }
      }

      df <- completeDT(df, myX)
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

      myPlot <- ggplot(data = subset(df, !is.na(get(myX))), aes_string(x = myX))
      myPlot <- myPlot + theme_bw()
      myPlot <- myPlot + labs(y = "", x = "")

        #consider when facetType is makeGroups to use geom_density instead ??
        if ((myX %in% nums$SOURCE_ID | myX %in% dates$SOURCE_ID) & myX != myFacet) {
          myPlot <- myPlot + geom_histogram(aes(text = paste0("Count: ", ..count..)), stat = "bin", fill = viridis(1, end = .25, direction = -1))
          myPlot <- myPlot + geom_vline(aes(xintercept = mean(df[[myX]], na.rm = T), text = paste0("mean:", mean(df[[myX]], na.rm = T))), color = viridis(1, begin = .75), linetype = "dashed", size = 1)

          if (facetType == "none" & facet2Type != "none") {
            myFacet <- myFacet2
            facetType <- facet2Type
            facet2Type <- "none"
          }
          if (facet2Type == "none") {
            if (facetType == 'direct') {
              myPlot <- myPlot + facet_wrap(reformulate(myFacet), ncol = 1)
              # scale_fill_brewer(palette = cbPalette)
            } else if (facetType == 'makeGroups') {
              myPlot <- myPlot + facet_wrap(~ FACET, ncol = 1)
            }
          } else {
            if (facetType == "makeGroups") {
              myFacet <- "FACET"
            }
            if (facet2Type == "makeGroups") {
              myFacet2 <- "FACET2"
            }
            myPlot <- myPlot + facet_grid(reformulate(myFacet, myFacet2))
          }

        } else {
          myPlot <- myPlot + geom_histogram(aes(text = paste0("Count: ", ..count..)), stat = "count", fill = viridis(1, end = .25, direction = -1))
          myPlot <- myPlot + theme(axis.text.x = element_text(angle = 90, hjust = 1))

          if (facetType == "makeGroups") {
            myFacet <- "FACET"
          }
          if (facet2Type == "makeGroups") {
            myFacet2 <- "FACET2"
          }
          if (facetType == "none" & facet2Type != "none") {
            myFacet <- myFacet2
            facetType <- facet2Type
            facet2Type <- "none"
          }
          if (facet2Type == "none") {
            myPlot <- myPlot + facet_wrap(reformulate(myFacet), ncol = 1)
          } else {

            myPlot <- myPlot + facet_grid(reformulate(myFacet, myFacet2))
          }

          myPlot <- myPlot + theme(axis.text.x = element_text(angle = 45, hjust = 1))
        }

      x_list <- list(
        title = xlab,
        size = 14,
	automargin = TRUE
      )
      y_list <- list(
        title = "Count",
        size = 14,
	automargin = TRUE
      )

      myPlotly <- ggplotly(myPlot, tooltip = c("text"), width = (0.70*as.numeric(input$dimension[1])), height = as.numeric(input$dimension[2]))
      myPlotly <- plotly:::config(myPlotly, displaylogo = FALSE, editable = TRUE, edits = list(shapePosition = FALSE))
      myPlotly <- layout(myPlotly, margin = list(l = 70, r = 50, b = 200, t = 50),
                                   xaxis = x_list,
                                   yaxis = y_list,
                                   legend = list(x = 100, y = .5),
                                   autosize=TRUE)

      myPlotly
    })
