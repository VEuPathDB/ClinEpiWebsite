output$distribution <- renderPlotly({
      message("render plot!")
      if (is.null(input$xaxis)) {
        message("xaxis null")
        return()
      }
      if (is.null(input$facetType)) {
	message("facetType null")
        return()
      }
      if (is.null(input$facet2Type)) {
	message("facetType2 null")
        return()
      }
      myX <- input$xaxis
      if (myX == "direct" | myX == "makeGroups") {
        if (is.null(getMyX$val)) {
          message("xaxis stp1 null")
          return()
        } else {
          myX <- getMyX$val
        }
      }
      if (input$facetType == "none") {
        myFacet <- "none"
      } else {
        myFacet <- getMyFacet$val
      }
      facetType <- input$facetType
      if (input$facet2Type == "none") {
        myFacet2 <- "none"
      } else {
        myFacet2 <- getMyFacet2$val
      }
      facet2Type <- input$facet2Type
      df <- plotData()

      if (is.null(df)) {
	message("plotData null")
        return()
      }

      message(paste("just starting plot"))

      df <- completeDT(df, myX)

      nums <- getNums(metadata.file)
      dates <- getDates(metadata.file)

      if (myX == 'ageDays') {
        xlab <- "Age in Days"
      } else if (myX == 'zscore') {
        xlab <- "Z-score"
      } else {
        xlab <- subset(metadata.file, metadata.file$source_id %in% myX)
        xlab <- as.character(xlab[1,2])
      }

      myPlot <- ggplot(data = df, aes_string(x = myX))
      myPlot <- myPlot + theme_bw()
      myPlot <- myPlot + labs(y = "", x = "")

        #consider when facetType is makeGroups to use geom_density instead ??
        if ((myX %in% nums$source_id | myX %in% dates$source_id) & myX != myFacet) {
          myPlot <- myPlot + geom_histogram(aes(text = paste0("Count: ", ..count..)), stat = "bin", fill = viridis(1, end = .25, direction = -1))
          myPlot <- myPlot + geom_vline(aes(xintercept = mean(df[[myX]], na.rm = T), text = paste0("mean:", mean(df[[myX]], na.rm = T))), color = viridis(1, begin = .75), linetype = "dashed", size = 1)

          if (facetType == "none" & facet2Type != "none") {
            myFacet <- myFacet2
            facetType <- facet2Type
            facet2Type <- "none"
          }
          print(facetType)
          print(facet2Type)
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
            print(reformulate(myFacet,myFacet2))
            myPlot <- myPlot + facet_grid(reformulate(myFacet, myFacet2))
          }

        } else {
          myPlot <- myPlot + geom_histogram(aes(text = paste0("Count: ", ..count..)), stat = "count", fill = viridis(1, end = .25, direction = -1))
          myPlot <- myPlot + theme(axis.text.x = element_text(angle = 90, hjust = 1))
          if (facetType == "none" & facet2Type != "none") {
            myFacet <- myFacet2
            facetType <- facet2Type
            facet2Type <- "none"
          }
          print(facetType)
          print(facet2Type)
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
            print(reformulate(myFacet,myFacet2))
            myPlot <- myPlot + facet_grid(reformulate(myFacet, myFacet2))
          }

          myPlot <- myPlot + theme(axis.text.x = element_text(angle = 45, hjust = 1))
        }

      message(paste("c'est fini"))
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

      myPlotly <- ggplotly(myPlot, tooltip = c("text"), width = (0.70*as.numeric(input$dimension[1])), height = as.numeric(input$dimension[2]))
      myPlotly <- plotly:::config(myPlotly, displaylogo = FALSE, collaborate = FALSE)
      myPlotly <- layout(myPlotly, margin = list(l = 70, r = 50, b = 200, t = 40),
                                   xaxis = x_list,
                                   yaxis = y_list,
                                   legend = list(x = 100, y = .5),
                                   autosize=TRUE)

      myPlotly
    })
