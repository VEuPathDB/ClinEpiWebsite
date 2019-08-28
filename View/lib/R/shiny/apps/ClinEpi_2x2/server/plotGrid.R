
    output$plot <- renderPlotly({
        df <- plotData()
        if (is.null(df)) {
          message("plotData returned null!")
          return()
        }
       
        if (is.null(validateAndDebounceAttr()) | is.null(validateAndDebounceOut()) | is.null(validateAndDebounceFacet()) | is.null(validateAndDebounceFacet2())) { return() }
        myInputs <- c(validateAndDebounceAttr(), validateAndDebounceOut(), validateAndDebounceFacet(), validateAndDebounceFacet2())
        
        facetType <- myInputs$facetType
        facet2Type <- myInputs$facet2Type
        myFacet <- myInputs$myFacet
        myFacet2 <- myInputs$myFacet2
        
        if ("FACET" %in% colnames(df)) {
          myFacet <- "FACET"
        }
        if ("FACET2" %in% colnames(df)) {
          myFacet2 <- "FACET2"
        }
       
        var1 <- myInputs$myAttr
        var2 <- myInputs$myOut 
 
        #define axis labels here
        xlab <- c(metadata.file$PROPERTY[metadata.file$SOURCE_ID == var2][1])
        ylab <- "Proportion"
        
        #determine width of bars for outcome
        #outPos <- tableData[1,3 , with = FALSE]
        #outNeg <- tableData[2,3 , with = FALSE]
        #total <- tableData$Totals[3]
        #OPprop <- outPos / total
        #ONprop <- outNeg / total
        #width <- c( OPprop*1.9, ONprop*1.9, OPprop*1.9, ONprop*1.9)
        #df$width <- width

        df$Outcome <- gsub(xlab, "", df$Outcome)

        #plot here
        myPlot <- ggplot(data = df, aes(x = Outcome, y = Proportion, fill = Exposure))
        myPlot <- myPlot + theme_bw()
        myPlot <- myPlot + labs(y = "", x = "")

        myPlot <- myPlot + geom_bar(stat = "identity", position = "fill")

        if (myFacet2 == "none") {
          if (myFacet != 'none') {
            myPlot <- myPlot + facet_wrap(reformulate(myFacet), ncol = 1)
          }
        } else {
          if (myFacet != 'none') {
            myPlot <- myPlot + facet_grid(reformulate(myFacet, myFacet2))
          } else {
            myPlot <- myPlot + facet_wrap(reformulate(myFacet2), ncol = 1)
          }
        }

        myPlot <- myPlot + scale_fill_manual(name = "", values = viridis(2, begin = .25, end = .75))
        myPlot <- myPlot + theme(axis.text.x = element_text(angle = 45, hjust = 1))

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
                         ylab,
                         rep(" ", 10),
                         "\n"),
                         collapse = ""),
          size = 14
        )
        maxChars <- max(nchar(as.vector(df$Exposure)))
        if (is.na(maxChars)) {
          legend_list <- list(x = 100, y = .8)
        } else {
          if (maxChars <= 35) {
            legend_list <- list(x = 100, y = .8)
          } else {
            legend_list <- list(x = .5, y = -.5)
          }
        }

        #myPlotly <- ggplotly(myPlot, tooltip = c("text", "x"))
        myPlotly <- ggplotly(myPlot, width = (0.75*as.numeric(input$dimension[1])), height = as.numeric(input$dimension[2]))
        myPlotly <- plotly:::config(myPlotly, displaylogo = FALSE)
        legend.title <- metadata.file$PROPERTY[metadata.file$SOURCE_ID == var1]
        legend.title <- gsub('(.{1,35})(\\s|$)', '\\1\n', legend.title)
        myPlotly <- add_annotations(myPlotly, text = legend.title, xref="paper",
                                    x=1.02, xanchor = "left",
                                    y=.7, yanchor = "bottom",
                                    legendtitle=TRUE, showarrow=FALSE)
        myPlotly <- layout(myPlotly, margin = list(l = 70, r = 50, b = 200, t = 40),
                                     xaxis = x_list,
                                     yaxis = y_list,
                                     legend = legend_list,
                                     autosize=TRUE)

        myPlotly

    })


