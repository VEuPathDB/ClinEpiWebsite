    output$individualPlot_stp1 <- renderUI({
      if (is.null(validateAndDebounceFacet())) {
        return()
      }
      myInputs <- validateAndDebounceFacet()
      myFacet <- myInputs$myFacet
      if (myFacet == "none") {
        return()
      }
      myFacet <- "FACET" 
      #TODO try to save this globally and reactively have it update?? cause right now its called in three different places..
      df <- plotData()
      
      facetVals <- unique(df[,myFacet, with=FALSE])
      facetVals <- unlist(facetVals)
      names(facetVals) <- facetVals
      
      if (is.null(properties)) {
        selectInput(inputId = "individualPlot_stp1",
                    label = "Stratify Plot (1) value:",
                    choices = facetVals)
      } else {
        mySelected <- properties$selected[properties$input == 'input$individualPlot_stp1']
        selectInput(inputId = "individualPlot_stp1",
                    label = "Stratify Plot (1) value:",
                    choices = facetVals,
                    selected = mySelected)
      }
    })

    output$individualPlot_stp2 <- renderUI({
      if (is.null(validateAndDebounceFacet2())) {
        return()
      }
      myInputs <- validateAndDebounceFacet2()
      myFacet2 <- myInputs$myFacet2
      if (myFacet2 == "none") {
        return()
      }
      myFacet2 <- "FACET2"
      
      #TODO try to save this globally and reactively have it update?? cause right now its called in three different places..
      df <- plotData()

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

    output$individual_plot <- renderPlotly({
      if (is.null(validateAndDebounceAxes()) | is.null(validateAndDebounceFacet()) | is.null(validateAndDebounceFacet2())) {
        return()
      }
      myInputs <- c(validateAndDebounceAxes(), validateAndDebounceFacet(), validateAndDebounceFacet2())
      myY <- myInputs$myY
      yaxis_stp1 <- myInputs$yaxis_stp1
      yaxis_stp2 <- myInputs$yaxis_stp2
      yaxis_stp3 <- myInputs$yaxis_stp3
      plotType <- myInputs$yaxis_stp3
      facetType <- myInputs$facetType
      myFacet <- myInputs$myFacet
      facet2Type <- myInputs$facet2Type
      myFacet2 <- myInputs$myFacet2
      longitudinal <- myInputs$longitudinal
      xaxisVar <- myInputs$xaxisVar
      xaxis_bins <- myInputs$xaxis_bins
      if (facetType == "none") {
        myFacet <- "none"
      } else {
        myFacet <- "FACET"
      }
      if (facet2Type == "none") {
        myFacet2 <- "none"
      } else {
        myFacet2 <- "FACET2"
      }

      iPlot_stp1 <- input$individualPlot_stp1
      iPlot_stp2 <- input$individualPlot_stp2

      dates <- getDates(metadata.file)
      nums <- getNums(metadata.file)
      #get data from plotData here
      df <- plotData()

      if (is.null(df)) {
        message("plotData returned null!")
        return()
      }
      numColors <- uniqueN(df$GROUPS)
      groups <- unique(df$GROUPS)

      names(df)[names(df) == 'GROUPS'] <- 'LINES'
      if (!is.null(iPlot_stp1)) {
        keep <- c(df[, myFacet, with=FALSE] == iPlot_stp1)
        df <- df[keep,]
      }
      if (!is.null(iPlot_stp2)) {
        keep2 <- c(df[, myFacet2, with=FALSE] == iPlot_stp2)
        df <- df[keep2,]
      }
      missingGroups <- groups[! groups %in% unique(df$LINES)]
      addMissingGroup <- function(x) {
        df <<- rbind(df, NA, fill=TRUE)
	df$LINES[nrow(df)] <<- x
        return(df)
      }
      lapply(missingGroups, addMissingGroup)

      if (contLongitudinal) {
        #define axis labels here
        xAxisType <- metadata.file$TYPE[metadata.file$SOURCE_ID == longitudinal]
        if (xAxisType == "number") {
          xlab = "Age"
        } else {
          xlab = "Time"
        }

        ylab <- makeGroupLabel(getMyY$val, metadata.file, yaxis_stp1, yaxis_stp2, NULL, NULL, NULL, useGroup = TRUE)[1]
        if (plotType == "proportion") {
          ylab <- paste("Proportion where", ylab)
        } else if (plotType == "count") {
          ylab <- paste("Count where", ylab)
        } else {
          ylab <- paste("Mean where", ylab)
          df$YAXIS <- as.numeric(df$YAXIS)
        }
        ylab <- gsub('(.{1,65})(\\s|$)', '\\1\n', ylab)

        #format xaxis ticks
        if (longitudinal %in% nums$SOURCE_ID) {
          df$XAXIS <- as.numeric(gsub("\\[|\\]", "", sub(".*,", "", df$XAXIS)))
        } else {
          df$XAXIS <- as.factor(df$XAXIS)
          levels(df$XAXIS) <- sort(levels(df$XAXIS))
        }

        #plot here
        myPlot <- ggplot(data = df, aes(x = XAXIS, y = YAXIS, group = LINES,  color = LINES))
        myPlot <- myPlot + theme_bw()
        myPlot <- myPlot + labs(y = "", x = "")
        #add the lines
        if (plotType == "proportion" | plotType == "count") {
          myPlot <- myPlot + geom_point()
          myPlot <- myPlot + geom_line(size = 1)
        } else if (plotType == "mean") {
          myPlot <- myPlot + stat_summary(fun.data = function(x){c( "y" = median(x, na.rm = TRUE), "ymax" = max(x, na.rm = TRUE), "ymin" = min(x, na.rm = TRUE))})
          myPlot <- myPlot + stat_summary(fun.y=function(x){y <- quantile(x, .25, na.rm = TRUE)})
          myPlot <- myPlot + stat_summary(fun.y=function(x){y <- quantile(x, .75, na.rm = TRUE)})
          myPlot <- myPlot + stat_summary(fun.y = mean, geom="line", size = 1)
        } else {
          myPlot <- myPlot + stat_summary(fun.data = function(x){c( "y" = median(x, na.rm = TRUE), "ymax" = max(x, na.rm = TRUE), "ymin" = min(x, na.rm = TRUE))})
          myPlot <- myPlot + stat_summary(fun.y=function(x){y <- quantile(x, .25, na.rm = TRUE)})
          myPlot <- myPlot + stat_summary(fun.y=function(x){y <- quantile(x, .75, na.rm = TRUE)})
          myPlot <- myPlot + geom_smooth(span = .3, na.rm = TRUE)
        }

        maxChars <- max(nchar(as.vector(df$LINES)))

        #find num colors needed
        if (numColors > 2) {
          myPlot <- myPlot + scale_color_manual(name = "", values = viridis(numColors))
        } else if (numColors == 2) {
          myPlot <- myPlot + scale_color_manual(name = "", values = viridis(numColors, begin = .25, end = .75))
        } else {
          myPlot <- myPlot + scale_color_manual(name = "", values = viridis(numColors, begin = .5))
        }

        if (!longitudinal %in% nums$SOURCE_ID) {
          myPlot <- myPlot + theme(axis.text.x = element_text(angle = 45, hjust = 1))
        }

      } else {
	names(df)[names(df) == 'LINES'] <- 'XAXIS'

        # if y axis is numeric box plots otherwise bar pltos.
        #define axis labels here
        xlab <- ""
        
        #test if numeric, if yes then "Mean" else proportion if vals between 0 and 1 otherwise "Count"
        ylab <- makeGroupLabel(getMyY$val, metadata.file, yaxis_stp1, yaxis_stp2, NULL, NULL, NULL, useGroup = TRUE)[1]
        if (plotType == "proportion") {
          ylab <- paste("Proportion where", ylab)
        } else if (plotType == "count") {
          ylab <- paste("Count where", ylab)
        } else {
          ylab <- paste("Mean where", ylab)
          df$YAXIS <- as.numeric(df$YAXIS)
        }
        ylab <- gsub('(.{1,45})(\\s|$)', '\\1\n', ylab)
        df$XAXIS <- as.factor(df$XAXIS)
        #plot here
        myPlot <- ggplot(data = df, aes(x = XAXIS, y = YAXIS, fill = XAXIS))
        myPlot <- myPlot + theme_bw()
        myPlot <- myPlot + labs(y = "", x = "")
        #add the lines
        if (plotType == "proportion") {
          myPlot <- myPlot + geom_bar(stat = "identity")
          myPlot <- myPlot + scale_y_continuous(limits = c(0,1))
        } else if (plotType == "count") {
          myPlot <- myPlot + geom_bar(stat = "identity")
        } else {
          myPlot <- myPlot + geom_boxplot()
        }

        maxChars <- max(nchar(as.vector(df$XAXIS)))

        #find num colors needed
        if (numColors > 2) {
          myPlot <- myPlot + scale_fill_manual(name = "", values = viridis(numColors))
        } else if (numColors == 2) {

          myPlot <- myPlot + scale_fill_manual(name = "", values = viridis(numColors, begin = .25, end = .75))
        } else {

          myPlot <- myPlot + scale_fill_manual(name = "", values = viridis(numColors, begin = .5))
        }

      }

      #should keep playing with this vs doing it with ggplot syntax. 
      x_list <- list(
        title = xlab,
        size = 14,
	automargin = TRUE
      )
      y_list <- list(
        title = ylab,
        size = 14,
	automargin = TRUE
      )

      if (is.na(maxChars)) {
        legend_list <- list(x=100, y=.5)
      } else {
        if (maxChars > 35) {
          legend_list <- list(x = .5, y = -.8)
        } else {
          legend_list <- list(x=100, y=.5)
        }
      }

      myPlotly <- ggplotly(myPlot, tooltip = c("text", "x", "y"), width = (0.75*as.numeric(input$dimension[1])), height = as.numeric(input$dimension[2]))
      if (is.null(legendTitle)) {
        legend.title <- "All"
      } else {
        legend.title <- metadata.file$PROPERTY[metadata.file$SOURCE_ID == legendTitle]
        legend.title <- gsub('(.{1,15})(\\s|$)', '\\1\n', legend.title)
      }
      #myPlotly <- add_annotations(myPlotly, text = legend.title, xref="paper",
      #                            x=1.02, xanchor = "left",
      #                            y=.3, yanchor = "bottom",
      #                            legendtitle=TRUE, showarrow=FALSE)
      myPlotly <- plotly:::config(myPlotly, displaylogo = FALSE, editable = TRUE, edits = list(shapePosition = FALSE))
      myPlotly <- layout(myPlotly, margin = list(l = 150, r = 50, b = 150, t = 40),
                         xaxis = x_list,
                         yaxis = y_list,
                         legend = legend_list,
                         autosize=TRUE)

      myPlotly

    })

