
    output$individualPlot_stp1 <- renderUI({
      if (is.null(input$facetType)) {
        return()
      }
      if (input$facetType == "none") {
        myFacet <- "none"
      } else {
        myFacet <- facetInfo()$group
      }
      facetType <- input$facetType
      
      if (myFacet == "none") {
        return()
      }
      
      #TODO try to save this globally and reactively have it update?? cause right now its called in three different places..
      df <- plotData()
      if (facetType == "makeGroups") {
        myFacet <- "FACET"
      }
      df <- as.data.table(df)
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
      if (is.null(input$facet2Type)) {
        return()
      }
      if (input$facet2Type == "none") {
        myFacet2 <- "none"
      } else {
        myFacet2 <- facet2Info()$group
      }
      facet2Type <- input$facet2Type

      if (myFacet2 == "none") {
        return()
      }

      #TODO try to save this globally and reactively have it update?? cause right now its called in three different places..
      df <- plotData()
      if (facet2Type == "makeGroups") {
        myFacet2 <- "FACET2"
      }
      df <- as.data.table(df)
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
      if (is.null(input$facetType)) {
        return()
      }
      if (is.null(input$facet2Type)) {
        return()
      }
      #if (is.null(input$individualPlot_stp1) & is.null(input$individualPlot_stp2)) {
      #  return()
      #}
      if (input$facetType == "none") {
        myFacet <- "none"
      } else {
        myFacet <- facetInfo()$group
        if (is.null(input$individualPlot_stp1)) {
          return()
        }
      }
      facetType <- input$facetType
      if (input$facet2Type == "none") {
        myFacet2 <- "none"
      } else {
        myFacet2 <- facet2Info()$group
        if (is.null(input$individualPlot_stp2)) {
          return()
        }
      }
      facet2Type <- input$facet2Type
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

      df <- plotData()
      df <- as.data.table(df)

      if (!is.null(iPlot_stp1)) {
        keep <- c(df[, myFacet, with=FALSE] == iPlot_stp1)
        df <- df[keep,]
      }
      if (!is.null(iPlot_stp2)) {
        keep2 <- c(df[, myFacet2, with=FALSE] == iPlot_stp2)
        df <- df[keep2,]
      }

      var1 <- attrInfo()$group
      var2 <- outInfo()$group

      #define axis labels here
      xlab <- metadata.file$PROPERTY[metadata.file$SOURCE_ID == var2]
      ylab <- "Proportion"

      df$Outcome <- gsub(xlab, "", df$Outcome)

      #plot here
      myPlot <- ggplot(data = df, aes(x = Outcome, y = Proportion, fill = Exposure))
      myPlot <- myPlot + theme_bw()
      myPlot <- myPlot + labs(y = "", x = "")

      myPlot <- myPlot + geom_bar(stat = "identity", position = "fill")

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
      myPlotly <- plotly:::config(myPlotly, displaylogo = FALSE, collaborate = FALSE)
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
