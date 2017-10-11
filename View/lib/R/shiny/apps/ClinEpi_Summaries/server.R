## server.r

require(shiny)
require(data.table)
#require(plotly)
require(ggplot2)
require(DT)
#require(viridisLite)

source("../../lib/wdkDataset.R")
source("config.R")
source("functions.R")

#are there any stats evidence for significance we could do?
#if there is no timeframe info (agedays or dates) consider a box plot instead. make sure that idea isnt redundant with any other apps or anything, and makes sense to do
#for facets ui: consider selecting more than one group/ check boxes ??
#make sure levels for numeric groups always start with the group that has square bracket in front (meaning smallest -> largest)
#bug where reactive context doesnt respond immediately (tableData never returns) and have to change something in ui before anything happens
#figure out when we need naToZero function when building own groups and facets. imagine need it for events stuffs but not others ??
# for some reason delta laz > -.5 returns fewer ppl than >-1.. seems should be reversed

options(shiny.sanitize.errors = TRUE)  

shinyServer(function(input, output, session) {
  
  event.file <- NULL
  prtcpnt.file <- NULL
  house.file <- NULL
  metadata.file <- NULL
  singleVarData <- NULL
 
  filesFetcher <- reactive({
  
   if (is.null(prtcpnt.file)) {
 
      prtcpnt.file <<- fread(
          getWdkDatasetFile('ShinyParticipants.tab', session, FALSE, dataStorageDir),
          na.strings = "N/A")

      house.file <<- fread(
          getWdkDatasetFile('ShinyHouseholds.tab', session, FALSE, dataStorageDir),
          na.strings = "N/A")

      event.file <<- fread(
          getWdkDatasetFile('ShinyEvents.tab', session, FALSE, dataStorageDir),
          na.strings = "N/A")

      metadata.file <<- fread(
          getWdkDatasetFile('ontologyMetadata.tab', session, FALSE, dataStorageDir),
          )

      #prtcpnt.file <<- fread("ShinyParticipants_gates2.tab", na.strings = "N/A")
      #house.file <<- fread("ShinyHouseholds_gates2.tab", na.strings = "N/A")
      #event.file <<- fread("ShinyEvents_gates2.tab", na.strings = "N/A")
      #metadata.file <<- fread("ontologyMetadata_gates2.tab")
  
      #make internal column names without brackets or spaces
      names(event.file) <<-  gsub(" ", "_", gsub("\\[|\\]", "", names(event.file)))
      names(prtcpnt.file) <<-  gsub(" ", "_", gsub("\\[|\\]", "", names(prtcpnt.file)))
      names(house.file) <<-  gsub(" ", "_", gsub("\\[|\\]", "", names(house.file)))
      names(metadata.file) <<- gsub(" ", "_", tolower(gsub("\\[|\\]", "", names(metadata.file))))
      
      setkey(prtcpnt.file, Participant_Id)
      setkey(event.file, Participant_Id)
      setkey(house.file, Participant_Id)
      setkey(metadata.file, source_id)
    }
  })
  
  singleVarDataFetcher <- function(){
    filesFetcher()
    
    #remove non-unique column names and merge them to one data table to return
    drop <- c("source_id", "project_id")
    prtcpnt.file <<- prtcpnt.file[, (drop):=NULL]
    house.file <<- house.file[, (drop):=NULL]
    merge1 <- merge(event.file, prtcpnt.file)
    singleVarData <<- merge(merge1, house.file)
    if (any(colnames(singleVarData) %in% "EUPATH_0000644")) {
      setkey(singleVarData, EUPATH_0000644)
    }
    
  }
  
  subsetDataFetcher <- function(min,max){
    data <- singleVarData
    
    #eupath_0000743 is last date observed. so the below doesnt include prtcpnts who drop out before the max day set
    #this inner if statement temporary (hopefully) until we sort the root of the problem. events table has data up to ageday 745, but prtcpnt file says no observations past 732
    if (any(colnames(prtcpnt.file) %in% "EUPATH_0000743")) {
      grabMe <- length(levels(as.factor(prtcpnt.file$EUPATH_0000743)))
      maxDay <- levels(as.factor(prtcpnt.file$EUPATH_0000743))[grabMe]
      if (max > maxDay) {
        tempDF <- data[data$EUPATH_0000644 >= min & data$EUPATH_0000644 <= max]
      } else {
        tempDF <- data[data$EUPATH_0000644 >= min & data$EUPATH_0000644 <= max & data$EUPATH_0000743 >= max]
      }
    } else {
      tempDF <- data
    }
    
    #maybe make it so that if it cant find these two columns then it doesn't let there be a subset/ timeframe option at all but will continue without it
    #can even have some else if options if there are backups that are likely to exist
    
    tempDF
  }
  
  outRange <- reactiveValues()
  facetRange <- reactiveValues()
  
  setOutVals <- reactive({
    myGroups <- input$groups
    nums <- getNums()
    
    if (myGroups %in% nums$source_id) {
      data <- singleVarData
      tempDF <- completeDT(data, myGroups)
      
      if (any(colnames(event.file) %in% myGroups) & any(colnames(tempDF) %in% "BFO_0000015")) {
        if (levels(as.factor(tempDF$BFO_0000015)) == "Diarrhea Episode") {
          outRange$myMin = 0
        }
      } else {
        outRange$myMin <- min(tempDF[, (myGroups), with=FALSE])
      }
      outRange$myMax <- max(tempDF[, (myGroups), with=FALSE])
      
      outRange$mean <- mean(tempDF[[myGroups]])
    }
    
  })
  
  setFacetVals <- reactive({
    myFacet <- input$facet
    nums <- getNums()
    
    if (myFacet %in% nums$source_id) {
      data <- singleVarData
      tempDF <- completeDT(data, myFacet)
      
      if (any(colnames(event.file) %in% myFacet) & any(colnames(tempDF) %in% "BFO_0000015")) {
        if (levels(as.factor(tempDF$BFO_0000015)) == "Diarrhea Episode") {
          facetRange$myMin = 0
        }
      } else {
        facetRange$myMin <- min(tempDF[, (myFacet), with=FALSE])
      }
      facetRange$myMax <- max(tempDF[, (myFacet), with=FALSE])
      
      facetRange$mean <- mean(tempDF[[myFacet]])
    }
    
  })
  
  #make sure ranges are updated when attr or out change
  observeEvent(input$groups, setOutVals())
  observeEvent(input$facet, setFacetVals())
  
  #ui stuffs
    output$choose_timeframe <- renderUI({
      ageDays = "EUPATH_0000644"
    
      if (!length(singleVarData)) {
        data <- singleVarDataFetcher()
      } else {
        data <- singleVarData
      }
      
      if (any(colnames(data) %in% ageDays)) {
        tempDF <- completeDT(data, ageDays)
        
        myMin <- min(tempDF[, (ageDays), with=FALSE])
        myMax <- max(tempDF[, (ageDays), with=FALSE])
        
        sliderInput("timeframe", "Timeframe:",
                    min = myMin, max = myMax, value = c(myMin,myMax), round=TRUE, width = '100%')
      }
        
    })
    
    output$groups_type <- renderUI({
      selectInput(inputId = "groupsType",
                  label = "Groups:",
                  choices = c("All possible" = "direct", "Make my own" = "makeGroups"),
                  selected = "direct",
                  width = '100%')
    })
    
    output$facet_type <- renderUI({
      selectInput(inputId = "facetType",
                  label = "Facet:",
                  choices = c("All possible" = "direct", "Make my own" = "makeGroups", "None" = "none"),
                  selected = "none",
                  width = '100%')
    })
    
    output$choose_groups <- renderUI({
      groupsType <- req(input$groupsType)
      
      if (groupsType == "direct") {
        groupChoiceList <- getGroupList()
        selectInput(inputId = "groups",
                    label = "groups for",
                    choices = groupChoiceList,
                    selected = "EUPATH_0000744",
                    width = '100%')
      } else {
        groupChoiceList <- getMakeGroupList()
        selectInput(inputId = "groups",
                    label = "group where:",
                    choices = groupChoiceList,
                    selected = "EUPATH_0000704",
                    width = '100%')
      }
      
    })
    
    output$choose_facet <- renderUI({
      facetType <- req(input$facetType)
      
      if (facetType == "direct") {
        facetChoiceList <- getFacetList()
        selectInput(inputId = "facet",
                    label = "facets for",
                    choices = facetChoiceList,
                    selected = "EUPATH_0000452",
                    width = '100%')
      } else if (facetType != "none") {
        #will have to change selected here TODO
        facetChoiceList <- getMakeGroupList()
        selectInput(inputId = "facet",
                    label = "facets where:",
                    choices = facetChoiceList,
                    selected = "EUPATH_0000452",
                    width = '100%')
      }
      
    })
    
    output$choose_yaxis <- renderUI({
      
      #this list should contain anything from events file
      outChoiceList <- getOutList()
      selectInput(inputId = "yaxis",
                  label = "Y-Axis:",
                  choices = outChoiceList,
                  selected = "EUPATH_0000689",
                  width = '100%')
    })
    
    output$yaxis_stp1 <- renderUI({
      if (is.null(input$yaxis)) {
        return()
      }
      myY <- input$yaxis
      attrStp1List <- getAttrStp1List(myY)
    
      if (myY == "EUPATH_0000704") {
        selectInput(inputId = "yaxis_stp1",
                    label = "for",
                    choices = attrStp1List,
                    width = '100%') 
      }
      
    })
    
    output$yaxis_stp2 <- renderUI({
      myY <- req(input$yaxis)
      nums <- getNums()
      
      if (myY %in% nums$source_id) {
        radioButtons(inputId = "yaxis_stp2",
                     label = "Display as:",
                     choices = list("Mean" = "mean", "Smoothed Conditional Mean" = "smooth"),
                     selected = "smooth",
                     width = '100%',
                     inline = TRUE)
      } else {
        if (is.null(input$yaxis_stp1)) {
          return()
        }
        radioButtons(inputId = "yaxis_stp2",
                     label = "Display as:",
                     choices = list("Count" = "count", "Proportion" = "proportion"),
                     selected = "proportion",
                     width = '100%',
                     inline = TRUE)
      }
      
    })
    
    output$groups_stp1 <- renderUI({
      if (is.null(input$groupsType)) {
        return()
      } else {
        if (input$groupsType == "direct") {
          return()
        }
      }
      if (is.null(input$groups)) {
        return()
      }
      
      myGroups <- input$groups
      nums <- getNums()
      
      data <- singleVarData
      tempDF <- completeDT(data, myGroups)
     
      if (any(colnames(event.file) %in% myGroups) & any(colnames(tempDF) %in% "BFO_0000015")) {
        if (levels(as.factor(tempDF$BFO_0000015)) == "Anthropometry") {
          selectInput(inputId = "groups_stp1",
                      label = "where",
                      choices = list('the change in value over time selected' = 'delta', 'more than the following percent of days' = 'percentDays', "a direct comparison" = "direct"),
                      selected = "delta",
                      width = '100%') 
        } else {
          if (myGroups %in% nums$source_id) {
            selectInput(inputId = "groups_stp1",
                        label = "is",
                        choices = list('<' = 'lessThan', '>' = 'greaterThan', '=' = 'equals'),
                        selected = "greaterThan",
                        width = '100%')
          } else {
            outStp1List <- getAttrStp1List(myGroups)
            if (length(outStp1List) == 2) {
              if (any(outStp1List %in% "Yes")) {
                selectInput(inputId = "groups_stp1",
                            label = "is",
                            choices = outStp1List,
                            selected = "Yes",
                            width = '100%')
              } else if (any(outStp1List %in% "TRUE")) {
                selectInput(inputId = "groups_stp1",
                            label = "is",
                            choices = outStp1List,
                            selected = "TRUE",
                            width = '100%')
              }
            } else {
              selectInput(inputId = "groups_stp1",
                          label = "is",
                          choices = outStp1List,
                          width = '100%')
            }
          }
        }
     } else {
        if (myGroups %in% nums$source_id) {
          selectInput(inputId = "groups_stp1",
                      label = "is",
                      choices = list('<' = 'lessThan', '>' = 'greaterThan', '=' = 'equals'),
                      selected = "greaterThan",
                      width = '100%')
        } else {
          outStp1List <- getAttrStp1List(myGroups)
          if (length(outStp1List) == 2) {
            if (any(outStp1List %in% "Yes")) {
              selectInput(inputId = "groups_stp1",
                          label = NULL,
                          choices = outStp1List,
                          selected = "Yes",
                          width = '100%')
            } else if (any(outStp1List %in% "TRUE")) {
              selectInput(inputId = "groups_stp1",
                          label = NULL,
                          choices = outStp1List,
                          selected = "TRUE",
                          width = '100%')
            }
          } else {
            selectInput(inputId = "groups_stp1",
                        label = NULL,
                        choices = outStp1List,
                        width = '100%')
          }
        }
      }
      
    })
    
    output$facet_stp1 <- renderUI({
      if (is.null(input$facetType)) {
        return()
      } else {
        if (input$facetType != "makeGroups") {
          return()
        }
      }
      if (is.null(input$facet)) {
        return()
      }
      
      myFacet <- input$facet
      nums <- getNums()
      
      data <- singleVarData
      tempDF <- completeDT(data, myFacet)
      
      if (any(colnames(event.file) %in% myFacet) & any(colnames(tempDF) %in% "BFO_0000015")) {
        if (levels(as.factor(tempDF$BFO_0000015)) == "Anthropometry") {
          selectInput(inputId = "facet_stp1",
                      label = "where",
                      choices = list('the change in value over time selected' = 'delta', 'more than the following percent of days' = 'percentDays', "a direct comparison" = "direct"),
                      selected = "delta",
                      width = '100%') 
        } else {
          if (myFacet %in% nums$source_id) {
            selectInput(inputId = "facet_stp1",
                        label = "is",
                        choices = list('<' = 'lessThan', '>' = 'greaterThan', '=' = 'equals'),
                        selected = "greaterThan",
                        width = '100%')
          } else {
            outStp1List <- getAttrStp1List(myFacet)
            if (length(outStp1List) == 2) {
              if (any(outStp1List %in% "Yes")) {
                selectInput(inputId = "facet_stp1",
                            label = "is",
                            choices = outStp1List,
                            selected = "Yes",
                            width = '100%')
              } else if (any(outStp1List %in% "TRUE")) {
                selectInput(inputId = "facet_stp1",
                            label = "is",
                            choices = outStp1List,
                            selected = "TRUE",
                            width = '100%')
              }
            } else {
              selectInput(inputId = "facet_stp1",
                          label = "is",
                          choices = outStp1List,
                          width = '100%')
            }
          }
        }
      } else {
        if (myFacet %in% nums$source_id) {
          selectInput(inputId = "facet_stp1",
                      label = "is",
                      choices = list('<' = 'lessThan', '>' = 'greaterThan', '=' = 'equals'),
                      selected = "greaterThan",
                      width = '100%')
        } else {
          outStp1List <- getAttrStp1List(myFacet)
          if (length(outStp1List) == 2) {
            if (any(outStp1List %in% "Yes")) {
              selectInput(inputId = "facet_stp1",
                          label = NULL,
                          choices = outStp1List,
                          selected = "Yes",
                          width = '100%')
            } else if (any(outStp1List %in% "TRUE")) {
              selectInput(inputId = "facet_stp1",
                          label = NULL,
                          choices = outStp1List,
                          selected = "TRUE",
                          width = '100%')
            }
          } else {
            selectInput(inputId = "facet_stp1",
                        label = NULL,
                        choices = outStp1List,
                        width = '100%')
          }
        }
      }
      
    })
 
    output$groups_stp2 <- renderUI({
      if (is.null(input$groups_stp1)) {
        return()
      }
      myStp1Val <- input$groups_stp1
      
      numeric <- c("lessThan", "greaterThan", "equals")
      anthro <- c("percentDays", "delta", "direct")
      
      if (myStp1Val %in% anthro) {
        if (myStp1Val == 'percentDays') {
          numericInput(inputId = "groups_stp2",
                       label = NULL,
                       value = 50,
                       min = 0,
                       max = 100,
                       width = '100%')
        } else {
          selectInput(inputId = "groups_stp2",
                      label = "is",
                      choices = list('<' = 'lessThan', '>' = 'greaterThan', '=' = 'equals'),
                      selected = "greaterThan",
                      width = '100%')
        }
      } else {
        if (myStp1Val %in% numeric) {
          #just going to set default value to whatever the mean is 
          sliderInput("groups_stp2", NULL,
                      min = outRange$myMin, max = outRange$myMax, value = outRange$mean, step = .1, width = '100%')
        } 
      }
      
    })
    
    output$facet_stp2 <- renderUI({
      if (is.null(input$facet_stp1)) {
        return()
      }
      if (is.null(input$facetType) | input$facetType != "makeGroups") {
        return()
      }
      myStp1Val <- input$facet_stp1
      
      numeric <- c("lessThan", "greaterThan", "equals")
      anthro <- c("percentDays", "delta", "direct")
      
      if (myStp1Val %in% anthro) {
        if (myStp1Val == 'percentDays') {
          numericInput(inputId = "facet_stp2",
                       label = NULL,
                       value = 50,
                       min = 0,
                       max = 100,
                       width = '100%')
        } else {
          selectInput(inputId = "facet_stp2",
                      label = "is",
                      choices = list('<' = 'lessThan', '>' = 'greaterThan', '=' = 'equals'),
                      selected = "greaterThan",
                      width = '100%')
        }
      } else {
        if (myStp1Val %in% numeric) {
          #just going to set default value to whatever the mean is 
          sliderInput("facet_stp2", NULL,
                      min = facetRange$myMin, max = facetRange$myMax, value = facetRange$mean, step = .1, width = '100%')
        } 
      }
      
    })
    
    output$groups_stp3 <- renderUI ({
      if (is.null(input$groups_stp1)) {
        return()
      }
      myStp1Val <- input$groups_stp1
    
      anthro <- c("delta", "direct", "percentDays")
      
      if (myStp1Val %in% anthro) {
        if (myStp1Val == "percentDays") {
          selectInput(inputId = "groups_stp3",
                      label = "are",
                      choices = list('<' = 'lessThan', '>' = 'greaterThan', '=' = 'equals'),
                      selected = "greaterThan",
                      width = '100%')
        } else if (myStp1Val == "direct") {
          sliderInput("groups_stp3", NULL,
                      min = outRange$myMin, max = outRange$myMax, value = outRange$mean, step = .1, width='100%')
        } else {
          sliderInput("groups_stp3", NULL,
                      min = -20, max = 20, value = -1, step = .1, width = '100%')
        }
        
      }
      
    })
    
    output$facet_stp3 <- renderUI ({
      if (is.null(input$facet_stp1)) {
        return()
      }
      if (is.null(input$facetType) | input$facetType != "makeGroups") {
        return()
      }
      myStp1Val <- input$facet_stp1
      
      anthro <- c("delta", "direct", "percentDays")
      
      if (myStp1Val %in% anthro) {
        if (myStp1Val == "percentDays") {
          selectInput(inputId = "facet_stp3",
                      label = "are",
                      choices = list('<' = 'lessThan', '>' = 'greaterThan', '=' = 'equals'),
                      selected = "greaterThan",
                      width = '100%')
        } else if (myStp1Val == "direct") {
          sliderInput("facet_stp3", NULL,
                      min = facetRange$myMin, max = facetRange$myMax, value = facetRange$mean, step = .1, width='100%')
        } else {
          sliderInput("facet_stp3", NULL,
                      min = -20, max = 20, value = -1, step = .1, width = '100%')
        }
        
      }
      
    })
      
    output$groups_stp4 <- renderUI({
      if (is.null(input$groups_stp1)) {
        return()
      }
      myStp1Val <- input$groups_stp1
      
      if (myStp1Val == "percentDays") {
        sliderInput("groups_stp4", NULL,
                    min = outRange$myMin, max = outRange$myMax, value = outRange$mean, step = .1, width='100%')
      }
    })
    
    output$facet_stp4 <- renderUI({
      if (is.null(input$facet_stp1)) {
        return()
      }
      if (is.null(input$facetType) | input$facetType != "makeGroups") {
        return()
      }
      myStp1Val <- input$facet_stp1
      
      if (myStp1Val == "percentDays") {
        sliderInput("facet_stp4", NULL,
                    min = facetRange$myMin, max = facetRange$myMax, value = facetRange$mean, step = .1, width='100%')
      }
    })
    
    output$plot <- renderPlot({
        plotType <- req(input$yaxis_stp2)     
 
        #get data from plotData here
        df <- plotData()
      
        if (is.null(df)) {
          message("plotData returned null!")
          return()
        }
        
        #define axis labels here
        xlab <- "Time"
        #test if numeric, if yes then "Mean" else proportion if vals between 0 and 1 otherwise "Count"
          if (plotType == "proportion") {
            ylab <- "Proportion"
          } else if (plotType == "count") {
            ylab <- "Count"
          } else if (plotType == "mean") {
            ylab <- "Mean"
          } else {
            ylab <- "Mean"
          }

        message(paste("plotType:", plotType))       
 
        #format xaxis ticks 
        df$XAXIS <- as.numeric(gsub("\\[|\\]", "",sub(".*,", "", df$XAXIS)))
        #df$XAXIS <- as.vector(df$XAXIS)
        #plot here
        myPlot <- ggplot(data = df, aes(x = XAXIS, y = YAXIS, color = GROUPS, group = GROUPS, text = paste0("GROUP: ", GROUPS, "\n", "25th PERCENTILE: ", quantile(XAXIS, .25, na.rm = TRUE))))
        myPlot <- myPlot + theme_bw()
        myPlot <- myPlot + labs(y = ylab, x = xlab)
        #add the lines
        if (plotType != "smooth") {
          myPlot <- myPlot + geom_line(size = 1.5)
        } else {
          #myPlot <- myPlot + geom_point()
          myPlot <- myPlot + stat_summary(fun.data = function(x){c( "y" = median(x, na.rm = TRUE), "ymax" = max(x, na.rm = TRUE), "ymin" = min(x, na.rm = TRUE))})
          myPlot <- myPlot + stat_summary(fun.y=function(x){y <- quantile(x, .25, na.rm = TRUE)})
          myPlot <- myPlot + stat_summary(fun.y=function(x){y <- quantile(x, .75, na.rm = TRUE)})
          #myPlot <- myPlot + quantile()
          myPlot <- myPlot + geom_smooth(span = .3, na.rm = TRUE)
        }
        
        #add facet if available
        if (any(colnames(df) %in% "FACET")) {
          myPlot <- myPlot + facet_wrap(~ FACET, ncol = 1)
        }
        
        #find num colors needed
        numColors <- length(levels(as.factor(df$GROUPS)))
        #myPlot <- myPlot + scale_color_manual(values = viridis(numColors))
        
        #should keep playing with this vs doing it with ggplot syntax. 
        x_list <- list(
          title = xlab,
          size = 14 
        )
        y_list <- list(
          title = ylab,
          size = 14
        )
        
        #myPlotly <- ggplotly(myPlot, tooltip = c("text", "x", "y"))
        #myPlotly <- ggplotly(myPlot)
        #myPlotly <- config(myPlotly, displaylogo = FALSE, collaborate = FALSE) %>% layout(xaxis = x_list, yaxis = y_list)
        
        myPlot
      
    })
    
    output$table <- DT::renderDataTable({
      data <- tableData()
      if (is.null(data)) {
        return()
      } 
      
      #and still need to remove duplicates across time.
      if (any(colnames(data) %in% "FACET")) {
        data <- reshape(aggregate(Participant_Id ~ FACET + GROUPS, data, FUN = function(x){ length(unique(x)) } ), 
                        timevar = "FACET", idvar = "GROUPS", v.names = "Participant_Id", direction = "wide")
        colnames(data)[1] <- "Group"
        colnames(data) <- gsub("Participant_Id.", "# Participants: ", colnames(data))
        #give totals
        data[, "Totals"] <- rowSums(data[, -1], na.rm=TRUE)
        rownames(data) <- data[,1]
        data[,1] <- NULL
        data["Totals" ,] <- colSums(data, na.rm=TRUE)
        data <- cbind("Group" = rownames(data), data)
      } else {
        data <- aggregate(Participant_Id ~ GROUPS, data, FUN = function(x){ length(unique(x)) } )
        colnames(data) <- c("Group", "# Participants")
        #totals
        levels(data$Group) <- c(levels(as.factor(data$Group)),"Totals")
        data <- rbind(data, c("Totals", sum(data[, -1])))
      }
      
      #fix custom groups if necessary
      if (all(unique(data$Group[-nrow(data)]) %in% c(1,0))) {
        data <- as.data.table(data)
        data <- transform(data, "Group" = ifelse(Group == 1, "Positive", "Negative"))
        #data$Group <- data$temp
        #data <- data[, -"temp"]
        data$Group[nrow(data)] <- "Totals"
      }
      
      datatable(data, 
                width = '100%',
                rownames = FALSE
      )
    })

    
    #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    #values grabbed through reactive functions for better control of reactive context
  
    #all the work will be done here in prepping data
    tableData <- debounce(reactive({
      
      #collecting inputs 
      myTimeframe <- input$timeframe
      groupsType <- input$groupsType
      facetType <- input$facetType
      myFacet <- input$facet
      myY <- input$yaxis
      yaxis_stp2 <- req(input$yaxis_stp2)
      myGroups <- input$groups
      #grab optional inputs
      groups_stp1 <- input$groups_stp1
      groups_stp3 <- input$groups_stp3
      groups_stp4 <- input$groups_stp4
      groups_stp2 <- input$groups_stp2
      facet_stp1 <- input$facet_stp1
      facet_stp3 <- input$facet_stp3
      facet_stp4 <- input$facet_stp4
      facet_stp2 <- input$facet_stp2
      yaxis_stp1 <- input$yaxis_stp1
      #subset data
      #which cols can be used for this will have to change. too specific right now
      if (any(colnames(singleVarData) %in% "EUPATH_0000644")) {
        if (!is.null(myTimeframe)) {
          data <- subsetDataFetcher(myTimeframe[1], myTimeframe[2])
        } else {
          return()
        }
      } else {
        data <- singleVarData
      }
  
      strings <- subset(metadata.file, metadata.file$type == "string", "source_id")
      go <- TRUE
      
      if (is.null(groupsType)) {
        go <- FALSE
      }
      if (is.null(facetType)) {
        go <- FALSE
      }
      if (is.null(myY)) {
        go <- FALSE
      }
      if (is.null(yaxis_stp2)) {
        go <- FALSE
      }
      if (is.null(myGroups)) {    
        go <- FALSE
      }
      if (myY %in% strings$source_id) {
        if (is.null(yaxis_stp1)) {
          go <- FALSE
        }
      }
      if (groupsType == "makeGroups") {
        if (is.null(groups_stp1)) {
          go <- FALSE
        }
      } 
      if (facetType != "none") {
        if (is.null(myFacet)) {
          go <- FALSE
        }
      }
      
      #once last field is populated .. GO
      if (go) {
        #may not need to do the splitting on pipes. grepl will still return true for it.
        #should have natozero before this for non-anthro events?? so an NA for diarrhea -> 0 ?? 
        plotData <- completeDT(data, myY)
        plotData <- getFinalDT(plotData, myY)
        
        myCols <- c("Participant_Id", myY, "EUPATH_0000644")
        tempData <- plotData[, myCols, with=FALSE] 
        colnames(tempData) <- c("Participant_Id", "YAXIS", "XAXIS")
        
        if (groupsType == "direct") {
          #check which column to pull for time, for now written in as EUPATH_0000644
          myCols <- c("Participant_Id", myGroups)
          groupData <- plotData[, myCols, with=FALSE]
          groupData <- unique(groupData)
          colnames(groupData) <- c("Participant_Id", "GROUPS")
          tempData <- merge(tempData, groupData, by = "Participant_Id")
        } 
        
        if (facetType == "direct") {
          #check which column to pull for time, for now written in as EUPATH_0000644
          myCols <- c("Participant_Id", myFacet)
          facetData <- plotData[, myCols, with=FALSE]
          facetData <- unique(facetData)
          colnames(facetData) <- c("Participant_Id", "FACET")
          tempData <- merge(tempData, facetData, by = "Participant_Id")
        } 
        
        plotData <- tempData
        
        #bin xaxis into 24?? bins.. play with the number
        #consider different methods of binning also, like variable number of bins with consistent amount of days/time in them.
        plotData$XAXIS <- cut(plotData$XAXIS, 24) 
        #bin facet if numeric and same for group 
        nums <- getNums()
        if (any(colnames(plotData) %in% "FACET")) {
          if (myFacet %in% nums$source_id) {
            plotData$FACET <- ggplot2::cut_number(plotData$FACET, 3)
          }
        } else {
          numeric <- c("lessThan", "greaterThan", "equals")
          anthro <- c("percentDays", "delta", "direct")
          if (facetType != "none") {
            if (is.null(facet_stp1)) {
              return()
            } else {
              if (facet_stp1 %in% numeric) {
                if (is.null(facet_stp2)) {
                  return()
                }
              }
              if (facet_stp1 %in% anthro) {
                if (facet_stp1 == "percentDays") {
                  if (is.null(facet_stp4)) {
                    return()
                  }
                } else {
                  if (is.null(facet_stp3)) {
                    return()
                  }
                }
              }
            }
            outData <- makeGroups(data, myFacet, facet_stp1, facet_stp2, facet_stp3, facet_stp4)
            label <- makeGroupLabel(myFacet, facet_stp1, facet_stp2, facet_stp3, facet_stp4)
            print(head(outData))
            #add makeGroups data to df and return
            colnames(outData) <- c("Participant_Id", "FACET")
            #will need a var called label that changes based on what the facet steps are. the below only works for strings.
            if (any(colnames(event.file) %in% myFacet)) {
              naToZero(outData, "FACET")
            }
            print(head(outData))
            outData <- transform(outData, "FACET" = ifelse(as.numeric(FACET) == 0, "Other", label))
           # outData$FACET <- factor(outData$FACET, levels(c("Other", facet_stp2)))
            print(head(outData))
            plotData <- merge(plotData, outData, by = "Participant_Id", all = TRUE)
            print(head(plotData))
          }
        }
        
        #if groups col exists return
        if (any(colnames(plotData) %in% "GROUPS")) {
          if (myGroups %in% nums$source_id) {
            plotData$GROUPS <- ggplot2::cut_number(plotData$GROUPS, 4)
          }
        } else {
          numeric <- c("lessThan", "greaterThan", "equals")
          anthro <- c("percentDays", "delta", "direct")
          if (groupsType != "none") {
            if (is.null(groups_stp1)) {
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
          }
          outData <- makeGroups(data, myGroups, groups_stp1, groups_stp2, groups_stp3, groups_stp4)
          label <- makeGroupLabel(myGroups, groups_stp1, groups_stp2, groups_stp3, groups_stp4)
          if (any(colnames(event.file) %in% myGroups)) {
            naToZero(plotData, "GROUPS")
          }
          #add makeGroups data to df and return
          print(head(outData))
          outData <- transform(outData, "GROUPS" = ifelse(as.numeric(GROUPS) == 0, "Other", label))
          plotData <- merge(plotData, outData, by = "Participant_Id", all = TRUE)
          print(head(plotData))
          print("NA in groups:", any(is.na(plotData$GROUPS)))
        }
    
        return(plotData)
      }
      
      #debounce will wait 2s with no changes to inputs before plotting.
    }), 2000)
      
    plotData <- reactive({  
        plotData <- tableData()
        if (is.null(plotData)) {
          return()
        } else {
          #collecting inputs .. i think these are the only ones i need here.. well see
          myY <- input$yaxis
          yaxis_stp2 <- req(input$yaxis_stp2)
          yaxis_stp1 <- input$yaxis_stp1
          
          strings <- subset(metadata.file, metadata.file$type == "string", "source_id")
        
          #before aggregation happens make sure no reporting is negative reporting for events. 
          #this logic may change with dcc data
          #also consider if we want to do this for anthro stuffs. seems like maybe no. 
          #if (any(colnames(event.file) %in% myY)) {
          #  naToZero(plotData, YAXIS)
          #}
        
        #prepare for return
        if (any(colnames(plotData) %in% "FACET")) {
          if (myY %in% strings$source_id) {
            #will have to replace all instances of myY with 1 and all else with 0 before can sum
            plotData <- transform(plotData, "YAXIS" = ifelse(YAXIS == yaxis_stp1, 1, 0))
            #plotData$YAXIS <- plotData$temp
            #plotData <- plotData[, -"temp"]
            mergeData <- aggregate(YAXIS ~ GROUPS + XAXIS + FACET, plotData, sum)
            if (yaxis_stp2 == "proportion") {
              groupSum <- as.data.table(aggregate(Participant_Id ~ GROUPS + FACET, plotData, FUN = function(x){length(unique(x))}))
              colnames(groupSum) <- c("GROUPS", "FACET", "SUM")
              #groupSum$GROUPS <- as.numeric(groupSum$GROUPS)
              mergeData <- as.data.table(mergeData)
              mergeData <- merge(mergeData, groupSum, by = c("GROUPS", "FACET"))
              proportion <- mergeData$YAXIS / mergeData$SUM
              mergeData$YAXIS <- proportion
              mergeData$SUM <- NULL
              #mergeData <- unique(mergeData)
            }
          } else {
            if (yaxis_stp2 != "smooth") {
              mergeData <- aggregate(YAXIS ~ GROUPS + XAXIS + FACET, plotData, mean)
            }
          }
          if (yaxis_stp2 == "smooth") {
            plotData$Participant_Id <- NULL
            plotData <- unique(plotData)
          } else {
            dropCols <- c("Participant_Id", "YAXIS")
            plotData <- plotData[, -dropCols, with=FALSE]
            plotData <- unique(plotData)
            plotData <- merge(plotData, mergeData, by = c("GROUPS", "XAXIS", "FACET"))
          }
        } else {
          if (myY %in% strings$source_id) {
            plotData <- transform(plotData, "YAXIS" = ifelse(YAXIS == yaxis_stp1, 1, 0))
            #plotData$YAXIS <- plotData$temp
            #plotData <- plotData[, -"temp"]
            mergeData <- aggregate(YAXIS ~ GROUPS + XAXIS, plotData, sum)
            if (yaxis_stp2 == "proportion") {
              groupSum <- as.data.table(aggregate(Participant_Id ~ GROUPS, plotData, FUN = function(x){length(unique(x))}))
              colnames(groupSum) <- c("GROUPS", "SUM")
              #groupSum$GROUPS <- as.numeric(groupSum$GROUPS)
              mergeData <- as.data.table(mergeData)
              mergeData <- merge(mergeData, groupSum, by = "GROUPS")
              proportion <- mergeData$YAXIS / mergeData$SUM
              mergeData$YAXIS <- proportion
              mergeData$SUM <- NULL
              #mergeData <- unique(mergeData)
            }
          } else {
            if (yaxis_stp2 != "smooth") {
              mergeData <- aggregate(YAXIS ~ GROUPS + XAXIS, plotData, mean)
            }
          }
          if (yaxis_stp2 == "smooth") {
            plotData$Participant_Id <- NULL
            plotData <- unique(plotData)
          } else {
            dropCols <- c("Participant_Id", "YAXIS")
            plotData <- plotData[, -dropCols, with=FALSE]
            plotData <- unique(plotData)
            plotData <- merge(plotData, mergeData, by = c("GROUPS", "XAXIS"))
          }
        }
        print(head(plotData))
        plotData <- unique(plotData)
        if (all(as.numeric(levels(as.factor(plotData$GROUPS))) %in% c(1,0))) {
          plotData <- transform(plotData, "GROUPS" = ifelse(GROUPS == 1, "Positive", "Negative"))
          #plotData$GROUPS <- plotData$temp
          #plotData <- plotData[, -"temp"]
        }
        plotData
      } 
     
    })
    
    
    #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    #below are functions dependent on the four original files pulled in or on singleVarData variable. could fix in future so can be put in other file (functions.R ??)
    
    getNums <- function(){
      #identify nums 
      nums <- subset(metadata.file, metadata.file$type == "number", "source_id") 
      
      nums
    }
    
    getDropList <- function(){
      c("EUPATH_0000644", "BFO_0000015", "EUPATH_0000702", "OBI_0100051")
    }
    
    getFacetList <- function(){
      data <- singleVarData  
      drop <- getDropList()
      
      all.colnames <- c(colnames(prtcpnt.file), colnames(house.file))
      colnames <- setdiff(all.colnames, drop)
      
      #get display names from metadata
      choices <- subset(metadata.file, source_id %in% colnames)
      choicesNumeric <- subset(choices, type %in% "number")
      #remove from choices df anything with levels ==0 or > 12 in tempDF
      temp <- as.vector(choices$source_id)
      boolean <- sapply(data[ , (temp), with=FALSE ], function(x) {(length(levels(as.factor(x))) < 13 & length(levels(as.factor(x))) > 1)})
      choices <- choices[match(names(boolean), choices$source_id),]
      choices <- choices[as.vector(boolean),]
      choices <- rbind(choices, choicesNumeric)
      unique(choices)
      myorder <- sort(choices$property)
      choices <- choices[match(myorder, choices$property),]
      #convert df to list
      choiceList <- as.vector(choices$source_id)
      names(choiceList) <- as.vector(choices$property)
      facetlist <- as.list(choiceList)
      
      facetlist
    }
    
    getGroupList <- function(){
      data <- singleVarData  
      drop <- getDropList()
      
      #this to exclude events info (cant plot event info against event info)
      dropEvents <- colnames(event.file)
      colnames <- colnames(data)
      colnames <- setdiff(colnames, drop)
      colnames <- setdiff(colnames, dropEvents)
      #get display names from metadata
      choices <- subset(metadata.file, source_id %in% colnames)
      myorder <- sort(choices$property)
      choices <- choices[match(myorder, choices$property),]
      #convert data table to list
      choiceList <- as.vector(choices$source_id)
      names(choiceList) <- as.vector(choices$property)
      list <- as.list(choiceList)
      
      list
    }
    
    getMakeGroupList <- function(){
      data <- singleVarData  
      drop <- getDropList()
      
      colnames <- colnames(data)
      colnames <- setdiff(colnames, drop)
      #get display names from metadata
      choices <- subset(metadata.file, source_id %in% colnames)
      myorder <- sort(choices$property)
      choices <- choices[match(myorder, choices$property),]
      #convert data table to list
      choiceList <- as.vector(choices$source_id)
      names(choiceList) <- as.vector(choices$property)
      list <- as.list(choiceList)
      
      list
    }
    
    getOutList <- function(){
      data <- event.file
      drop <- getDropList()
      
      colnames <- colnames(data)
      colnames <- setdiff(colnames, drop)
      #get display names from metadata
      choices <- subset(metadata.file, source_id %in% colnames)
      myorder <- sort(choices$property)
      choices <- choices[match(myorder, choices$property),]
      #convert data table to list
      choiceList <- as.vector(choices$source_id)
      names(choiceList) <- as.vector(choices$property)
      list <- as.list(choiceList)
      
      list
    }
    
    getAttrStp1List <- function(col){
      data <- singleVarData
      tempDF <- completeDT(data, col)
      
      data <- setDT(tempDF)[, lapply(.SD, function(x) unlist(tstrsplit(x, " | ", fixed=TRUE))), 
                          by = setdiff(names(tempDF), eval(col))][!is.na(eval(col))]
 
      levels <- levels(as.factor(data[[col]]))
    }
    
    #seperate pipe delimited fields into their own rows if necessary
    getFinalDT <- function(data, col){
     
      strings <- subset(metadata.file, metadata.file$type == "string", "source_id")
      
      if (col %in% strings$source_id) {
        data <- setDT(data)[, lapply(.SD, function(x) unlist(tstrsplit(x, " | ", fixed=TRUE))), 
                              by = setdiff(names(data), eval(col))][!is.na(eval(col))]
      }
       
      data
      
    }
    
    makeGroups <- function(data, myGroups, groups_stp1, groups_stp2, groups_stp3, groups_stp4){
      #should see if can make this a seperate function to go in the shiny function lib eventually. just pass it stp1, stp2 etc.. dunno
      if (is.null(groups_stp1)) {
        return()
      }
      #get group data for make groups option
      groupData <- completeDT(data, myGroups)
      groupData <- getFinalDT(groupData, myGroups)
      myCols <- c("Participant_Id", myGroups)
      outData <- groupData[, myCols, with=FALSE]
      
      #if anthro direct comparison do same as for number
      if (groups_stp1 == "direct") {
        if (is.null(groups_stp3)) {
          return()
        }
        groups_stp1 = groups_stp2
        groups_stp2 = groups_stp3
      }
      #for numbers
      if (groups_stp1 == "lessThan") {
        if (is.null(groups_stp2)) {
          return()
        }
        outData <- aggregate(outData, by=list(outData$Participant_Id), FUN = function(x){ if (any(x < as.numeric(groups_stp2))) {1} else {0} })
      } else if (groups_stp1 == "greaterThan") {
        if (is.null(groups_stp2)) {
          return()
        }
        outData <- aggregate(outData, by=list(outData$Participant_Id), FUN = function(x){ if (any(x > as.numeric(groups_stp2))) {1} else {0} })
      } else if (groups_stp1 == "equals") {
        if (is.null(groups_stp2)) {
          return()
        }
        outData <- aggregate(outData, by=list(outData$Participant_Id), FUN = function(x){ if (any(x == as.numeric(groups_stp2))) {1} else {0} })
        #for change over time  
      } else if (groups_stp1 == "delta") {
        if(is.null(groups_stp3)) {
          return()
        }
        outData <- completeDT(data, myGroups)
        outData <- getFinalDT(outData, myGroups)
        myCols <- c("Participant_Id", "EUPATH_0000644", myGroups)
        outData <- outData[, myCols, with=FALSE]
        #should start an empty table here to add values in as i go through the loop
        tempTable <- NULL
        #do the below for each participant. think i need for loop. :( but will see if i can think up better way.
        prtcpnts <- levels(as.factor(outData$Participant_Id))
        for (i in prtcpnts) {
          currData <- subset(outData, outData$Participant_Id %in% i)
          
          startDay <- min(currData[, EUPATH_0000644])
          startVal <- currData[[myGroups]][currData$EUPATH_0000644 == startDay]
          endVal <- currData[[myGroups]][currData$EUPATH_0000644 == max(currData[, EUPATH_0000644])]
          diffVal <- startVal - endVal
          
          #if statement for direction of change
          if (startVal > endVal) {
            diffVal = diffVal * -1
          }
          
          if (groups_stp2 == "lessThan") {
            if (diffVal < groups_stp3) {
              row <- c(i, 1)
            } else {
              row <- c(i,0)
            }
          } else if (groups_stp2 == "greaterThan") {
            if (diffVal > groups_stp3) {
              row <- c(i,1)
            } else {
              row <- c(i,0)
            }
          } else {
            if (diffVal == groups_stp3) {
              row <- c(i,1)
            } else {
              row <- c(i,0)
            }
          }
          
          #add participant to growing data table for outcomes
          tempTable <- rbindlist(list(tempTable, as.list(row)))
        }
        #edit outdata so the merge with attr data works..
        outData <- tempTable
        colnames(outData) <- c("Participant_Id", "GROUPS")
      } else if (groups_stp1 == "percentDays") {
        if (is.null(groups_stp4)) {
          return()
        }
        tempTable <- NULL
        #may be able to do this option with aggregate. look into it TODO
        prtcpnts <- levels(as.factor(outData$Participant_Id))
        for (i in prtcpnts) {
          currData <- subset(outData, outData$Participant_Id %in% i)
          
          if (groups_stp3 == "lessThan") {
            currData <- transform(currData, "GROUPS" =  ifelse(currData[[myGroups]] < groups_stp4,1 ,0))
          } else if (groups_stp3 == "greaterThan") {
            currData <- transform(currData, "GROUPS" =  ifelse(currData[[myGroups]] > groups_stp4,1 ,0))
          } else {
            currData <- transform(currData, "GROUPS" =  ifelse(currData[[myGroups]] == groups_stp4,1 ,0))
          }
          colnames(currData) <- c("Participant_Id", "drop", "GROUPS")
          if ((sum(currData$Outcome)/length(currData$Outcome)*100) >= groups_stp2) {
            row <- c(i,1)
          } else {
            row <- c(i,0)
          }
          
          tempTable <- rbindlist(list(tempTable, as.list(row)))
        }
        outData <- tempTable
        colnames(outData) <- c("Participant_Id", "GROUPS")
      }  else {
        #for strings
        outData <- aggregate(outData, by=list(outData$Participant_Id), FUN = function(x){ if(any(grepl(groups_stp1, x, fixed=TRUE)) == TRUE) {1} else {0} })
      }
      if (ncol(outData) > 2) {
        colnames(outData) <- c("Participant_Id", "drop", "GROUPS")
      }
      
      outData <- as.data.table(outData)
      #dtop dtop
      if (any(colnames(outData) %in% "drop")) {
        outData$drop <- NULL
      }
      
      outData
    }
    
    makeGroupLabel <- function(myFacet, facet_stp1, facet_stp2, facet_stp3, facet_stp4){
      numeric <- c("lessThan", "greaterThan", "equals")
      anthro <- c("percentDays", "delta", "direct")
      
      displayName <- metadata.file$property[metadata.file$source_id == myFacet]
      if (facet_stp1 %in% numeric ){
        if (facet_stp1 == "greaterThan") {
          label <- paste0(displayName, " > ", facet_stp2)
        } else if (facet_stp1 == "lessThan") {
          label <- paste0(displayName, " < ", facet_stp2)
        } else {
          label <- paste0(displayName, " = ", facet_stp2)
        }
      } else if (facet_stp1 %in% anthro) {
        if (facet_stp1 == "direct") {
          if (facet_stp2 == "lessThan") {
            label <- paste0(displayName, " < ", facet_stp3)
          } else if (facet_stp2 == "greaterThan") {
            label <- paste0(displayName, " > ", facet_stp3)
          } else {
            label <- paste0(displayName, " = ", facet_stp3)
          }
        } else if (facet_stp1 == "delta") {
          if (facet_stp2 == "lessThan") {
            label <- paste0("Change in ", displayName, " over time < ", facet_stp3)
          } else if (facet_stp2 == "greaterThan") {
            label <- paste0("Change in ", displayName, " over time > ", facet_stp3)
          } else {
            label <- paste0("Change in ", displayName, " over time = ", facet_stp3)
          }
        } else {
          if (facet_stp3 == "lessThan") {
            label <- paste0(displayName, " < ", facet_stp4, " for more than ", facet_stp1, "% of days monitored")
          } else if (facet_stp3 == "greaterThan") {
            label <- paste0(displayName, " > ", facet_stp4, " for more than ", facet_stp1, "% of days monitored")
          } else {
            label <- paste0(displayName, " = ", facet_stp4, " for more than ", facet_stp1, "% of days monitored")
          }
        }
      } else {
        label <- facet_stp1
      }
    }

})
