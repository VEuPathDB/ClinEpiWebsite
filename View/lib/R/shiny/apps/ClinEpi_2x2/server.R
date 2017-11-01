## server.r

require(shiny)
require(data.table)
require(plotly)
require(DT)
require(viridisLite)

source("../../lib/wdkDataset.R")
source("config.R")
source("../../lib/ebrc_functions.R")
source("../../lib/clinepi_functions.R")

options(shiny.sanitize.errors = TRUE)

#% days for diarrhea doesnt show up in outcome because its in participant info rather than events.. what to do.. what to do  
#eupath_0000743 appears to be wrong
#should the ui remember downstream choices?? ex: if you switch from < to > should the num be maintained or change back to mean? 
#biological sex is broken because it has only two options but theyre not yes/no or true/false
#the sliders only show transparent for the first two to appear, names not reused/ dont refer only to whats on screen at the time
#do we want to move naToZero before the merge and only apply to outdata? seems NA for prtcpnt and house info are real NAs

shinyServer(function(input, output, session) {
  
  event.file <- NULL
  prtcpnt.file <- NULL
  house.file <- NULL
  metadata.file <- NULL
  singleVarData <- NULL
  
  #this so i can change mean vals when necessary
  last_attr <- "placeholder"
  last_out <- "placeholder"
 
  filesFetcher <- reactive({
  
  if (is.null(prtcpnt.file)) {

      prtcpnt_temp <- try(fread(
          getWdkDatasetFile('ShinyParticipants.tab', session, FALSE, dataStorageDir),
          na.strings = "N/A"))

      house_temp <- try(fread(
          getWdkDatasetFile('ShinyHouseholds.tab', session, FALSE, dataStorageDir),
          na.strings = "N/A"))

      event_temp <- try(fread(
          getWdkDatasetFile('ShinyObservations.tab', session, FALSE, dataStorageDir),
          na.strings = "N/A"))

      metadata_temp <- try(fread(
          getWdkDatasetFile('ontologyMetadata.tab', session, FALSE, dataStorageDir),
          ))
      
      if (grepl("Error", prtcpnt_temp[1])){
        stop("Error: Participant file missing or unreadable!")
      } else {
        prtcpnt.file <<- prtcpnt_temp
        names(prtcpnt.file) <<-  gsub(" ", "_", gsub("\\[|\\]", "", names(prtcpnt.file)))
        setkey(prtcpnt.file, Participant_Id)
      }
      
      if (grepl("Error", metadata_temp[1])){
        stop("Error: Metadata file missing or unreadable!")
      } else {
        metadata.file <<- metadata_temp
        names(metadata.file) <<- gsub(" ", "_", tolower(gsub("\\[|\\]", "", names(metadata.file))))
        setkey(metadata.file, source_id)
        
        #check for unique display names in metdata file.
        if (length(unique(metadata.file$property)) != nrow(metadata.file)) {
          true <- duplicated(metadata.file$property) | duplicated(metadata.file$property, fromLast = TRUE)
          metadata.file <<- transform(metadata.file, "property" = ifelse(true, paste0(property, ", ", parent), property))
        }
      }
      
      if (grepl("Error", house_temp[1])){
        message("Warning: Household file not found or unreadable.")
      } else {
        house.file <<- house_temp
        names(house.file) <<-  gsub(" ", "_", gsub("\\[|\\]", "", names(house.file)))
        setkey(house.file, Participant_Id)
      }
      
      if (grepl("Error", event_temp[1])){
        message("Warning: Events file not found or unreadable.")
      } else {
        event.file <<- event_temp
        names(event.file) <<-  gsub(" ", "_", gsub("\\[|\\]", "", names(event.file)))
        setkey(event.file, Participant_Id)
      }
      
    }
  })
  
  singleVarDataFetcher <- function(){
    filesFetcher()
    
    #remove non-unique column names and merge them to one data table to return
    drop <- c("source_id", "project_id")
    #consider moving drop to event.file TODO
    prtcpnt.file <<- prtcpnt.file[, (drop):=NULL]
    
    if (exists("event.file") & !is.null(event.file) & nrow(event.file) > 1) {
      merge1 <- merge(event.file, prtcpnt.file)
    } else {
      merge1 <- prtcpnt.file
    }
    
    if (exists("house.file") & !is.null(house.file) & nrow(house.file) > 1) {
      #house.file <<- house.file[, (drop):=NULL]
      house.file <<- house.file[, -(drop), with = FALSE]
      singleVarData <<- merge(merge1, house.file)
    } else {
      singleVarData <<- merge1
    }
    
    if (any(colnames(singleVarData) %in% "EUPATH_0000644")) {
      setkey(singleVarData, EUPATH_0000644)
    }
    
    #for all dates convert strings to date format
    dates <- getDates(metadata.file)$source_id
    for (col in dates) set(singleVarData, j=col, value=as.Date(singleVarData[[col]]))
    
    singleVarData
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
  
  #store info about numeric vals
  attrRange <- reactiveValues()
  
  setAttrVals <- reactive ({
    myAttr <- input$attr
    nums <- getNums(metadata.file)
    dates <- getDates(metadata.file)
    
    if (myAttr %in% nums$source_id | myAttr %in% dates$source_id) {
      data <- singleVarData
      tempDF <- completeDT(data, myAttr)
      
      if (any(colnames(event.file) %in% myAttr) & any(colnames(tempDF) %in% "BFO_0000015")) {
        if (levels(as.factor(tempDF$BFO_0000015)) == "Diarrhea Episode") {
          attrRange$myMin = 0
        }
      } else {
        attrRange$myMin <- min(tempDF[[myAttr]])
      }
      attrRange$myMax <- max(tempDF[[myAttr]])
      
      if (myAttr %in% nums$source_id) {
        attrRange$mean <- mean(tempDF[[myAttr]])
      } else {
        attrRange$startDate <- as.Date(quantile(as.POSIXct(tempDF[[myAttr]]), .25))
        attrRange$endDate <- as.Date(quantile(as.POSIXct(tempDF[[myAttr]]), .75))
        attrRange$myMin <- as.Date(attrRange$myMin)
        attrRange$myMax <- as.Date(attrRange$myMax)
        message(paste("start and end dates:", attrRange$startDate, attrRange$endDate))
      }
    }
    
  })
  
  outRange <- reactiveValues()
  
  setOutVals <- reactive({
    myOut <- input$out
    nums <- getNums(metadata.file)
    dates <- getDates(metadata.file)
    
    if (myOut %in% nums$source_id | myOut %in% dates$source_id) {
      data <- singleVarData
      tempDF <- completeDT(data, myOut)
      
      if (any(colnames(event.file) %in% myOut) & any(colnames(tempDF) %in% "BFO_0000015")) {
        if (levels(as.factor(tempDF$BFO_0000015)) == "Diarrhea Episode") {
          outRange$myMin = 0
        }
      } else {
        print(head(tempDF))
        print(tempDF[[myOut]])
        message(paste("class myOut:", class(tempDF[[myOut]])))
        outRange$myMin <- min(tempDF[[myOut]])
      }
      outRange$myMax <- max(tempDF[[myOut]])
      
      if (myOut %in% nums$source_id) {
        outRange$mean <- mean(tempDF[[myOut]])
      } else {
        outRange$startDate <- as.Date(quantile(as.POSIXct(tempDF[[myOut]]), .25))
        outRange$endDate <- as.Date(quantile(as.POSIXct(tempDF[[myOut]]), .75))
        outRange$myMin <- as.Date(outRange$myMin)
        outRange$myMax <- as.Date(outRange$myMax)
        message(paste("start and end dates:", outRange$startDate, outRange$endDate))
      }
    }
    
  })
  
  #make sure ranges are updated when attr or out change
  observeEvent(input$out, setOutVals())
  observeEvent(input$attr, setAttrVals())
  
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
    
    output$choose_attribute <- renderUI({
      
        attrChoiceList <- getUIList(singleVarData, metadata.file)
        selectInput(inputId = "attr",
                    label = "Variable 1:",
                    choices = attrChoiceList,
                    selected = "EUPATH_0000704",
                    width = '100%')
    })
    
    output$choose_outcome <- renderUI({
      
      #this list should contain anything from events file
      outChoiceList <- getUIList(singleVarData, metadata.file)
      selectInput(inputId = "out",
                  label = "Variable2:",
                  choices = outChoiceList,
                  selected = "EUPATH_0000665",
                  width = '100%')
    })
    
    output$attr_stp1 <- renderUI({
      if (is.null(input$attr)) {
        return()
      }
      myAttr <- input$attr
      nums <- getNums(metadata.file)
      dates <- getDates(metadata.file)
      
      data <- singleVarData
      tempDF <- completeDT(data, myAttr)
      
      if (any(colnames(tempDF) %in% "BFO_0000015")) {
        if (any(colnames(event.file) %in% myAttr) & levels(as.factor(tempDF$BFO_0000015)) == "Anthropometry") {
          selectInput(inputId = "attr_stp1",
                      label = "where",
                      choices = list('the change in value over time selected' = 'delta', 'more than the following percent of days' = 'percentDays', "a direct comparison" = "direct"),
                      selected = "delta",
                      width = '100%')
        } else {
          if (myAttr %in% nums$source_id) {
            selectInput(inputId = "attr_stp1",
                        label = "is",
                        choices = list('<' = 'lessThan', '>' = 'greaterThan', '=' = 'equals'),
                        selected = "greaterThan",
                        width = '100%')
          } else if (myAttr %in% dates$source_id) {
            dateRangeInput(inputId = "attr_stp1",
                           label = "is between",
                           start = attrRange$startDate, end = attrRange$endDate,
                           min = attrRange$myMin, max = attrRange$myMax,
                           separator = "and",
                           startview = "year")
          } else {
            attrStp1List <- getUIStp1List(singleVarData, myAttr)
            if (length(attrStp1List) == 2) {
              if (any(attrStp1List %in% "Yes")) {
                selectInput(inputId = "attr_stp1",
                            label = NULL,
                            choices = attrStp1List,
                            selected = "Yes",
                            width = '100%')
              } else if (any(attrStp1List %in% "TRUE")) {
                selectInput(inputId = "attr_stp1",
                            label = NULL,
                            choices = attrStp1List,
                            selected = "TRUE",
                            width = '100%')
              } else {
                selectInput(inputId = "attr_stp1",
                            label = NULL,
                            choices = attrStp1List,
                            width = '100%')
              }
            } else {
              selectInput(inputId = "attr_stp1",
                          label = NULL,
                          choices = attrStp1List,
                          width = '100%')
            }
          }
        }
      } else {
        if (myAttr %in% nums$source_id) {
          selectInput(inputId = "attr_stp1",
                      label = "is",
                      choices = list('<' = 'lessThan', '>' = 'greaterThan', '=' = 'equals'),
                      selected = "greaterThan",
                      width = '100%')
        } else if (myAttr %in% dates$source_id) {
          dateRangeInput(inputId = "attr_stp1",
                         label = "is between",
                         start = attrRange$startDate, end = attrRange$endDate,
                         min = attrRange$myMin, max = attrRange$myMax,
                         separator = "and",
                         startview = "year")
        } else {
          attrStp1List <- getUIStp1List(singleVarData, myAttr)
          if (length(attrStp1List) == 2) {
            if (any(attrStp1List %in% "Yes")) {
              selectInput(inputId = "attr_stp1",
                          label = NULL,
                          choices = attrStp1List,
                          selected = "Yes",
                          width = '100%')
            } else if (any(attrStp1List %in% "TRUE")) {
              selectInput(inputId = "attr_stp1",
                          label = NULL,
                          choices = attrStp1List,
                          selected = "TRUE",
                          width = '100%')
            } else {
              selectInput(inputId = "attr_stp1",
                          label = NULL,
                          choices = attrStp1List,
                          width = '100%')
            }
          } else {
            selectInput(inputId = "attr_stp1",
                        label = NULL,
                        choices = attrStp1List,
                        width = '100%')
          }
        }
      }
      
    })
    
    output$out_stp1 <- renderUI({
      if (is.null(input$out)) {
        return()
      }
      myOut <- input$out
      nums <- getNums(metadata.file)
      dates <- getDates(metadata.file)
      
      data <- singleVarData
      tempDF <- completeDT(data, myOut)
     
      if (any(colnames(tempDF) %in% "BFO_0000015")) {
        if (any(colnames(event.file) %in% myOut) & levels(as.factor(tempDF$BFO_0000015)) == "Anthropometry") {
          selectInput(inputId = "out_stp1",
                      label = "where",
                      choices = list('the change in value over time selected' = 'delta', 'more than the following percent of days' = 'percentDays', "a direct comparison" = "direct"),
                      selected = "delta",
                      width = '100%') 
        } else {
          if (myOut %in% nums$source_id) {
            selectInput(inputId = "out_stp1",
                        label = "is",
                        choices = list('<' = 'lessThan', '>' = 'greaterThan', '=' = 'equals'),
                        selected = "greaterThan",
                        width = '100%')
          } else if (myOut %in% dates$source_id) {
            dateRangeInput(inputId = "out_stp1",
                           label = "is between",
                           start = outRange$startDate, end = outRange$endDate,
                           min = outRange$myMin, max = outRange$myMax,
                           separator = "and",
                           startview = "year")
          } else {
            outStp1List <- getUIStp1List(singleVarData, myOut)
            if (length(outStp1List) == 2) {
              if (any(outStp1List %in% "Yes")) {
                selectInput(inputId = "out_stp1",
                            label = NULL,
                            choices = outStp1List,
                            selected = "Yes",
                            width = '100%')
              } else if (any(outStp1List %in% "TRUE")) {
                selectInput(inputId = "out_stp1",
                            label = NULL,
                            choices = outStp1List,
                            selected = "TRUE",
                            width = '100%')
              } else {
                selectInput(inputId = "out_stp1",
                            label = NULL,
                            choices = outStp1List,
                            width = '100%')
              }
            } else {
              selectInput(inputId = "out_stp1",
                          label = NULL,
                          choices = outStp1List,
                          width = '100%')
            }
          }
        }
     } else {
        if (myOut %in% nums$source_id) {
          selectInput(inputId = "out_stp1",
                      label = "is",
                      choices = list('<' = 'lessThan', '>' = 'greaterThan', '=' = 'equals'),
                      selected = "greaterThan",
                      width = '100%')
        } else if (myOut %in% dates$source_id) {
          dateRangeInput(inputId = "out_stp1",
                         label = "is between",
                         start = outRange$startDate, end = outRange$endDate,
                         min = outRange$myMin, max = outRange$myMax,
                         separator = "and",
                         startview = "year")
        } else {
          outStp1List <- getUIStp1List(singleVarData, myOut)
          if (length(outStp1List) == 2) {
            if (any(outStp1List %in% "Yes")) {
              selectInput(inputId = "out_stp1",
                          label = NULL,
                          choices = outStp1List,
                          selected = "Yes",
                          width = '100%')
            } else if (any(outStp1List %in% "TRUE")) {
              selectInput(inputId = "out_stp1",
                          label = NULL,
                          choices = outStp1List,
                          selected = "TRUE",
                          width = '100%')
            } else {
              selectInput(inputId = "out_stp1",
                          label = NULL,
                          choices = outStp1List,
                          width = '100%')
            }
          } else {
            selectInput(inputId = "out_stp1",
                        label = NULL,
                        choices = outStp1List,
                        width = '100%')
          }
        }
      }
      
    })
    
    output$attr_stp2 <- renderUI ({
      if (is.null(input$attr_stp1)) {
        return()
      }
      myStp1Val <- input$attr_stp1
      
      numeric <- c("lessThan", "greaterThan", "equals")
      anthro <- c("percentDays", "delta", "direct")
      
      if (myStp1Val %in% anthro) {
        if (myStp1Val == 'percentDays') {
          numericInput(inputId = "attr_stp2",
                       label = NULL,
                       value = 50,
                       min = 0,
                       max = 100,
                       width = '100%')
        } else {
          selectInput(inputId = "attr_stp2",
                      label = "is",
                      choices = list('<' = 'lessThan', '>' = 'greaterThan', '=' = 'equals'),
                      selected = "greaterThan",
                      width = '100%')
        }
      } else {
        if (myStp1Val %in% numeric) {
          #just going to set default value to whatever the mean is
          sliderInput("attr_stp2", NULL,
                      min = attrRange$myMin, max = attrRange$myMax, value = attrRange$mean, step = .1, width = '100%')
        }
      }
      
    })
 
    output$out_stp2 <- renderUI({
      if (is.null(input$out_stp1)) {
        return()
      }
      myStp1Val <- input$out_stp1
      
      numeric <- c("lessThan", "greaterThan", "equals")
      anthro <- c("percentDays", "delta", "direct")
      
      if (myStp1Val %in% anthro) {
        if (myStp1Val == 'percentDays') {
          numericInput(inputId = "out_stp2",
                       label = NULL,
                       value = 50,
                       min = 0,
                       max = 100,
                       width = '100%')
        } else {
          selectInput(inputId = "out_stp2",
                      label = "is",
                      choices = list('<' = 'lessThan', '>' = 'greaterThan', '=' = 'equals'),
                      selected = "greaterThan",
                      width = '100%')
        }
      } else {
        if (myStp1Val %in% numeric) {
          #just going to set default value to whatever the mean is 
          sliderInput("out_stp2", NULL,
                      min = outRange$myMin, max = outRange$myMax, value = outRange$mean, step = .1, width = '100%')
        } 
      }
      
    })
    
    output$attr_stp3 <- renderUI ({
      if (is.null(input$attr_stp1)) {
        return()
      }
      myStp1Val <- input$attr_stp1
      
      anthro <- c("delta", "direct", "percentDays")
      
      if (myStp1Val %in% anthro) {
        if (myStp1Val == "percentDays") {
          selectInput(inputId = "attr_stp3",
                      label = "are",
                      choices = list('<' = 'lessThan', '>' = 'greaterThan', '=' = 'equals'),
                      selected = "greaterThan",
                      width = '100%')
        } else if (myStp1Val == "direct") {
           sliderInput("attr_stp3", NULL,
                       min = attrRange$myMin, max = attrRange$myMax, value = attrRange$mean, step = .1, width='100%')
        } else {
          sliderInput("attr_stp3", NULL,
                      min = -20, max = 20, value = -1, step = .1, width = '100%')
        }
        
      }
      
    })
    
    output$out_stp3 <- renderUI ({
      if (is.null(input$out_stp1)) {
        return()
      }
      myStp1Val <- input$out_stp1
    
      anthro <- c("delta", "direct", "percentDays")
      
      if (myStp1Val %in% anthro) {
        if (myStp1Val == "percentDays") {
          selectInput(inputId = "out_stp3",
                      label = "are",
                      choices = list('<' = 'lessThan', '>' = 'greaterThan', '=' = 'equals'),
                      selected = "greaterThan",
                      width = '100%')
        } else if (myStp1Val == "direct") {
          sliderInput("out_stp3", NULL,
                      min = outRange$myMin, max = outRange$myMax, value = outRange$mean, step = .1, width='100%')
        } else {
          sliderInput("out_stp3", NULL,
                      min = -20, max = 20, value = -1, step = .1, width = '100%')
        }
        
      }
      
    })
  
    output$attr_stp4 <- renderUI({
      if (is.null(input$attr_stp1)) {
        return()
      }
      myStp1Val <- input$attr_stp1
      
      if (!any(c("POSIXct", "Date") %in% class(myStp1Val))) {
        if (myStp1Val == "percentDays") {
          sliderInput("attr_stp4", NULL,
                      min = attrRange$myMin, max = attrRange$myMax, value = attrRange$mean, step = .1, width='100%')
        }
      }
    })
      
    output$out_stp4 <- renderUI({
      if (is.null(input$out_stp1)) {
        return()
      }
      myStp1Val <- input$out_stp1
     
      if (!any(c("POSIXct", "Date") %in% class(myStp1Val))) { 
        if (myStp1Val == "percentDays") {
          sliderInput("out_stp4", NULL,
                      min = outRange$myMin, max = outRange$myMax, value = outRange$mean, step = .1, width='100%')
        }
      }
    })
    
    output$plot <- renderPlotly({
      
        tableData <- plotData()
        if (is.null(tableData)) {
          message("plotData returned null!")
          return()
        }
        
        rows <- tableData$rownames
        #remove totals
        df <- tableData[ -nrow(tableData), ]
        df$Totals <- NULL
        df$rownames <- NULL
        #reorganize to make stacked bar
        cols <- colnames(df)
        df <- t(df)
        message("plot df after transform")
        df <- rbind(df, c(df[1,2], NA))
        df <- rbind(df, c(df[2,2], NA))
        df <- cbind(c(cols[1], cols[2], cols[1], cols[2]), df)
        colnames(df) <- c("Outcome" ,"Proportion", "Attribute")
        df <- data.table(df)
        df$Proportion = as.numeric(df$Proportion)
        df$Attribute <- c(rows[1], rows[1], rows[2], rows[2])
        df$Outcome <- factor(df$Outcome, levels = c(cols[1], cols[2]))
        df$Attribute <- factor(df$Attribute, levels = c(rows[1], rows[2]))
        
        #define axis labels here
        xlab <- ""
        ylab <- "Proportion"
        
        #determine width of bars for outcome
        outPos <- tableData[1,3 , with = FALSE]
        outNeg <- tableData[2,3 , with = FALSE]
        total <- tableData$Totals[3]
        OPprop <- outPos / total
        ONprop <- outNeg / total
        width <- c( OPprop*1.9, ONprop*1.9, OPprop*1.9, ONprop*1.9)
        df$width <- width
   
        #plot here
        myPlot <- ggplot(data = df, aes(x = Outcome, y = Proportion, width = width, fill = Attribute))
        myPlot <- myPlot + theme_bw()
        myPlot <- myPlot + labs(y = ylab, x = xlab)
        
        myPlot <- myPlot + geom_bar(stat = "identity", position = "fill")
        #myPlot <- myPlot + scale_fill_manual(values = c("#32baba", "#e26c6c"))
        #myPlot <- myPlot + scale_fill_manual(values = plasma(2))
        myPlot <- myPlot + scale_fill_manual(values = viridis(2, begin = .25, end = .75))
        
        #should keep playing with this vs doing it with ggplot syntax. 
        x_list <- list(
          title = xlab,
          size = 14 
        )
        y_list <- list(
          title = ylab,
          size = 14
        )
        
        #myPlotly <- ggplotly(myPlot, tooltip = c("text", "x"))
        myPlotly <- ggplotly(myPlot)
        myPlotly <- config(myPlotly, displaylogo = FALSE, collaborate = FALSE) %>% layout(xaxis = x_list, yaxis = y_list)
        
        myPlotly
      
    })

    output$table <- DT::renderDataTable({
      data <- plotData()
      if (is.null(data)) {
        return()
      }
      data$rownames <- NULL
      datatable(data, 
                width = '100%',
                options = list(
                  sDom = '<"top"><"bottom">',
                  autoWidth = TRUE, 
                  columnDefs = list(list(width = '250px', className = 'dt-right', targets = c(1,2,3)))
        )
      )
    })
    
    output$statsTable <- DT::renderDataTable({
      tableData <- plotData()
      if (is.null(tableData)) {
        return()
      }
      
      #remove totals
      df <- tableData[ -nrow(tableData), ]
      df$Totals <- NULL
      df$rownames <- NULL
      #get pval
      p <- chisq.test(df)
      p <- p$p.value
      #get or
      a <- df[1,1 , with = FALSE]
      b <- df[2,1 , with = FALSE]
      c <- df[1,2, with = FALSE]
      d <- df[2,2 , with = FALSE]
      OR <- (a*d)/(b*c)
      #get rr
      RR <- (a/(a+b)) / (c/(c+d))
      #if 10 digits or fewer dont use exponential notation
      options("scipen"=10)
      #round to 4 digits and for p-value show "<0.0001" when appropriate
      OR <- round(OR, digits=4)
      RR <- round(RR, digits=4)
  
      #calc conf interval. later install epitools and let it do the work for you.
      alpha <- 0.05
      siglog <- sqrt((1/a) + (1/b) + (1/c) + (1/d))
      zalph <- qnorm(1 - alpha/2)
      logOR <- log(OR)
      logRR <- log(RR)
      logloOR <- logOR - zalph * siglog
      logloRR <- logRR - zalph * siglog
      loghiOR <- logOR + zalph * siglog
      loghiRR <- logRR + zalph * siglog
      
      ORlo <- round(exp(logloOR), digits=4)
      RRlo <- round(exp(logloRR), digits=4)
      ORhi <- round(exp(loghiOR), digits=4)
      RRhi <- round(exp(loghiRR), digits=4)

      p <- round(p, digits=4)
      if (p != "NaN" & p < 0.0001) {
        p <- "<0.0001"
      }
      #make stats table
      stats <- data.table("p-value" = p, "Odds Ratio" = paste(ORlo, "-", ORhi), "Relative Risk" = paste(RRlo, "-", RRhi))
      colnames(stats) <- c("p-value", "Odds Ratio", "Relative Risk")
      rownames(stats) <- "Statistics"
      
      datatable(stats, 
                width = '100%',
                options = list(
                  sDom = '<"top"><"bottom">',
                  autoWidth = TRUE, 
                  columnDefs = list(list(width = '250px', className = 'dt-right', targets = c(1,2,3)))
                )
      )
    })
    
    #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    #values grabbed through reactive functions for better control of reactive context
  
    #all the work will be done here in prepping data
    plotData <- debounce(reactive({
      
      #collecting inputs 
      myTimeframe <- input$timeframe
      if (is.null(input$attr)) {
        return()
      } else {
        myAttr <- input$attr
      }
      if (is.null(input$out)) {
        return()
      } else {
        myOut <- input$out
      }
      
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
  
      go <- TRUE
      
      if (is.null(input$out_stp1) | is.null(input$attr_stp1)) {
        go <- FALSE
      }
        
      #once last field is populated .. GO
      if (go) {
        message("GO!!")
        #grab validated inputs
        out_stp1 <- input$out_stp1
        out_stp3 <- input$out_stp3
        out_stp4 <- input$out_stp4
        out_stp2 <- input$out_stp2
        attr_stp1 <- input$attr_stp1
        attr_stp2 <- input$attr_stp2
        attr_stp3 <- input$attr_stp3
        attr_stp4 <- input$attr_stp4
        
        #get attr col
        attrData <- completeDT(data, myAttr)
        attrData <- getFinalDT(attrData, metadata.file, myAttr)
        myCols <- c("Participant_Id", myAttr)
        attrData <- attrData[, myCols, with=FALSE]
        
        numeric <- c("lessThan", "greaterThan", "equals")
        anthro <- c("percentDays", "delta", "direct")
        if (is.null(attr_stp1) | is.null(myAttr)) {
            return()
        } else {
          if (attr_stp1 %in% numeric) {
            if (is.null(attr_stp2)) {
              return()
            }
          }
          if (attr_stp1 %in% anthro) {
            if (attr_stp1 == "percentDays") {
              if (is.null(attr_stp4)) {
                return()
              }
            } else {
              if (is.null(attr_stp3)) {
                return()
              }
            }
          }
        }
        message("in plotData()")
        attrData <- makeGroups(attrData, metadata.file, myAttr, attr_stp1, attr_stp2, attr_stp3, attr_stp4)
        attrLabel <- makeGroupLabel(myAttr, metadata.file, attr_stp1, attr_stp2, attr_stp3, attr_stp4)
        colnames(attrData) <- c("Participant_Id", "Attribute")
       print(attrData)
        #get outcome data
        #may not need to do the splitting on pipes. grepl will still return true for it.
        outData <- completeDT(data, myOut)
        outData <- getFinalDT(outData, metadata.file, myOut)
        myCols <- c("Participant_Id", myOut)
        outData <- outData[, myCols, with=FALSE]
        
        if (is.null(out_stp1) | is.null(myOut)) {
          return()
        } else {
            if (out_stp1 %in% numeric) {
              if (is.null(out_stp2)) {
                return()
              }
            }
            if (out_stp1 %in% anthro) {
              if (out_stp1 == "percentDays") {
                if (is.null(out_stp4)) {
                  return()
                }
              } else {
                if (is.null(out_stp3)) {
                  return()
                }
              }
            }
          }
        
        outData <- makeGroups(outData, metadata.file, myOut, out_stp1, out_stp2, out_stp3, out_stp4)
        outLabel <- makeGroupLabel(myOut, metadata.file, out_stp1, out_stp2, out_stp3, out_stp4)
        colnames(outData) <- c("Participant_Id", "Outcome")
        print(outData)
        #merge on participant id an1d keep all prtcpnts.
        data <- merge(attrData, outData, by = "Participant_Id", all = TRUE)
        #replace NA with 0, essentially assuming that no reporting is negative reporting
        naToZero(data)
        
        #format into 2x2
        data <- transform(data, "APOP" = ifelse( Attribute == 1 & Outcome == 1, 1, 0))
        data <- transform(data, "APOF" = ifelse( Attribute == 1 & Outcome == 0, 1, 0))
        data <- transform(data, "AFOP" = ifelse( Attribute == 0 & Outcome == 1, 1, 0))
        data <- transform(data, "AFOF" = ifelse( Attribute == 0 & Outcome == 0, 1, 0))
        
        APOP <- sum(data$APOP)
        APOF <- sum(data$APOF)
        AFOP <- sum(data$AFOP)
        AFOF <- sum(data$AFOF)
        
        APtotal <- APOP + APOF
        AFtotal <- AFOP + AFOF
        OPtotal <- APOP + AFOP
        OFtotal <- APOF + AFOF
        total <- APOP + APOF + AFOP + AFOF
        
        OP <- c(APOP, AFOP, OPtotal)
        OF <- c(APOF, AFOF, OFtotal)
        totals <- c(APtotal, AFtotal, total)
        
        #tableData <- data.table(outLabel[1] = OP, outLabel[2] = OF, "Totals" = totals)
        tableData <- data.table("Outcome+" = OP, "Outcome-" = OF, "Totals" = totals)
        colnames(tableData) <- c(outLabel[1], outLabel[2], "Totals")
        rownames(tableData) <- c(attrLabel[1], attrLabel[2], "Totals")
        tableData$rownames <- c(attrLabel[1], attrLabel[2], "Totals")
        print(tableData)
        tableData
      } 
      
      #debounce will wait 2s with no changes to inputs before plotting.
    }), 2000)
    

})
