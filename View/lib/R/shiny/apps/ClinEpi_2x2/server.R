## server.r

require(shiny)
require(data.table)
require(plotly)
require(DT)
require(viridisLite)

source("../../lib/wdkDataset.R")
source("config.R")
source("functions.R")

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
          getWdkDatasetFile('ShinyEvents.tab', session, FALSE, dataStorageDir),
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
    
    if (exists("event.file") & !is.null(event.file)) {
      merge1 <- merge(event.file, prtcpnt.file)
    } else {
      merge1 <- prtcpnt.file
    }
    
    if (exists("house.file") & !is.null(house.file)) {
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
    dates <- getDates()$source_id
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
    nums <- getNums()
    dates <- getDates()
    
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
    nums <- getNums()
    dates <- getDates()
    
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
      
        attrChoiceList <- getAttrList()
        selectInput(inputId = "attr",
                    label = "Variable 1:",
                    choices = attrChoiceList,
                    selected = "EUPATH_0000704",
                    width = '100%')
    })
    
    output$choose_outcome <- renderUI({
      
      #this list should contain anything from events file
      outChoiceList <- getAttrList()
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
      nums <- getNums()
      dates <- getDates()
      
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
            attrStp1List <- getAttrStp1List(myAttr)
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
          attrStp1List <- getAttrStp1List(myAttr)
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
      nums <- getNums()
      dates <- getDates()
      
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
            outStp1List <- getAttrStp1List(myOut)
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
          outStp1List <- getAttrStp1List(myOut)
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
        myPlot <- myPlot + scale_fill_manual(values = viridis(2))
        
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
                  columnDefs = list(list(width = '60px', targets = c(1,2,3)))
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
      p <- round(p, digits=4)
      if (p != "NaN" & p < 0.0001) {
        p <- "<0.0001"
      }
      #make stats table
      stats <- data.table("p-value" = p, "Odds Ratio" = OR, "Relative Risk" = RR)
      colnames(stats) <- c("p-value", "Odds Ratio", "Relative Risk")
      rownames(stats) <- "Statistics"
      
      datatable(stats, 
                width = '100%',
                options = list(
                  sDom = '<"top"><"bottom">', 
                  autoWidth = TRUE, 
                  columnDefs = list(list(width = '60px', targets = c(1,2,3)))
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
        attrData <- getFinalDT(attrData, myAttr)
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
        attrData <- makeGroups(attrData, myAttr, attr_stp1, attr_stp2, attr_stp3, attr_stp4)
        attrLabel <- makeGroupLabel(myAttr, attr_stp1, attr_stp2, attr_stp3, attr_stp4)
        colnames(attrData) <- c("Participant_Id", "Attribute")
       print(attrData)
        #get outcome data
        #may not need to do the splitting on pipes. grepl will still return true for it.
        outData <- completeDT(data, myOut)
        outData <- getFinalDT(outData, myOut)
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
        
        outData <- makeGroups(outData, myOut, out_stp1, out_stp2, out_stp3, out_stp4)
        outLabel <- makeGroupLabel(myOut, out_stp1, out_stp2, out_stp3, out_stp4)
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
    
    
    #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    #below are functions dependent on the four original files pulled in or on singleVarData variable. could fix in future so can be put in other file (functions.R ??)
    
    getNums <- function(){
      #identify nums 
      nums <- subset(metadata.file, metadata.file$type == "number", "source_id") 
      
      nums
    }
    
    getStrings <- function(){
      #identify nums 
      nums <- subset(metadata.file, metadata.file$type == "string", "source_id") 
      
      nums
    }
    
    getDates <- function(){
      #identify nums 
      nums <- subset(metadata.file, metadata.file$type == "date", "source_id") 
      
      nums
    }
    
    getDropList <- function(){
      c("EUPATH_0000644", "BFO_0000015", "EUPATH_0000702", "OBI_0100051")
    }
    
    getAttrList <- function(){
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
        message("groups stp1 is null!! returning")
        return()
      }
      message("preparing data to make own groups")
      #get group data for make groups option
      groupData <- completeDT(data, myGroups)
      groupData <- getFinalDT(groupData, myGroups)
      myCols <- c("Participant_Id", myGroups)
      outData <- groupData[, myCols, with=FALSE]
      
      #if anthro direct comparison do same as for number
      if (!any(c("POSIXct", "Date") %in% class(groups_stp1))) {
        if (groups_stp1 == "direct") {
          if (is.null(groups_stp3)) {
            return()
          }
          groups_stp1 = groups_stp2
          groups_stp2 = groups_stp3
        }
      }
      #this if statement will have to change. handle dates first
      if (any(c("POSIXct", "Date") %in% class(groups_stp1))) {
        outData <- transform(data, "GROUPS" = ifelse(data[[myGroups]] < groups_stp1[2] & data[[myGroups]] > groups_stp1[1], 1, 0))
        cols <- c("Participant_Id", "GROUPS")
        outData <- outData[, cols, with = FALSE]
        outData <- unique(outData)
        message("custom groups for date")
        #for numbers
      } else if (groups_stp1 == "lessThan") {
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
      label <- vector()
      
      displayName <- metadata.file$property[metadata.file$source_id == myFacet]
      if (facet_stp1 %in% numeric ){
        if (facet_stp1 == "greaterThan") {
          label[1] <- paste0(displayName, " > ", facet_stp2)
          label[2] <- paste0(displayName, " <= ", facet_stp2)
        } else if (facet_stp1 == "lessThan") {
          label[1] <- paste0(displayName, " < ", facet_stp2)
          label[2] <- paste0(displayName, " >= ", facet_stp2)
        } else {
          label[1] <- paste0(displayName, " = ", facet_stp2)
          label[2] <- paste0(displayName, " != ", facet_stp2)
        }
      } else if (facet_stp1 %in% anthro) {
        if (facet_stp1 == "direct") {
          if (facet_stp2 == "lessThan") {
            label[1] <- paste0(displayName, " < ", facet_stp3)
            label[2] <- paste0(displayName, " >= ", facet_stp3)
          } else if (facet_stp2 == "greaterThan") {
            label[1] <- paste0(displayName, " > ", facet_stp3)
            label[2] <- paste0(displayName, " <= ", facet_stp3)
          } else {
            label[1] <- paste0(displayName, " = ", facet_stp3)
            label[2] <- paste0(displayName, " != ", facet_stp3)
          }
        } else if (facet_stp1 == "delta") {
          if (facet_stp2 == "lessThan") {
            label[1] <- paste0("Change in ", displayName, " over time < ", facet_stp3)
            label[2] <- paste0("Change in ", displayName, " over time >= ", facet_stp3)
          } else if (facet_stp2 == "greaterThan") {
            label[1] <- paste0("Change in ", displayName, " over time > ", facet_stp3)
            label[2] <- paste0("Change in ", displayName, " over time <= ", facet_stp3)
          } else {
            label[1] <- paste0("Change in ", displayName, " over time = ", facet_stp3)
            label[2] <- paste0("Change in ", displayName, " over time != ", facet_stp3)
          }
        } else {
          if (facet_stp3 == "lessThan") {
            label[1] <- paste0(displayName, " < ", facet_stp4, " for more than ", facet_stp1, "% of days monitored")
            label[2] <- paste0(displayName, " >= ", facet_stp4, " for more than ", facet_stp1, "% of days monitored")
          } else if (facet_stp3 == "greaterThan") {
            label[1] <- paste0(displayName, " > ", facet_stp4, " for more than ", facet_stp1, "% of days monitored")
            label[2] <- paste0(displayName, " <= ", facet_stp4, " for more than ", facet_stp1, "% of days monitored")
          } else {
            label[1] <- paste0(displayName, " = ", facet_stp4, " for more than ", facet_stp1, "% of days monitored")
            label[2] <- paste0(displayName, " != ", facet_stp4, " for more than ", facet_stp1, "% of days monitored")
          }
        }
      } else {
        if (!any(c("POSIXct", "Date") %in% class(facet_stp1))) {
          label[1] <- facet_stp1
          label[2] <- "Other"
        } else {
          label[1] <- "Within Date Range"
          label[2] <- "Outside Date Range"
        }
      }
      label
    }

})
