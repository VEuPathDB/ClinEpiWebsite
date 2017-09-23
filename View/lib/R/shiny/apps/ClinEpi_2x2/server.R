## server.r

require(shiny)
require(data.table)
#require(plotly)
require(ggplot2)
require(DT)

source("../../lib/wdkDataset.R")
source("config.R")
source("functions.R")

#% days for diarrhea doesnt show up in outcome because its in participant info rather than events.. what to do.. what to do  
#eupath_0000743 appears to be wrong

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

      #this being done for now while i work in rstudio
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
    setkey(singleVarData, EUPATH_0000644)
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
      stop("This dataset not recognized ... yet.")
    }
    
    #maybe make it so that if it cant find these two columns then it doesn't let there be a subset/ timeframe option at all but will continue without it
    #can even have some else if options if there are backups that are likely to exist
    
    tempDF
  }
    
    output$choose_timeframe <- renderUI({
      ageDays = "EUPATH_0000644"
    
          if (!length(singleVarData)) {
            data <- singleVarDataFetcher()
          } else {
            data <- singleVarData
          }
        tempDF <- completeDT(data, ageDays)
    
        myMin <- min(tempDF[, (ageDays), with=FALSE])
        myMax <- max(tempDF[, (ageDays), with=FALSE])
      
        sliderInput("timeframe", "Timeframe:",
                  min = myMin, max = myMax, value = c(myMin,myMax), round=TRUE, width = '100%')
    })
    
    output$choose_attribute <- renderUI({
      
        attrChoiceList <- getAttrList()
        selectInput(inputId = "attr",
                    label = "Attribute:",
                    choices = attrChoiceList,
                    selected = "EUPATH_0000704",
                    width = '100%')
    })
    
    output$choose_outcome <- renderUI({
      
      #this list should contain anything from events file
      outChoiceList <- getOutList()
      selectInput(inputId = "out",
                  label = "Outcome:",
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
      
      data <- singleVarData
      tempDF <- completeDT(data, myAttr)
      
      if (any(colnames(event.file)) %in% myAttr & levels(as.factor(tempDF$BFO_0000015)) == "Anthropometry") {
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
      
      data <- singleVarData
      tempDF <- completeDT(data, myOut)
      
     if (levels(as.factor(tempDF$BFO_0000015)) == "Anthropometry") {
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
          myAttr <- input$attr
          data <- singleVarData
          tempDF <- completeDT(data, myAttr)
          
          if (levels(as.factor(tempDF$BFO_0000015)) == "Diarrhea Episode") {
            myMin = 0
          } else {
            myMin <- min(tempDF[, (myAttr), with=FALSE])
          }
          myMax <- max(tempDF[, (myAttr), with=FALSE])
          
          #just going to set default value to whatever the mean is
          #sliderInput("attr_stp2", "Comparative value:",
          #            min = myMin, max = myMax, value = mean(tempDF[[myAttr]], na.rm = T) , round=TRUE, width = '100%')
          numericInput("attr_stp2", NULL,
                       min = myMin, max = myMax, value = mean(tempDF[[myAttr]]), width = '100%')
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
          myOut <- input$out
          data <- singleVarData
          tempDF <- completeDT(data, myOut)
          
          if (levels(as.factor(tempDF$BFO_0000015)) == "Diarrhea Episode") {
            myMin = 0
          } else {
            myMin <- min(tempDF[, (myOut), with=FALSE])
          }
          myMax <- max(tempDF[, (myOut), with=FALSE])
          
          #just going to set default value to whatever the mean is 
          #sliderInput("out_stp2", "Comparative value:",
          #            min = myMin, max = myMax, value = mean(tempDF[[myOut]], na.rm = T) , round=TRUE, width = '100%')
          numericInput("out_stp2", NULL,
                       min = myMin, max = myMax, value = mean(tempDF[[myOut]]), width = '100%')
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
          myOut <- input$attr
          data <- singleVarData
          tempDF <- completeDT(data, myOut)
          
          if (levels(as.factor(tempDF$BFO_0000015)) == "Diarrhea Episode") {
            myMin = 0
          } else {
            myMin <- min(tempDF[, (myAttr), with=FALSE])
          }
          myMax <- max(tempDF[, (myAttr), with=FALSE])
          
          #sliderInput("attr_stp3", "Comparative value:",
          #            min = myMin, max = myMax, value = mean(tempDF[[myOut]], na.rm = T) , round=TRUE, width='100%')
          numericInput("attr_stp3", NULL,
                       min = myMin, max = myMax, value = mean(tempDF[[myAttr]]), width = '100%')
        } else {
          #sliderInput("attr_stp3", "Comparative value:",
          #            min = -20, max = 20, value = -1, step = .1, width = '100%')
          numericInput("attr_stp3", NULL,
                       min = -20, max = 20, value = -0.5, step = .1, width = '100%')
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
          myOut <- input$out
          data <- singleVarData
          tempDF <- completeDT(data, myOut)
          
          if (levels(as.factor(tempDF$BFO_0000015)) == "Diarrhea Episode") {
            myMin = 0
          } else {
            myMin <- min(tempDF[, (myOut), with=FALSE])
          }
          myMax <- max(tempDF[, (myOut), with=FALSE])
          
          #sliderInput("out_stp3", "Comparative value:",
          #            min = myMin, max = myMax, value = mean(tempDF[[myOut]], na.rm = T) , round=TRUE, width='100%')
          numericInput("out_stp3", NULL,
                       min = myMin, max = myMax, value = mean(tempDF[[myOut]]), width = '100%')
        } else {
          #sliderInput("out_stp3", "Comparative value:",
          #            min = -20, max = 20, value = -1, step = .1, width = '100%')
          numericInput("out_stp3", NULL,
                       min = -20, max = 20, value = -0.5, step = .1, width = '100%')
        }
        
      }
      
    })
  
    output$attr_stp4 <- renderUI({
      if (is.null(input$attr_stp1)) {
        return()
      }
      myStp1Val <- input$attr_stp1
      
      if (myStp1Val == "percentDays") {
        myOut <- input$attr
        data <- singleVarData
        tempDF <- completeDT(data, myOut)
        
        if (levels(as.factor(tempDF$BFO_0000015)) == "Diarrhea Episode") {
          myMin = 0
        } else {
          myMin <- min(tempDF[, (myAttr), with=FALSE])
        }
        myMax <- max(tempDF[, (myAttr), with=FALSE])
        
        #sliderInput("attr_stp4", "Comparative value:",
        #            min = myMin, max = myMax, value = mean(tempDF[[myOut]], na.rm = T) , round=TRUE, width='100%')
        numericInput("attr_stp4", NULL,
                     min = myMin, max = myMax, value = mean(tempDF[[myAttr]]), width = '100%')
      }
    })
      
    output$out_stp4 <- renderUI({
      if (is.null(input$out_stp1)) {
        return()
      }
      myStp1Val <- input$out_stp1
      
      if (myStp1Val == "percentDays") {
        myOut <- input$out
        data <- singleVarData
        tempDF <- completeDT(data, myOut)
        
        if (levels(as.factor(tempDF$BFO_0000015)) == "Diarrhea Episode") {
          myMin = 0
        } else {
          myMin <- min(tempDF[, (myOut), with=FALSE])
        }
        myMax <- max(tempDF[, (myOut), with=FALSE])
        
        #sliderInput("out_stp4", "Comparative value:",
        #            min = myMin, max = myMax, value = mean(tempDF[[myOut]], na.rm = T) , round=TRUE, width='100%')
        numericInput("out_stp4", NULL,
                     min = myMin, max = myMax, value = mean(tempDF[[myOut]]), width = '100%')
      }
    })      
 
    output$plot <- renderPlot({
      
        tableData <- plotData()
        if (is.null(tableData)) {
          message("plotData returned null!")
          return()
        } else {
          message("Yay!! I have plot data now :)")
        }
        
        #remove totals
        df <- tableData[ -nrow(tableData), ]
        df$Totals <- NULL
        #reorganize to make stacked bar
        df <- t(df)
        df <- rbind(df, c(df[1,2], NA))
        df <- rbind(df, c(df[2,2], NA))
        df <- cbind(c("Outcome+", "Outcome-", "Outcome+", "Outcome-"), df)
        colnames(df) <- c("Outcome" ,"Proportion", "Attribute")
        df <- data.table(df)
        df$Proportion = as.numeric(df$Proportion)
        df$Attribute <- c("Attribute+", "Attribute+", "Attribute-", "Attribute-")
        df$Outcome <- factor(df$Outcome, levels = c("Outcome+", "Outcome-"))
        df$Attribute <- factor(df$Attribute, levels = c("Attribute+", "Attribute-"))
        
        #define axis labels here
        xlab <- ""
        ylab <- "Proportion"
        
        #determine width of bars for outcome
        outPos <- tableData$`Outcome+`[3]
        outNeg <- tableData$`Outcome-`[3]
        total <- tableData$Totals[3]
        OPprop <- outPos / total
        ONprop <- outNeg / total
        width <- c( OPprop*1.9, ONprop*1.9, OPprop*1.9, ONprop*1.9)
        df <- data.table(df, width)
        
        myPlot <- ggplot(data = df, aes(x = Outcome, y = Proportion, width = width, fill = Attribute))
        myPlot <- myPlot + theme_bw()
        #myPlot <- myPlot + labs(y = ylab, x = xlab)
        
        #plot here
        myPlot <- myPlot + geom_bar(stat = "identity", position = "fill")
        myPlot <- myPlot + scale_fill_manual(values = c("#32baba", "#e26c6c"))
        
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
        #myPlotly <- ggplotly(myPlot)
        #myPlotly <- config(myPlotly, displaylogo = FALSE, collaborate = FALSE) %>% layout(xaxis = x_list, yaxis = y_list)
        
        myPlot
      
    })

    output$table <- DT::renderDataTable({
      data <- plotData()
      if (is.null(data)) {
        return()
      }

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
      #get pval
      p <- chisq.test(df)
      p <- p$p.value
      #get or
      a <- df$`Outcome+`[1]
      b <- df$`Outcome-`[1]
      c <- df$`Outcome+`[2]
      d <- df$`Outcome-`[2]
      OR <- (a*d)/(b*c)
      #get rr
      RR <- (a/(a+b)) / (c/(c+d))
      #if 10 digits or fewer dont use exponential notation
      options("scipen"=10)
      #round to 4 digits and for p-value show "<0.0001" when appropriate
      OR <- round(OR, digits=4)
      RR <- round(RR, digits=4)
      p <- round(p, digits=4)
      if (p < 0.0001) {
        p <- "<0.0001"
      }
      #make stats table
      stats <- data.table("p-value" = p, "Odds Ratio" = OR, "Relative Risk" = RR)
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
      out_stp2 <- input$out_stp2
      myTimeframe <- input$timeframe
      myAttr <- input$attr
      attr_stp1 <- input$attr_stp1
      attr_stp2 <- input$attr_stp2
      myOut <- input$out
      out_stp1 <- input$out_stp1
      out_stp3 <- input$out_stp3
      out_stp4 <- input$out_stp4
        
      #once last non-optional field is populated do some stuffs
      if (!is.null(input$out_stp2)) {
        
        #subset data
        data <- subsetDataFetcher(myTimeframe[1], myTimeframe[2])
        
        #get attr col
        attrData <- completeDT(data, myAttr)
        attrData <- getFinalDT(attrData, myAttr)
        myCols <- c("Participant_Id", myAttr)
        attrData <- attrData[, myCols, with=FALSE]
        
        #for numbers
        if (attr_stp1 == "lessThan") {
          attrData <- aggregate(attrData, by=list(attrData$Participant_Id), FUN = function(x){ if (any(x < attr_stp2)) {1} else {0} })
        } else if (attr_stp1 == "greaterThan") {
          attrData <- aggregate(attrData, by=list(attrData$Participant_Id), FUN = function(x){ if (any(x > attr_stp2)) {1} else {0} })
        } else if (attr_stp1 == "equals") {
          attrData <- aggregate(attrData, by=list(attrData$Participant_Id), FUN = function(x){ if (any(x == attr_stp2)) {1} else {0} })
          #for change over time  
        } else if (attr_stp1 == "delta") {
          if(is.null(input$attr_stp3)) {
            return()
          }
          attrData <- completeDT(data, myAttr)
          attrData <- getFinalDT(outData, myAttr)
          myCols <- c("Participant_Id", "EUPATH_0000644", myAttr)
          attrData <- attrData[, myCols, with=FALSE]
          #should start an empty table here to add values in as i go through the loop
          tempTable <- NULL
          #do the below for each participant. think i need for loop. :( but will see if i can think up better way.
          prtcpnts <- levels(as.factor(attrData$Participant_Id))
          for (i in prtcpnts) {
            currData <- subset(attrData, attrData$Participant_Id %in% i)
            
            startDay <- min(currData[, EUPATH_0000644])
            startVal <- currData[[myOut]][currData$EUPATH_0000644 == startDay]
            endVal <- currData[[myOut]][currData$EUPATH_0000644 == max(currData[, EUPATH_0000644])]
            diffVal <- startVal - endVal
            
            #if statement for direction of change
            if (startVal > endVal) {
              diffVal = diffVal * -1
            }
            
            if (attr_stp2 == "lessThan") {
              if (diffVal < attr_stp3) {
                row <- c(i, 1)
              } else {
                row <- c(i,0)
              }
            } else if (attr_stp2 == "greaterThan") {
              if (diffVal > attr_stp3) {
                row <- c(i,1)
              } else {
                row <- c(i,0)
              }
            } else {
              if (diffVal == attr_stp3) {
                row <- c(i,1)
              } else {
                row <- c(i,0)
              }
            }
            
            #add participant to growing data table for outcomes
            tempTable <- rbindlist(list(tempTable, as.list(row)))
          }
          #edit outdata so the merge with attr data works..
          attrData <- tempTable
          colnames(attrData) <- c("Participant_Id", "Outcome")
        } else if (attr_stp1 == "percentDays") {
          if (is.null(attr_stp4)) {
            return()
          }
          tempTable <- NULL
          #do the below for each participant. think i need for loop. :( but will see if i can think up better way.
          prtcpnts <- levels(as.factor(attrData$Participant_Id))
          for (i in prtcpnts) {
            currData <- subset(attrData, attrData$Participant_Id %in% i)
            
            if (attr_stp3 == "lessThan") {
              currData <- transform(currData, "Outcome" =  ifelse(currData[[myOut]] < attr_stp4,1 ,0))
            } else if (attr_stp3 == "greaterThan") {
              currData <- transform(currData, "Outcome" =  ifelse(currData[[myOut]] > attr_stp4,1 ,0))
            } else {
              currData <- transform(currData, "Outcome" =  ifelse(currData[[myOut]] == attr_stp4,1 ,0))
            }
            colnames(currData) <- c("Participant_Id", "drop", "Outcome")
            if ((sum(currData$Outcome)/length(currData$Outcome)*100) >= attr_stp2) {
              row <- c(i,1)
            } else {
              row <- c(i,0)
            }
            
            tempTable <- rbindlist(list(tempTable, as.list(row)))
          }
          attrData <- tempTable
          colnames(attrData) <- c("Participant_Id", "Outcome")  
        #for strings
        } else {
          attrData <- aggregate(attrData, by=list(attrData$Participant_Id), FUN = function(x){ if(any(grepl(attr_stp1, x, fixed=TRUE)) == TRUE) {1} else {0} })
        }
        if (ncol(attrData) > 2) {
          colnames(attrData) <- c("Participant_Id", "drop", "Attribute")
        }
       
        #get outcome data
        #may not need to do the splitting on pipes. grepl will still return true for it.
        outData <- completeDT(data, myOut)
        outData <- getFinalDT(outData, myOut)
        myCols <- c("Participant_Id", myOut)
        outData <- outData[, myCols, with=FALSE]
        
        #if anthro direct comparison do same as for number
        if (out_stp1 == "direct") {
          out_stp1 = out_stp2
          out_stp2 = out_stp3
        }
        #for numbers
        if (out_stp1 == "lessThan") {
          outData <- aggregate(outData, by=list(outData$Participant_Id), FUN = function(x){ if (any(x < out_stp2)) {1} else {0} })
        } else if (out_stp1 == "greaterThan") {
          outData <- aggregate(outData, by=list(outData$Participant_Id), FUN = function(x){ if (any(x > out_stp2)) {1} else {0} })
        } else if (out_stp1 == "equals") {
          outData <- aggregate(outData, by=list(outData$Participant_Id), FUN = function(x){ if (any(x == out_stp2)) {1} else {0} })
        #for change over time  
        } else if (out_stp1 == "delta") {
          if(is.null(input$out_stp3)) {
            return()
          }
          outData <- completeDT(data, myOut)
          outData <- getFinalDT(outData, myOut)
          myCols <- c("Participant_Id", "EUPATH_0000644", myOut)
          outData <- outData[, myCols, with=FALSE]
          #should start an empty table here to add values in as i go through the loop
          tempTable <- NULL
          #do the below for each participant. think i need for loop. :( but will see if i can think up better way.
          prtcpnts <- levels(as.factor(outData$Participant_Id))
          for (i in prtcpnts) {
            currData <- subset(outData, outData$Participant_Id %in% i)
              
              startDay <- min(currData[, EUPATH_0000644])
              startVal <- currData[[myOut]][currData$EUPATH_0000644 == startDay]
              endVal <- currData[[myOut]][currData$EUPATH_0000644 == max(currData[, EUPATH_0000644])]
              diffVal <- startVal - endVal
              
              #if statement for direction of change
              if (startVal > endVal) {
                diffVal = diffVal * -1
              }
              
              if (out_stp2 == "lessThan") {
                if (diffVal < out_stp3) {
                  row <- c(i, 1)
                } else {
                  row <- c(i,0)
                }
              } else if (out_stp2 == "greaterThan") {
                if (diffVal > out_stp3) {
                  row <- c(i,1)
                } else {
                  row <- c(i,0)
                }
              } else {
                if (diffVal == out_stp3) {
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
          colnames(outData) <- c("Participant_Id", "Outcome")
        } else if (out_stp1 == "percentDays") {
          if (is.null(out_stp4)) {
            return()
          }
          tempTable <- NULL
          #do the below for each participant. think i need for loop. :( but will see if i can think up better way.
          prtcpnts <- levels(as.factor(outData$Participant_Id))
          for (i in prtcpnts) {
            currData <- subset(outData, outData$Participant_Id %in% i)
            
            if (out_stp3 == "lessThan") {
              currData <- transform(currData, "Outcome" =  ifelse(currData[[myOut]] < out_stp4,1 ,0))
            } else if (out_stp3 == "greaterThan") {
              currData <- transform(currData, "Outcome" =  ifelse(currData[[myOut]] > out_stp4,1 ,0))
            } else {
              currData <- transform(currData, "Outcome" =  ifelse(currData[[myOut]] == out_stp4,1 ,0))
            }
            colnames(currData) <- c("Participant_Id", "drop", "Outcome")
            if ((sum(currData$Outcome)/length(currData$Outcome)*100) >= out_stp2) {
              row <- c(i,1)
            } else {
              row <- c(i,0)
            }
            
            tempTable <- rbindlist(list(tempTable, as.list(row)))
          }
          outData <- tempTable
          colnames(outData) <- c("Participant_Id", "Outcome")
        }  else {
          #for strings
          outData <- aggregate(outData, by=list(outData$Participant_Id), FUN = function(x){ if(any(grepl(out_stp1, x, fixed=TRUE)) == TRUE) {1} else {0} })
        }
        if (ncol(outData) > 2) {
          colnames(outData) <- c("Participant_Id", "drop", "Outcome")
        }
        
        #merge on participant id and keep all prtcpnts.
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
        
        tableData <- data.table("Outcome+" = OP, "Outcome-" = OF, "Totals" = totals)
        rownames(tableData) <- c("Attribute+", "Attribute-", "Totals")
        
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

})
