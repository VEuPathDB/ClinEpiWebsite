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

#are there any stats evidence for significance we could do?
#if there is no timeframe info (agedays or dates) consider a box plot instead. make sure that idea isnt redundant with any other apps or anything, and makes sense to do
#for facets ui: consider selecting more than one group/ check boxes ??
#make sure levels for numeric groups always start with the group that has square bracket in front (meaning smallest -> largest)
#figure out when we need naToZero function when building own groups and facets. imagine need it for events stuffs but not others ??
# for some reason delta laz > -.5 returns fewer ppl than >-1.. seems should be reversed

#options(shiny.sanitize.errors = TRUE)  

shinyServer(function(input, output, session) {
  
  event.file <- NULL
  event.file.exists <- NULL
  prtcpnt.file <- NULL
  house.file <- NULL
  house.file.exists <- NULL
  metadata.file <- NULL
  singleVarData <- NULL
 
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
    
    if (exists("event.file")) {
      if (!is.null(event.file) & nrow(event.file) > 1) {
        merge1 <- merge(event.file, prtcpnt.file)
        event.file.exists <<- TRUE
      } else {
        merge1 <- prtcpnt.file
        event.file.exists <<- FALSE
      }
    } else {
      merge1 <- prtcpnt.file
      event.file.exists <<- FALSE
    }
    
    if (exists("house.file")) {
      if (!is.null(house.file) & nrow(house.file) > 1) { 
        house.file <<- house.file[, -(drop), with = FALSE]
        singleVarData <<- merge(merge1, house.file)
        house.file.exists <<- TRUE
      } else {
        singleVarData <<- merge1
        house.file.exists <<- FALSE
      }
    } else {
      singleVarData <<- merge1
      house.file.exists <<- FALSE
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
  
  outRange <- reactiveValues()
  facetRange <- reactiveValues()
  
  setOutVals <- reactive({
    myGroups <- input$groups
    nums <- getNums(metadata.file)
    dates <- getDates(metadata.file)
    
    if (myGroups %in% nums$source_id | myGroups %in% dates$source_id) {
      data <- singleVarData
      tempDF <- completeDT(data, myGroups)
      
      if (any(colnames(event.file) %in% myGroups) & any(colnames(tempDF) %in% "BFO_0000015")) {
        if (levels(as.factor(tempDF$BFO_0000015)) == "Diarrhea Episode") {
          outRange$myMin = 0
        }
      } else {
        outRange$myMin <- min(tempDF[[myGroups]])
      }
      outRange$myMax <- max(tempDF[[myGroups]])
      
      if (myGroups %in% nums$source_id) {
        outRange$mean <- mean(tempDF[[myGroups]])
      } else {
        outRange$startDate <- as.Date(quantile(as.POSIXct(tempDF[[myGroups]]), .25))
        outRange$endDate <- as.Date(quantile(as.POSIXct(tempDF[[myGroups]]), .75))
        outRange$myMin <- as.Date(outRange$myMin)
        outRange$myMax <- as.Date(outRange$myMax)
        message(paste("start and end dates:", outRange$startDate, outRange$endDate))
      }
    }
    
  })
  
  setFacetVals <- reactive({
    myFacet <- input$facet
    nums <- getNums(metadata.file)
    dates <- getDates(metadata.file)
    
    if (myFacet %in% nums$source_id | myFacet %in% dates$source_id) {
      data <- singleVarData
      tempDF <- completeDT(data, myFacet)
      
      if (any(colnames(event.file) %in% myFacet) & any(colnames(tempDF) %in% "BFO_0000015")) {
        if (levels(as.factor(tempDF$BFO_0000015)) == "Diarrhea Episode") {
          facetRange$myMin = 0
        }
      } else {
        facetRange$myMin <- min(tempDF[[myFacet]])
      }
      facetRange$myMax <- max(tempDF[[myFacet]])
      
      if (myFacet %in% nums$source_id) {
        facetRange$mean <- mean(tempDF[[myFacet]])
      } else {
        facetRange$startDate <- as.Date(quantile(as.POSIXct(tempDF[[myFacet]]), .25))
        facetRange$endDate <- as.Date(quantile(as.POSIXct(tempDF[[myFacet]]), .75)) 
        facetRange$myMin <- as.Date(facetRange$myMin)
        facetRange$myMax <- as.Date(facetRange$myMax)
        message(paste("facet start and end dates:", facetRange$startDate, facetRange$endDate))
      }
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
      
      if (any(colnames(singleVarData) %in% ageDays)) {
        tempDF <- completeDT(data, ageDays)
        
        
        myMin <- min(tempDF[, (ageDays), with=FALSE])
        myMax <- max(tempDF[, (ageDays), with=FALSE]) 
        
        sliderInput("timeframe", "Timeframe:",
                    min = myMin, max = myMax, value = c(myMin,myMax), round=TRUE, width = '100%')
      }
        
    })
    
    output$groups_type <- renderUI({
      ageDays <- "EUPATH_0000644"
      
      if (any(colnames(singleVarData) %in% ageDays)) {
        selectInput(inputId = "groupsType",
                    label = "Facet Line:",
                    choices = c("All possible" = "direct", "Make my own" = "makeGroups"),
                    selected = "direct",
                    width = '100%')
      } else {
        selectInput(inputId = "groupsType",
                    label = "X-Axis:",
                    choices = c("All possible" = "direct", "Make my own" = "makeGroups"),
                    selected = "direct",
                    width = '100%')
      }
      
    })
    
    output$facet_type <- renderUI({
      selectInput(inputId = "facetType",
                  label = "Facet Plot:",
                  choices = c("All possible" = "direct", "Make my own" = "makeGroups", "None" = "none"),
                  selected = "none",
                  width = '100%')
    })
    
    output$choose_groups <- renderUI({
      if (is.null(input$groupsType)) {
        return()
      } else {
        groupsType <- input$groupsType
      }
      ageDays <- "EUPATH_0000644"
      
      if (any(colnames(singleVarData) %in% ageDays)) {
        if (groupsType == "direct") {

          if (house.file.exists) {
            useData <- list(prtcpnt.file, house.file)
            groupChoiceList <- lapply(useData, getUIList, metadata.file = metadata.file)
            groupChoiceList <- unlist(groupChoiceList, recursive = FALSE)
          } else {
            groupChoiceList <- getUIList(prtcpnt.file, metadata.file)
          }

          selectInput(inputId = "groups",
                      label = "facets for",
                      choices = groupChoiceList,
                      selected = "EUPATH_0000744",
                      width = '100%')
        } else {
          groupChoiceList <- getUIList(singleVarData, metadata.file)
          selectInput(inputId = "groups",
                      label = "facet where:",
                      choices = groupChoiceList,
                      selected = "EUPATH_0000704",
                      width = '100%')
        }
      } else {
        if (groupsType == "direct") {

          if (house.file.exists) {
            useData <- list(prtcpnt.file, house.file)
            groupChoiceList <- lapply(useData, getUIList, metadata.file = metadata.file)
            groupChoiceList <- unlist(groupChoiceList, recursive = FALSE)
          } else {
            groupChoiceList <- getUIList(prtcpnt.file, metadata.file)
          }

          selectInput(inputId = "groups",
                      label = "x-axis categories for",
                      choices = groupChoiceList,
                      selected = "EUPATH_0000744",
                      width = '100%')
        } else {
          groupChoiceList <- getUIList(singleVarData, metadata.file)
          selectInput(inputId = "groups",
                      label = "x-axis category where:",
                      choices = groupChoiceList,
                      selected = "EUPATH_0000704",
                      width = '100%')
        }
      }
      
    })
    
    output$choose_facet <- renderUI({
      if (is.null(input$facetType)) {
        return()
      } else {
        facetType <- input$facetType
      }
      
      if (facetType == "direct") {

        if (house.file.exists) {
          useData <- list(prtcpnt.file, house.file)
          facetChoiceList <- lapply(useData, getUIList, metadata.file = metadata.file, minLevels = 2, maxLevels = 12)
          facetChoiceList <- unlist(facetChoiceList, recursive = FALSE)
        } else {
          facetChoiceList <- getUIList(prtcpnt.file, metadata.file, minLevels = 2, maxLevels = 12)
        }

        selectInput(inputId = "facet",
                    label = "facets for",
                    choices = facetChoiceList,
                    selected = "EUPATH_0000452",
                    width = '100%')
      } else if (facetType != "none") {
        #will have to change selected here TODO
        facetChoiceList <- getUIList(singleVarData, metadata.file)
        selectInput(inputId = "facet",
                    label = "facet where:",
                    choices = facetChoiceList,
                    selected = "EUPATH_0000452",
                    width = '100%')
      }
      
    })
    
    output$choose_yaxis <- renderUI({
      
      #this list should contain anything from events file
      if (event.file.exists) {
        useData <- event.file
      } else {
        useData <- singleVarData
      }
      outChoiceList <- getUIList(useData, metadata.file)
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
      attrStp1List <- getUIStp1List(singleVarData, myY)
      nums <- getNums(metadata.file)
      
      if (!myY %in% nums$source_id) {
        selectInput(inputId = "yaxis_stp1",
                    label = "for",
                    choices = attrStp1List,
                    width = '100%') 
      }
      
    })
    
    output$yaxis_stp2 <- renderUI({
      if (is.null(input$yaxis)) {
        return()
      } else {
        myY <- input$yaxis
      }
      nums <- getNums(metadata.file)
      ageDays <- "EUPATH_0000644"
      
      if (myY %in% nums$source_id) {
        if (any(colnames(singleVarData) %in% ageDays)) {
          radioButtons(inputId = "yaxis_stp2",
                       label = "Display as:",
                       choices = list("Mean" = "mean", "Smoothed Conditional Mean" = "smooth"),
                       selected = "smooth",
                       width = '100%',
                       inline = TRUE)
        } else {
          #later think up better way. shouldnt show something with only one option. but this is a required input later on. will need if statements below instead.
          radioButtons(inputId = "yaxis_stp2",
                       label = "Display as:",
                       choices = list("Mean" = "mean"),
                       selected = "mean",
                       width = '100%',
                       inline = TRUE)
        }
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
      nums <- getNums(metadata.file)
      dates <- getDates(metadata.file)
      
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
          } else if (myGroups %in% dates$source_id) {
            dateRangeInput(inputId = "groups_stp1",
                           label = "is between",
                           start = outRange$startDate, end = outRange$endDate,
                           min = outRange$myMin, max = outRange$myMax,
                           separator = "and",
                           startview = "year")
          } else {
            outStp1List <- getUIStp1List(singleVarData, myGroups)
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
        } else if (myGroups %in% dates$source_id) {
          dateRangeInput(inputId = "groups_stp1",
                         label = "is between",
                         start = outRange$startDate, end = outRange$endDate,
                         min = outRange$myMin, max = outRange$myMax,
                         separator = "and",
                         startview = "year")
        } else {
          outStp1List <- getUIStp1List(singleVarData, myGroups)
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
      nums <- getNums(metadata.file)
      dates <- getDates(metadata.file)
      
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
          } else if (myFacet %in% dates$source_id) {
            dateRangeInput(inputId = "facet_stp1",
                           label = "is between",
                           start = facetRange$startDate, end = facetRange$endDate,
                           min = facetRange$myMin, max = facetRange$myMax,
                           separator = "and",
                           startview = "year")
          } else {
            outStp1List <- getUIStp1List(singleVarData, myFacet)
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
        } else if (myFacet %in% dates$source_id) {
          dateRangeInput(inputId = "facet_stp1",
                         label = "is between",
                         start = facetRange$startDate, end = facetRange$endDate,
                         min = facetRange$myMin, max = facetRange$myMax,
                         separator = "and",
                         startview = "year")
        } else {
          outStp1List <- getUIStp1List(singleVarData, myFacet)
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
      message(paste("class group stp1:", class(myStp1Val)))
      if (!any(c("POSIXct", "Date") %in% class(myStp1Val))) {
        if (myStp1Val == "percentDays") {
          sliderInput("groups_stp4", NULL,
                      min = outRange$myMin, max = outRange$myMax, value = outRange$mean, step = .1, width='100%')
        }
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
      
      if (!any(c("POSIXct", "Date") %in% class(myStp1Val))) {
        if (myStp1Val == "percentDays") {
          sliderInput("facet_stp4", NULL,
                      min = facetRange$myMin, max = facetRange$myMax, value = facetRange$mean, step = .1, width='100%')
        }
      }
    })
    
    output$plot <- renderPlotly({
      if (is.null(input$yaxis_stp2)) {
        return()
      } else {
        plotType <- input$yaxis_stp2
      }
      ageDays <- "EUPATH_0000644"
      
        #get data from plotData here
        df <- plotData()
      
        if (is.null(df)) {
          message("plotData returned null!")
          return()
        } 
        
        names(df)[names(df) == 'GROUPS'] <- 'LINES'
        #temp placeholder for checking if data has time vars for x axis
        if (any(colnames(singleVarData) %in% ageDays)) {
          #define axis labels here
          xlab <- "Time"
          #test if numeric, if yes then "Mean" else proportion if vals between 0 and 1 otherwise "Count"
          if (plotType == "proportion") {
            ylab <- "Proportion"
          } else if (plotType == "count") {
            ylab <- "Count"
          } else {
            ylab <- "Mean"
            df$YAXIS <- as.numeric(df$YAXIS)
          }
          
          #format xaxis ticks
          df$XAXIS <- as.numeric(gsub("\\[|\\]", "", sub(".*,", "", df$XAXIS)))
          
          #plot here
          myPlot <- ggplot(data = df, aes(x = XAXIS, y = YAXIS, group = LINES,  color = LINES))
          myPlot <- myPlot + theme_bw()
          myPlot <- myPlot + labs(y = ylab, x = xlab)
          message(paste("plot type:", plotType))
          #add the lines
          if (plotType == "proportion" | plotType == "count") {
            myPlot <- myPlot + geom_line(size = 1)
          } else if (plotType == "mean") {
            message("plotting mean")
            myPlot <- myPlot + stat_summary(fun.data = function(x){c( "y" = median(x, na.rm = TRUE), "ymax" = max(x, na.rm = TRUE), "ymin" = min(x, na.rm = TRUE))})
            myPlot <- myPlot + stat_summary(fun.y=function(x){y <- quantile(x, .25, na.rm = TRUE)})
            myPlot <- myPlot + stat_summary(fun.y=function(x){y <- quantile(x, .75, na.rm = TRUE)})
            myPlot <- myPlot + stat_summary(fun.y = mean, geom="line", size = 1)
          } else {
            message("plotting smooth")
            #myPlot <- myPlot + geom_point()
            myPlot <- myPlot + stat_summary(fun.data = function(x){c( "y" = median(x, na.rm = TRUE), "ymax" = max(x, na.rm = TRUE), "ymin" = min(x, na.rm = TRUE))})
            myPlot <- myPlot + stat_summary(fun.y=function(x){y <- quantile(x, .25, na.rm = TRUE)})
            myPlot <- myPlot + stat_summary(fun.y=function(x){y <- quantile(x, .75, na.rm = TRUE)})
            #myPlot <- myPlot + quantile()
            myPlot <- myPlot + geom_smooth(span = .3, na.rm = TRUE)
          }
         
          numColors <- length(levels(as.factor(df$LINES)))
          
        } else {
          names(df)[names(df) == 'LINES'] <- 'XAXIS'
          # if y axis is numeric box plots otherwise bar pltos.
          #define axis labels here
          xlab <- ""
          #test if numeric, if yes then "Mean" else proportion if vals between 0 and 1 otherwise "Count"
          if (plotType == "proportion") {
            ylab <- "Proportion"
          } else if (plotType == "count") {
            ylab <- "Count"
          } else {
            ylab <- "Mean"
            df$YAXIS <- as.numeric(df$YAXIS)
          }
          
          df$XAXIS <- as.factor(df$XAXIS) 
          #plot here
          myPlot <- ggplot(data = df, aes(x = XAXIS, y = YAXIS, fill = XAXIS))
          myPlot <- myPlot + theme_bw()
          myPlot <- myPlot + labs(y = ylab, x = xlab)
          message(paste("plot type:", plotType))
          #add the lines
          if (plotType == "proportion") {
            myPlot <- myPlot + geom_bar(stat = "identity")
            myPlot <- myPlot + scale_y_continuous(limits = c(0,1))
          } else if (plotType == "count") {
            myPlot <- myPlot + geom_bar(stat = "identity")
          } else {
            message("plotting mean")
            myPlot <- myPlot + geom_boxplot()
          }

          numColors <- length(levels(as.factor(df$XAXIS)))   

        }
        
        #add facet if available
        if (any(colnames(df) %in% "FACET")) {
          myPlot <- myPlot + facet_wrap(~ FACET, ncol = 1)
        }
        
        #find num colors needed
        if (numColors > 2) { 
          myPlot <- myPlot + scale_color_manual(values = viridis(numColors))
          myPlot <- myPlot + scale_fill_manual(values = viridis(numColors))
        } else if (numColors == 2) {
          myPlot <- myPlot + scale_color_manual(values = viridis(numColors, begin = .25, end = .75))
          myPlot <- myPlot + scale_fill_manual(values = viridis(numColors, begin = .25, end = .75))
        } else {
          myPlot <- myPlot + scale_color_manual(values = viridis(numColors, begin = .5))
          myPlot <- myPlot + scale_fill_manual(values = viridis(numColors, begin = .5))
        }
     
        #should keep playing with this vs doing it with ggplot syntax. 
        x_list <- list(
          title = xlab,
          size = 14 
        )
        y_list <- list(
          title = ylab,
          size = 14
        )
        
        myPlotly <- ggplotly(myPlot, tooltip = c("text", "x", "y"))
        #myPlotly <- ggplotly(myPlot)
        myPlotly <- config(myPlotly, displaylogo = FALSE, collaborate = FALSE) %>% layout(xaxis = x_list, yaxis = y_list)
        
        myPlotly
      
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
        colnames(data)[1] <- "Line"
        colnames(data) <- gsub("Participant_Id.", "# Participants: ", colnames(data))
        #give totals
        data[, "Totals"] <- rowSums(data[, -1], na.rm=TRUE)
        rownames(data) <- data[,1]
        data[,1] <- NULL
        data["Totals" ,] <- colSums(data, na.rm=TRUE)
        data <- cbind("Line" = rownames(data), data)
      } else {
        data <- aggregate(Participant_Id ~ GROUPS, data, FUN = function(x){ length(unique(x)) } )
        colnames(data) <- c("Line", "# Participants")
        #totals
        levels(data$Line) <- c(levels(as.factor(data$Line)),"Totals")
        data <- rbind(data, c("Totals", sum(data[, -1])))
      }
      
      #fix custom groups if necessary
      if (all(unique(data$Line[-nrow(data)]) %in% c(1,0))) {
        data <- as.data.table(data)
        data <- transform(data, "Line" = ifelse(Line == 1, "Positive", "Negative"))
        #data$Group <- data$temp
        #data <- data[, -"temp"]
        data$Group[nrow(data)] <- "Totals"
      }
    
      ageDays <- "EUPATH_0000644" 
      #temp placeholder for checking if data has time vars for x axis
      if (!any(colnames(singleVarData) %in% ageDays)) {
        names(data)[names(data) == 'Line'] <- 'X-Axis'
      } 

      datatable(data, 
                rownames = FALSE
      )
    })

    observeEvent(tableData(), tableData())
    
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
      yaxis_stp2 <- input$yaxis_stp2
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
      message("have all inputs for plotData")
      #subset data
      #which cols can be used for this will have to change. too specific right now
      if (any(colnames(singleVarData) %in% "EUPATH_0000644")) {
        if (!is.null(myTimeframe)) {
          data <- subsetDataFetcher(myTimeframe[1], myTimeframe[2])
          message("subsetting data..")
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
      } else {
        if (groupsType == "makeGroups") {
          if (is.null(groups_stp1)) {
            go <- FALSE
          }
        } 
      }
      if (is.null(facetType)) {
        go <- FALSE
      } else {
        if (facetType != "none") {
          if (is.null(myFacet)) {
            go <- FALSE
          }
        }
      }
      if (is.null(myY)) {
        go <- FALSE
      } else {
        if (myY %in% strings$source_id) {
          if (is.null(yaxis_stp1)) {
            go <- FALSE
          }
        }
      }
      if (is.null(yaxis_stp2)) {
        go <- FALSE
      }
      if (is.null(myGroups)) {    
        go <- FALSE
      }
      
      #once last field is populated .. GO
      if (go) {
        message("GO!!")
        #may not need to do the splitting on pipes. grepl will still return true for it.
        #should have natozero before this for non-anthro events?? so an NA for diarrhea -> 0 ?? 
        plotData <- completeDT(data, myY)
        plotData <- getFinalDT(plotData, metadata.file, myY)
        
        if (any(colnames(plotData) %in% "EUPATH_0000644")) {
          myCols <- c("Participant_Id", myY, "EUPATH_0000644")
          tempData <- plotData[, myCols, with=FALSE] 
          colnames(tempData) <- c("Participant_Id", "YAXIS", "XAXIS")
        } else {
          myCols <- c("Participant_Id", myY)
          tempData <- plotData[, myCols, with=FALSE] 
          colnames(tempData) <- c("Participant_Id", "YAXIS")
        }
        
        if (groupsType == "direct") {
          message("groups is direct")
          #check which column to pull for time, for now written in as EUPATH_0000644
          myCols <- c("Participant_Id", myGroups)
          groupData <- plotData[, myCols, with=FALSE]
          groupData <- unique(groupData)
          colnames(groupData) <- c("Participant_Id", "GROUPS")
          tempData <- merge(tempData, groupData, by = "Participant_Id")
        } 
        
        if (facetType == "direct") {
          message("facet is direct")
          #check which column to pull for time, for now written in as EUPATH_0000644
          myCols <- c("Participant_Id", myFacet)
          facetData <- plotData[, myCols, with=FALSE]
          facetData <- unique(facetData)
          colnames(facetData) <- c("Participant_Id", "FACET")
          tempData <- merge(tempData, facetData, by = "Participant_Id")
        } 
        
        plotData <- tempData
        #need better way. too specific right now. just need to know if xaxis is time
        #consider what to do about cinning for actual dates. will that work??
        if (any(colnames(singleVarData) %in% "EUPATH_0000644")) {
          plotData$XAXIS <- cut(plotData$XAXIS, 24) 
          message("binning xaxis data")
        }
        
        #bin facet if numeric and same for group 
        nums <- getNums(metadata.file)
        dates <- getDates(metadata.file)
        if (any(colnames(plotData) %in% "FACET")) {
          if (myFacet %in% nums$source_id | myFacet %in% dates$source_id) {
            message("bin facet cause its numeric")
            if (length(levels(as.factor(plotData$FACET))) >= 4) {
              plotData$FACET <- ggplot2::cut_number(plotData$FACET, 3)
            }
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
            outData <- makeGroups(data, metadata.file, myFacet, facet_stp1, facet_stp2, facet_stp3, facet_stp4)
            label <- makeGroupLabel(myFacet, metadata.file, facet_stp1, facet_stp2, facet_stp3, facet_stp4)
            message(paste("label is:", label))
            message("have custom facet! now merge..")
            print(head(outData))
            #add makeGroups data to df and return
            colnames(outData) <- c("Participant_Id", "FACET")
            #will need a var called label that changes based on what the facet steps are. the below only works for strings.
            if (any(colnames(event.file) %in% myFacet)) {
              naToZero(outData, "FACET")
            }
            print(head(outData))
            message(paste("levels facet:", levels(as.factor(outData$FACET))))
            outData <- transform(outData, "FACET" = ifelse(as.numeric(FACET) == 0, label[2], label[1]))
           # outData$FACET <- factor(outData$FACET, levels(c("Other", facet_stp2)))
            print(head(outData))
            message(paste("levels facet:", levels(as.factor(outData$FACET))))
            plotData <- merge(plotData, outData, by = "Participant_Id", all = TRUE)
            print(head(plotData))
            message(paste("levels facet:", levels(as.factor(plotData$FACET))))
          }
        }
        #if groups col exists return
        if (any(colnames(plotData) %in% "GROUPS")) {
          if (myGroups %in% nums$source_id | myGroups %in% dates$source_id) {
            if (length(levels(as.factor(plotData$GROUPS))) >= 4) {
              hold <- try(ggplot2::cut_number(plotData$GROUPS, 4))
              if (!grepl("Error", hold[1])){
                plotData$GROUPS <- hold
              }
            } 
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
          outData <- makeGroups(data, metadata.file, myGroups, groups_stp1, groups_stp2, groups_stp3, groups_stp4)
          label <- makeGroupLabel(myGroups, metadata.file, groups_stp1, groups_stp2, groups_stp3, groups_stp4)
          message(paste("label is:", label))
          if (any(colnames(event.file) %in% myGroups)) {
            naToZero(plotData, "GROUPS")
          }
          message("have custom groups! now merge..")
          #add makeGroups data to df and return
          print(head(outData))
          outData <- transform(outData, "GROUPS" = ifelse(as.numeric(GROUPS) == 0, label[2], label[1]))
          plotData <- merge(plotData, outData, by = "Participant_Id", all = TRUE)
          print(head(plotData))
          print("NA in groups:", any(is.na(plotData$GROUPS)))
        }
        plotData
      }
      
      #debounce will wait 2s with no changes to inputs before plotting.
    }), 2500)
      
    plotData <- reactive({  
      message("just before tableData call")
        plotData <- tableData()
        if (is.null(plotData)) {
          message("oh no! tableData returned null!")
          return()
        } else {
          #collecting inputs .. i think these are the only ones i need here.. well see
          myY <- input$yaxis
          message(paste("my y from plotData:", myY))
          if (is.null(input$yaxis_stp2)) {
            return()
          } else {
            yaxis_stp2 <- input$yaxis_stp2
          }
          yaxis_stp1 <- input$yaxis_stp1
          
          strings <- subset(metadata.file, metadata.file$type == "string", "source_id")
        
        message("and format rest of data .. being lazy and not including custom messages with each step :(")
        #prepare for return
        
        #determine necessary column id vectors before start
        if (any(colnames(plotData) %in% "FACET")) {
          if (any(colnames(plotData) %in% "XAXIS")) {
            aggStr1 <- "YAXIS ~ GROUPS + XAXIS + FACET"
            aggStr2 <- "Participant_Id ~ GROUPS + FACET"
            sumCols <- c("GROUPS", "FACET", "SUM")
            mergeBy <- c("GROUPS", "FACET")
            dropCols <- c("Participant_Id", "YAXIS")
            mergeBy2 <- c("GROUPS", "XAXIS", "FACET")
          } else {
            aggStr1 <- "YAXIS ~ GROUPS + FACET"
            aggStr2 <- "Participant_Id ~ GROUPS + FACET"
            sumCols <- c("GROUPS", "FACET", "SUM")
            mergeBy <- c("GROUPS", "FACET")
            dropCols <- c("Participant_Id", "YAXIS")
            mergeBy2 <- c("GROUPS", "FACET")
          }
        } else {
          if (any(colnames(plotData) %in% "XAXIS")) {
            aggStr1 <- "YAXIS ~ GROUPS + XAXIS"
            aggStr2 <- "Participant_Id ~ GROUPS"
            sumCols <- c("GROUPS", "SUM")
            mergeBy <- c("GROUPS")
            dropCols <- c("Participant_Id", "YAXIS")
            mergeBy2 <- c("GROUPS", "XAXIS")  
          } else {
            aggStr1 <- "YAXIS ~ GROUPS"
            aggStr2 <- "Participant_Id ~ GROUPS"
            sumCols <- c("GROUPS", "SUM")
            mergeBy <- c("GROUPS")
            dropCols <- c("Participant_Id", "YAXIS")
            mergeBy2 <- c("GROUPS")
          }
        }
        
        if (myY %in% strings$source_id) {
          #will have to replace all instances of myY with 1 and all else with 0 before can sum
          plotData <- transform(plotData, "YAXIS" = ifelse(YAXIS == yaxis_stp1, 1, 0))
          mergeData <- aggregate(as.formula(aggStr1), plotData, sum)
          if (yaxis_stp2 == "proportion") {
            groupSum <- as.data.table(aggregate(as.formula(aggStr2), plotData, FUN = function(x){length(unique(x))}))
            colnames(groupSum) <- sumCols
            mergeData <- as.data.table(mergeData)
            mergeData <- merge(mergeData, groupSum, by = mergeBy)
            proportion <- mergeData$YAXIS / mergeData$SUM
            mergeData$YAXIS <- proportion
            mergeData$SUM <- NULL
          }
        }
        if (yaxis_stp2 == "smooth" | yaxis_stp2 == "mean") {
          plotData$Participant_Id <- NULL
          plotData <- unique(plotData)
        } else {
          plotData <- plotData[, -dropCols, with=FALSE]
          plotData <- unique(plotData)
          print(head(plotData))
          print(head(mergeData))
          plotData <- merge(plotData, mergeData, by = mergeBy2)
        }
        plotData <- unique(plotData)
        if (all(as.numeric(levels(as.factor(plotData$GROUPS))) %in% c(1,0))) {
          message("rename strings to Positive and Negative from 1 and 0")
          plotData <- transform(plotData, "GROUPS" = ifelse(GROUPS == 1, "Positive", "Negative"))
        }
        message("return!")
        plotData
        }
        
    })
    
})
