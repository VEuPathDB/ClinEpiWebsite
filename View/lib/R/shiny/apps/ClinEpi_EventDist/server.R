## server.r

require(shiny)
require(data.table)
require(plotly)
require(viridisLite)

source("../../lib/wdkDataset.R")
source("config.R")
source("functions.R")

#also need to figure out why the plots renders multiple times
#maybe let user define their own groups??
#reorganize ui and plotData to reflect newer apps structure

shinyServer(function(input, output, session) {
  
  event.file <- NULL
  prtcpnt.file <- NULL
  house.file <- NULL
  metadata.file <- NULL
  singleVarData <- NULL
  prevFacet <- NULL
  
  cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
 
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
  
    output$choose_groups <- renderUI({
      if (is.null(input$plotChoice)) {
        return()
      }
      if (is.null(input$xaxis)) {
        return()
      }
      myX <- input$xaxis
      plotChoice <- input$plotChoice
      
      if (plotChoice == 'groups'){
        groupsChoiceList <- getGroupsList(myX)
        if (myX == 'zscore') {
          selectList <- c('EUPATH_0000734', 'EUPATH_0000689')
        } else {
          selectList <- c('EUPATH_0000689')
        }
        
        #TODO change to selectize input
        dropdownButton(
          label = "Click here to select groups", status = "default",
          checkboxGroupInput("groups",
                             "Groups:", 
                             choiceNames  = names(groupsChoiceList),
                             choiceValues = unname(groupsChoiceList),
                             selected = selectList)
        )
      }
    })
    
    #reconsider this. still useful with plotly? replace with choose_timeframe and call singleVarDataFetcher there like in the others??
    output$choose_range <- renderUI({
      if (is.null(input$plotChoice)) {
        return()
      }
      if (is.null(input$xaxis)) {
        return()
      }
      myX <- input$xaxis
      myGroups <- input$groups
      plotChoice <- input$plotChoice
      nums <- getNums()
      
      if (plotChoice == 'groups') {
        tempDF <- groupsDataFetcher(myGroups, myX)
        
        myMin <- min(tempDF[, (myX), with=FALSE])
        myMax <- max(tempDF[, (myX), with=FALSE])
        
        sliderInput("range", "Range:",
                    min = myMin, max = myMax, value = c(myMin,myMax))
      } else {
        #whats the point of the first half of this if statement?? isnt that a number?
        if (myX %in% nums$source_id) {
          message("hmm")
          data <- singleVarData
          tempDF <- completeDT(data, myX)
      
          myMin <- min(tempDF[, (myX), with=FALSE])
          myMax <- max(tempDF[, (myX), with=FALSE])
        
          message(paste("in choose range function"))
      
          sliderInput("range", "Range:",
                    min = myMin, max = myMax, value = c(myMin,myMax), round=TRUE)
        }
      }
    })
    
    output$choose_xaxis <- renderUI({
      message("in choose_xaxis ui")
      if (!length(singleVarData)) {
        message(paste("fetching dt"))
        singleVarData <<- singleVarDataFetcher()
      }
  
      if (is.null(input$plotChoice)) {
        return()
      }
      plotChoice <- input$plotChoice
      
      if (plotChoice == 'groups') {
        getXList(plotChoice)
        selectInput(inputId = "xaxis",
                    label = "X-Axis:",
                    choices = list('Z-score' = 'zscore', 'Age in Days' = 'ageDays'),
                    selected = 'zscore') 
      } else {
        xChoiceList <- getXList()
        selectInput(inputId = "xaxis",
                    label = "X-Axis:",
                    choices = xChoiceList,
                    selected = "EUPATH_0000689")
      }
    })
    
    output$choose_facet <- renderUI({
      if (is.null(input$plotChoice)) {
        return()
      }
      if (is.null(input$xaxis)) {
        return()
      }
      plotChoice <- input$plotChoice
      myX <- input$xaxis
      myGroups <- input$groups
      
      facetChoiceList <- getFacetList()

      if (!is.null(prevFacet)) {
        if (prevFacet %in% facetChoiceList) {
          mySelected <- prevFacet
        } else {
          mySelected <- "EUPATH_0000452"
        }
      } else { 
        mySelected <- "EUPATH_0000452"
      }
      message("in facet ui")
      print(mySelected)
      
      selectInput(inputId = "facet",
                  label = "Facet:",
                  choices = facetChoiceList,
                  selected = mySelected)
           
    })
    
    output$distribution <- renderPlotly({
      if (is.null(input$xaxis)) {
        return()
      }
      if (is.null(input$plotChoice)) {
        return()
      }
      if (is.null(input$facet)) {
        return()
      }
      myX <- input$xaxis
      plotChoice <- input$plotChoice
      myMin <- input$range[1]
      myMax <- input$range[2]
      myFacet <- input$facet
      #should switch to same setup as in other two.. use plotData()
      df <- plotData()
   
      message(paste("just starting plot"))
       
      df <- completeDT(df, myX)
      
      nums <- getNums()

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
      myPlot <- myPlot + labs(y = "Count", x = xlab)
      
      if ( myX %in% nums$source_id  || plotChoice == 'groups'){
        myPlot <- myPlot + coord_cartesian(xlim=c(myMin,myMax))
      }
      
      if (plotChoice == 'groups') {
        #myPlot <- myPlot + geom_tooltip(aes(fill = groups, y = .3 * ..count.., tooltip=groups), alpha = .2, real.geom=geom_histogram)  
        myPlot <- myPlot + geom_density(aes(fill = groups, y = .3 * ..count.., text = paste0("Group: ", group , "<br>", "Count: ", .3 * ..count..)), alpha = .2) 
        #myPlot <- myPlot + scale_y_continuous(formatter = function(x) format(10 ^ x))
        if (length(levels(as.factor(df$groups))) > 12) {
          myPlot <- myPlot + theme(legend.position="none")
        }
        if (myFacet != 'none') {
          myPlot <- myPlot + facet_wrap(reformulate(myFacet), ncol = 1) 
           # scale_fill_brewer(palette = cbPalette)
        }
      } else {
        if (myX %in% nums$source_id) {
          #myPlot <- myPlot + geom_tooltip(aes(tooltip=paste0("count: ", ..count..)), fill = "#56B4E9", real.geom="geom_histogram")
          myPlot <- myPlot + geom_histogram(aes(text = paste0("Count: ", ..count..)), stat = "bin", fill = viridis(1, end = .75, direction = -1))
          myPlot <- myPlot + geom_vline(aes(xintercept = mean(df[[myX]], na.rm = T), text = paste0("mean:", mean(df[[myX]], na.rm = T))), color = viridis(1, begin = .25), linetype = "dashed", size = 1)
          if (myFacet != 'none') {
            myPlot <- myPlot + facet_wrap(reformulate(myFacet), ncol = 3)   
          }
        } else {
          #myPlot <- myPlot + geom_tooltip(aes(tooltip=paste0("count: ", ..count..)), stat = "count", fill = "#56B4E9", real.geom="geom_histogram")
          myPlot <- myPlot + geom_histogram(aes(text = paste0("Count: ", ..count..)), stat = "count", fill = viridis(1, end = .75, direction = -1))
          myPlot <- myPlot + theme(axis.text.x = element_text(angle = 90, hjust = 1))
          if(length(levels(as.factor(df[[myX]]))) < 7) {
            if (myFacet != 'none') {
              myPlot <- myPlot + facet_wrap(reformulate(myFacet), ncol = 3)   
            }
          } else {
            if (myFacet != 'none') {
              myPlot <- myPlot + facet_wrap(reformulate(myFacet), ncol = 1)   
            }
          }
        }
      }
            
      message(paste("c'est fini"))
      
      #should keep playing with this vs doing it with ggplot syntax. also see if facet_grid gets the axis labels alignment better
      x_list <- list(
        title = xlab,
        size = 14 
      )
      y_list <- list(
        title = "Count",
        size = 14
      )
      
      myPlotly <- ggplotly(myPlot, tooltip = c("text", "x"))
      myPlotly <- config(myPlotly, displaylogo = FALSE, collaborate = FALSE) %>% layout(xaxis = x_list, yaxis = y_list)
      
      myPlotly
    })

    
    #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    #datagrabbed through reactive expression for better control of reactive context
    
    plotData <- debounce(reactive({
      if (is.null(input$plotChoice)) {
        return()
      }
      plotChoice <- input$plotChoice
      if (is.null(input$xaxis)) {
        return()
      }
      if (is.null(input$facet)) {
        return()
      }
      myFacet <- input$facet
      prevFacet <<- myFacet
      myGroups <- input$groups
      myX <- input$xaxis
      
      strings <- subset(metadata.file, metadata.file$type == "string", "source_id")
      if (plotChoice == 'groups') {
        data <- groupsDataFetcher(myGroups, myX)
        col = 'groups'
        data <- setDT(data)[, lapply(.SD, function(x) unlist(tstrsplit(x, " | ", fixed=TRUE))), 
                            by = setdiff(names(data), eval(col))][!is.na(eval(col))]
      } else {
        data <- singleVarData
        if (myX %in% strings$source_id) {
          data <- setDT(data)[, lapply(.SD, function(x) unlist(tstrsplit(x, " | ", fixed=TRUE))), 
                              by = setdiff(names(data), eval(myX))][!is.na(eval(myX))]
        }
      }
     
      nums <- getNums()
      
      if (myFacet %in% nums$source_id) {
        data[[myFacet]] <- cut_number(data[[myFacet]],4)
      }
      
      data
      
    }), 2000)
    
    groupsDataFetcher <- function(myGroups, myX) {
      #since singlevar is default im assuming fles fetcher is already called. can add check later though to be safe.
      
      groupsData <- NULL
      nums <- getNums()
      
      if (myX == 'zscore') {
        for (i in myGroups) {
          merge1 <- data.table("Participant_Id" = event.file$Participant_Id, "placeholder" = event.file[[i]], "groups" = metadata.file$property[metadata.file$source_id == i])
          merge2 <- merge(merge1, prtcpnt.file, by = "Participant_Id")
          merge3 <- merge(merge2, house.file, by = "Participant_Id")
          names(merge3)[names(merge3) == 'placeholder'] <- myX
          draftDF <- completeDT(merge3, myX)
          groupsData <- rbind(groupsData, draftDF)
        }
      } else {
        for (i in myGroups) {
          if (i %in% nums$source_id) {
            merge1 <- data.table("Participant_Id" = event.file$Participant_Id, "placeholder" = event.file$EUPATH_0000644, 
                                 "groups" = paste0(metadata.file$property[metadata.file$source_id == i], ": ", cut(event.file[[i]], 5)))
            merge1 <- merge1[- grep("NA", merge1$groups),]
          } else {
            #right now this assumes that if a string the x axis is ageDays. when we have time as an option to we will need an if statement somewhere here
            merge1 <- data.table("Participant_Id" = event.file$Participant_Id, "placeholder" = event.file$EUPATH_0000644, "groups" = event.file[[i]])
            merge1 <- completeDT(merge1, "groups")
          }
          merge2 <- merge(merge1, prtcpnt.file, by = "Participant_Id")
          merge3 <- merge(merge2, house.file, by = "Participant_Id")
          names(merge3)[names(merge3) == 'placeholder'] <- myX
          groupsData <- rbind(groupsData, merge3)
        }
      }
      
      groupsData
    }
    
    #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    #below are functions dependent on the four original files pulled in or on singleVarData variable. could fix in future so can be put in other file (functions.R ??)
    
    getNums <- function(){
      #identify nums 
      nums <- subset(metadata.file, metadata.file$type == "number", "source_id") 
      
      nums
    }
    
    getDates <- function(){
      #identify nums 
      nums <- subset(metadata.file, metadata.file$type == "date", "source_id") 
      
      nums
    }
    
    getStrings <- function(){
      #identify nums 
      nums <- subset(metadata.file, metadata.file$type == "string", "source_id") 
      
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
      choiceList <- as.vector(c("none", choices$source_id))
      names(choiceList) <- as.vector(c("None", choices$property))
      facetlist <- as.list(choiceList)
      
      facetlist
    }
    
    getXList <- function(){
      xlist <- NULL
      data <- singleVarData

      xcolnames <- colnames(data)
      #get display names from metadata
      xchoices <- subset(metadata.file, source_id %in% xcolnames)
      myorder <- sort(xchoices$property)
      xchoices <- xchoices[match(myorder, xchoices$property),]
      #convert df to list
      xchoiceList <- as.vector(xchoices$source_id)
      names(xchoiceList) <- as.vector(xchoices$property)
      xlist <- as.list(xchoiceList)  
     
      xlist
    }
    
    getGroupsList <- function(myX) {
      
      if (myX == 'zscore') {
        groupsSubset <- metadata.file[grep("z-score", metadata.file$property), ]
      } else if (myX == 'ageDays'){
        colnames <- colnames(event.file[,4:29])
        groupsSubset <- subset(metadata.file, source_id %in% colnames)
      } else {
        return;
      }
      
      groupsChoiceList <- as.vector(groupsSubset$source_id)
      names(groupsChoiceList) <- as.vector(groupsSubset$property)
      
      groupsChoiceList
    }

})
