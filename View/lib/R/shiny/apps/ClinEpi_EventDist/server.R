## server.r

require(shiny)
require(data.table)
require(plotly)
require(viridisLite)

source("../../lib/wdkDataset.R")
source("config.R")
source("../../lib/ebrc_functions.R")
source("../../lib/clinepi_functions.R")

#also need to figure out why the plots renders multiple times
#maybe let user define their own groups??
#reorganize ui and plotData to reflect newer apps structure

shinyServer(function(input, output, session) {
  
  event.file <- NULL
  event.file.exists <- NULL
  prtcpnt.file <- NULL
  house.file <- NULL
  house.file.exists <- NULL
  metadata.file <- NULL
  singleVarData <- NULL
  prevFacet <- NULL
  
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
   message("done with singleVarDataFetcher!") 
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
        if (myX == 'zscore') {
          groupsSubset <- metadata.file[grep("z-score", metadata.file$property), ]
          cols <- groupsSubset$source_id
          useData <- singleVarData[, cols, with = FALSE]
        } else {
          if (event.file.exists) {
            useData <- event.file
          } else {
            useData <- singleVarData
          }
        }
        groupsChoiceList <- getUIList(useData, metadata.file)
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
      message("in choose_range ui")
      if (!length(singleVarData)) {
        message(paste("fetching dt"))
        singleVarData <<- singleVarDataFetcher()
      }

      if (is.null(input$plotChoice)) {
        return()
      }
      if (is.null(input$xaxis)) {
        return()
      }
      myX <- input$xaxis
      myGroups <- input$groups
      plotChoice <- input$plotChoice
      nums <- getNums(metadata.file)
      
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
  
      if (is.null(input$plotChoice)) {
        return()
      }
      plotChoice <- input$plotChoice
      
      if (plotChoice == 'groups') {
        #getXList(plotChoice)
        selectInput(inputId = "xaxis",
                    label = "X-Axis:",
                    choices = list('Z-score' = 'zscore', 'Age in Days' = 'ageDays'),
                    selected = 'zscore') 
      } else {
        #temporary until i figure out how to plot histograms with dates in plotly
        dates <- getDates(metadata.file)$source_id
        useData <- singleVarData[, -dates, with = FALSE]
        xChoiceList <- getUIList(useData, metadata.file)
        selectInput(inputId = "xaxis",
                    label = "X-Axis:",
                    choices = xChoiceList,
                    selected = "EUPATH_0000689")
      }
    })
    
    output$choose_facet <- renderUI({
      message("in choose_facet ui")
      if (is.null(input$plotChoice)) {
        return()
      }
      if (is.null(input$xaxis)) { 
        return()
      }
      plotChoice <- input$plotChoice
      myX <- input$xaxis
      myGroups <- input$groups
      
      if (house.file.exists) {
        useData <- list(prtcpnt.file, house.file)
        facetChoiceList <- lapply(useData, getUIList, metadata.file, minLevels = 2, maxLevels = 12, addNone = TRUE)
        facetChoiceList <- unlist(facetChoiceList, recursive = FALSE)
      } else {
        facetChoiceList <- getUIList(prtcpnt.file, metadata.file, minLevels = 2, maxLevels = 12, addNone = TRUE)
      }
      
      if (!is.null(prevFacet)) {
        if (prevFacet %in% facetChoiceList) {
          mySelected <- prevFacet
        } else {
          mySelected <- "OBI_0001627"
        }
      } else { 
        mySelected <- "OBI_0001627"
      }
      message("in facet ui")
      print(mySelected)
      
      selectInput(inputId = "facet",
                  label = "Facet:",
                  choices = facetChoiceList,
                  selected = mySelected)
           
    })
    
    output$distribution <- renderPlotly({
      message("render plot!")
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
        if (myX %in% nums$source_id | myX %in% dates$source_id) {
          #myPlot <- myPlot + geom_tooltip(aes(tooltip=paste0("count: ", ..count..)), fill = "#56B4E9", real.geom="geom_histogram")
          myPlot <- myPlot + geom_histogram(aes(text = paste0("Count: ", ..count..)), stat = "bin", fill = viridis(1, end = .25, direction = -1))
          myPlot <- myPlot + geom_vline(aes(xintercept = mean(df[[myX]], na.rm = T), text = paste0("mean:", mean(df[[myX]], na.rm = T))), color = viridis(1, begin = .75), linetype = "dashed", size = 1)
          if (myFacet != 'none') {
            myPlot <- myPlot + facet_wrap(reformulate(myFacet), ncol = 3)   
          }
        } else {
          #myPlot <- myPlot + geom_tooltip(aes(tooltip=paste0("count: ", ..count..)), stat = "count", fill = "#56B4E9", real.geom="geom_histogram")
          myPlot <- myPlot + geom_histogram(aes(text = paste0("Count: ", ..count..)), stat = "count", fill = viridis(1, end = .25, direction = -1))
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
      
      myPlotly <- ggplotly(myPlot, tooltip = c("text"))
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
     
      nums <- getNums(metadata.file)
      dates <- getDates(metadata.file)      

      if (myFacet %in% nums$source_id | myFacet %in% dates$source_id) {
        data[[myFacet]] <- cut(data[[myFacet]],4)
      }
      
      data
      
    }), 2000)
    
    groupsDataFetcher <- function(myGroups, myX) {
      #since singlevar is default im assuming fles fetcher is already called. can add check later though to be safe.
      
      groupsData <- NULL
      nums <- getNums(metadata.file)
      
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
    
})
