## server.r

require(shiny)
require(ggplot2)

source("../../lib/wdkDataset.R")
source("config.R")

shinyServer(function(input, output, session) {
  
  event.file <- NULL
  prtcpnt.file <- NULL
  house.file <- NULL
  metadata.file <- NULL
  singleVarData <- NULL
  
  completeDF <- function(data, desiredCols) {
    completeVec <- complete.cases(data[, desiredCols])
    return(data[completeVec, ])
  }
  
  cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
 
  filesFetcher <- reactive({
   
    prtcpnt.file <<- read.csv(
          getWdkDatasetFile('ShinyParticipants.tab', session, FALSE, dataStorageDir),
          sep = "\t",
          check.names = FALSE, 
          na.strings = "N/A")

    house.file <<- read.csv(
          getWdkDatasetFile('ShinyHouseholds.tab', session, FALSE, dataStorageDir),
          sep = "\t",
          check.names = FALSE, 
          na.strings = "N/A")

    event.file <<- read.csv(
          getWdkDatasetFile('ShinyEvents.tab', session, FALSE, dataStorageDir),
          sep = "\t", 
          check.names = FALSE, 
          na.strings = "N/A")

    metadata.file <<- read.csv(
          getWdkDatasetFile('ontologyMetadata.tab', session, FALSE, dataStorageDir),
          sep = "\t", 
          check.names = FALSE)

    #this being done for now while i work in rstudio
    #prtcpnt.file <<- read.csv("ParticipantsByRelativeVisits_maled_ShinyParticipants.txt", sep = "\t", check.names = FALSE, na.strings = "N/A")
    #house.file <<- read.csv("ParticipantsByRelativeVisits_maled_ShinyHouseholds.txt", sep = "\t", check.names = FALSE, na.strings = "N/A")
    #event.file <<- read.csv("ParticipantsByRelativeVisits_maled_ShinyEvents.txt", sep = "\t", check.names = FALSE, na.strings = "N/A")
    #metadata.file <<- read.csv("Metadata_maled_Shiny.tsv", sep = "\t", check.names = FALSE)
  
    #make internal column names without brackets or spaces
    names(event.file) <<-  gsub(" ", "_", gsub("\\[|\\]", "", names(event.file)))
    names(prtcpnt.file) <<-  gsub(" ", "_", gsub("\\[|\\]", "", names(prtcpnt.file)))
    names(house.file) <<-  gsub(" ", "_", gsub("\\[|\\]", "", names(house.file)))
    names(metadata.file) <<- gsub(" ", "_", tolower(gsub("\\[|\\]", "", names(metadata.file))))
    
  })
  
  groupsDataFetcher <- function(myGroups, myX) {
    #since singlevar is default im assuming fles fetcher is already called. can add check later though to be safe.
  
    groupsData <- NULL
    nums <- getNums()
    
    if (myX == 'zscore') {
      for (i in myGroups) {
        merge1 <- data.frame("Participant_Id" = event.file$Participant_Id, "placeholder" = event.file[[i]], "groups" = metadata.file$property[metadata.file$source_id == i])
        merge2 <- merge(merge1, prtcpnt.file, by = "Participant_Id")
        merge3 <- merge(merge2, house.file, by = "Participant_Id")
        names(merge3)[names(merge3) == 'placeholder'] <- myX
        draftDF <- completeDF(merge3, myX)
        groupsData <- rbind(groupsData, draftDF)
      }
    } else {
      for (i in myGroups) {
        if (i %in% nums$source_id) {
          merge1 <- data.frame("Participant_Id" = event.file$Participant_Id, "placeholder" = event.file$EUPATH_0000644, 
                               "groups" = paste0(metadata.file$property[metadata.file$source_id == i], ": ", cut(event.file[[i]], 5)))
        } else {
          #right now this assumes that if a string the x axis is ageDays. when we have time as an option to we will need an if statement somewhere here
          merge1 <- data.frame("Participant_Id" = event.file$Participant_Id, "placeholder" = event.file$EUPATH_0000644, "groups" = event.file[[i]])
        }
        merge2 <- merge(merge1, prtcpnt.file, by = "Participant_Id")
        merge3 <- merge(merge2, house.file, by = "Participant_Id")
        names(merge3)[names(merge3) == 'placeholder'] <- myX
        draftDF <- completeDF(merge3, "groups")
        groupsData <- rbind(groupsData, draftDF)
      }
    }
    
    groupsData
  }
    
  singleVarDataFetcher <- function(){
    #not sure if i'll have to call filesFetcher from here or if i get them for free since the files are environment vars
    filesFetcher()
    
    #remove non-unique column names and merge them to one file to return
    drop <- c("source_id", "project_id")
    prtcpnt.file <<- prtcpnt.file[, !(names(prtcpnt.file)) %in% drop]
    house.file <<- house.file[, !(names(house.file)) %in% drop]
    merge1 <- merge(event.file, prtcpnt.file, by = "Participant_Id")
    singleVarData <<- merge(merge1, house.file, by = "Participant_Id")
  }
  
  getFinalDF <- function(plotChoice, myGroups, myX){

    strings <- subset(metadata.file, metadata.file$type == "string", "source_id")
    if (plotChoice == 'groups') {
      data <- groupsDataFetcher(myGroups, myX)
      tempDF <- data[, !(names(data)) %in% "groups"]
      s <- strsplit(as.character(data$groups), split = ",")
      data <- cbind.data.frame(apply(tempDF, 2, function(x) {rep(x, sapply(s,length))}), "groups" = unlist(s), stringsAsFactors=FALSE)
      
      nums <- getNums()
      colsToMakeNumeric <- rbind(subset(nums, nums$source_id %in% names(data)), data.frame("source_id" = myX))
      for (i in colsToMakeNumeric$source_id) {
        data[[i]] = as.numeric(data[[i]])
      }
    } else {
      message(paste("no longer in groups"))
      if (!length(singleVarData)) {
        data <- singleVarDataFetcher()
      } else {
        data <- singleVarData
      }
      #tanzania data is duplicated when x is country because there is a comma in the entry.. 
      if (myX %in% strings$source_id) {
        tempDF <- data[, !(names(data)) %in% myX]
        s <- strsplit(as.character(data[[myX]]), split = ",")
        data <- cbind.data.frame(apply(tempDF, 2, function(x) {rep(x, sapply(s,length))}), "placeholder" = unlist(s), stringsAsFactors=FALSE)
        names(data)[ncol(data)] <- myX
        
        #after above is done need to convert numbers back
        nums <- getNums()
        colsToMakeNumeric <- subset(nums, nums$source_id %in% names(data))
        for (i in colsToMakeNumeric$source_id) {
          data[[i]] = as.numeric(data[[i]])
        }
      }
    }
    
    #not going to let these be binned/ used for faceting for now. more important to have them for xaxis (which requires them not be binned). 
    #perhaps later can reimplement this where additional columns are added that replicate the numeric columns except they are binned.
    #ex: EUPATH_0000025 would have a second column EUPATH_0000025_binned. perhaps could even only choose to bin them if more than 12 levels.
    #nums <- getNums()
    #facetlist <- getFacetList()
    
    #pick just nums which are also facet options
    #numsFacet <- subset(nums, nums$source_id %in% facetlist)
    #numsFacet <- as.vector(numsFacet$source_id)
    #bin numeric values for facet optioins
    #df <- cbind( data[, !(names(data)) %in% numsFacet], lapply(data[, names(data) %in% numsFacet], function(x) {cut(as.numeric(x), 10)}))
    
    data
    #df
  }
  
  getNums <- function(){
    #identify nums 
    nums <- subset(metadata.file, metadata.file$type == "number", "source_id") 
    
    nums
  }

  getGroupsList <- function(myX) {
    
    if (myX == 'zscore') {
      groupsSubset <- metadata.file[grep("z-score", metadata.file$property), ]
    } else if (myX == 'ageDays'){
      colnames <- colnames(event.file[,4:29])
      groupsSubset <- subset(metadata.file, source_id %in% colnames)
    }
    
    groupsChoiceList <- as.vector(groupsSubset$source_id)
    names(groupsChoiceList) <- as.vector(groupsSubset$property)
    
    message(paste("in groups list function"))
   # shinyjs::show("facetUI")
    
    groupsChoiceList
  }
  
    output$choose_groups <- renderUI({
      plotChoice <- input$plotChoice 
      
      if (plotChoice == 'groups'){
        myX <- input$xaxis
        groupsChoiceList <- getGroupsList(myX)
 
        if (myX == 'zscore') {
          selectList <- c('EUPATH_0000734', 'EUPATH_0000689')
        } else {
          selectList <- c('EUPATH_0000734', 'EUPATH_0000704')
        }
        
        checkboxGroupInput("groups",
                           "Groups:", 
                           choiceNames  = names(groupsChoiceList),
                           choiceValues = unname(groupsChoiceList),
                           selected = selectList)
      } else {
      #  shinyjs::show("facetUI")
      }

    })
    
    getXList <- function(plotChoice){
      xlist <- NULL
      
      if (plotChoice != 'groups'){
        if (!length(singleVarData)) {
          data <- singleVarDataFetcher()
        } else {
          data <- singleVarData
        }
        
        #only allow event info for x-axis
        #xcolnames <- colnames(data)[colSums(is.na(data)) > 0]
        xcolnames <- colnames(data)
        #get display names from metadata
        xchoices <- subset(metadata.file, source_id %in% xcolnames)
        myorder <- sort(xchoices$property)
        xchoices <- xchoices[match(myorder, xchoices$property),]
        #convert df to list
        xchoiceList <- as.vector(xchoices$source_id)
        names(xchoiceList) <- as.vector(xchoices$property)
        xlist <- as.list(xchoiceList)
      }
      
      message(paste("in x list function."))
     # shinyjs::show("groupsUI")
      
      xlist
    }
    
    getFacetList <- function(){
      plotChoice <- input$plotChoice
     # if(is.null(input$xaxis)) {
     #   return()
     # }
     # if(plotChoice == 'groups') {
     #   if (is.null(input$groups)) {
     #    message(paste("wah wah being returned from facet"))
     #    return()
     #   }
     # }
      myX <- input$xaxis
      myGroups <- input$groups
      if (plotChoice == 'groups') {
        tempDF <- groupsDataFetcher(myGroups, myX)
      } else {
        if (!length(singleVarData)) {
          data <- singleVarDataFetcher()
        } else {
          data <- singleVarData
        }
        tempDF <- completeDF(data, myX)
      }
      
      #only allow faceting by data that does not contain NA (participant and household info)
      #colnames <- colnames(data)[colSums(is.na(data)) == 0]
      drop = c("Participant_Id", "", "project_id", "source_id")
      #the if statement is commented out for now bc while i think it may be cool to do i dont currently pull the eventfile info into the groups df
      #if (myX == 'zscore' | myX == 'ageDays') {
       # colnames <- c(colnames(prtcpnt.file[, !(names(prtcpnt.file)) %in% drop]), colnames(house.file[, !(names(house.file)) %in% drop]), colnames(event.file[, !(names(event.file)) %in% drop]))
      #} else {}
        colnames <- c(colnames(prtcpnt.file[, !(names(prtcpnt.file)) %in% drop]), colnames(house.file[, !(names(house.file)) %in% drop]))
      #}
      #get display names from metadata
      choices <- subset(metadata.file, source_id %in% colnames)
      #based on xaxis choice remove from choices df anything with levels ==0 or > 12 in tempDF
      boolean <- sapply(tempDF[ , names(tempDF) %in% as.vector(choices$source_id) ], function(x) {(length(levels(x)) < 13 & length(levels(x)) > 1)})
      choices <- choices[match(names(boolean), choices$source_id),]
      #choicesNumeric <- subset(choices, type %in% "number")
      choices <- choices[boolean,]
      myorder <- sort(choices$property)
      choices <- choices[match(myorder, choices$property),]
      #choices <- rbind(choices, choicesNumeric)
      #convert df to list
      choiceList <- c('none', as.vector(choices$source_id))
      names(choiceList) <- c('None', as.vector(choices$property))
      facetlist <- as.list(choiceList)
      
      message(paste("got facet list"))
      #shinyjs::hide(id = "plot_loading", anim = TRUE, animType = "fade")
      #shinyjs::show("plot_area")
      
      facetlist
    } 
    
   # observeEvent(input$plotChoice, {
   #   shinyjs::hide(id="groupsUI")
   #   shinyjs::hide(id="facetUI")
   #   shinyjs::hide(id="plot_area")
   #   shinyjs::show("plot_loading")
   #   message(paste("in observe thing"))
   # })
    
    output$choose_xaxis <- renderUI({
      plotChoice <- input$plotChoice
      
      if (plotChoice == 'groups') {
        getXList(plotChoice)
        selectInput(inputId = "xaxis",
                    label = "X-Axis:",
                    choices = list('Z-score' = 'zscore', 'Age in Days' = 'ageDays'),
                    selected = 'zscore') 
      } else {
        xChoiceList <- getXList(plotChoice)
        selectInput(inputId = "xaxis",
                    label = "X-Axis:",
                    choices = xChoiceList,
                    selected = "EUPATH_0000734")
      }
    })
    
    output$choose_facet <- renderUI({
      facetChoiceList <- getFacetList()

      selectInput(inputId = "facet",
                  label = "Facet:",
                  choices = facetChoiceList,
                  selected = "EUPATH_0000452")
           
    })
    
    output$distribution <- renderPlot({
      
      plotChoice <- input$plotChoice
      #if (plotChoice == 'groups') {
      #  if (is.null(input$groups)) {
     #     return()
     #   }
     # }
     # if(is.null(input$xaxis)) {
     #   return()
     # }
      if(is.null(input$facet)) {
        return()
      }
      myGroups <- input$groups
      myX <- input$xaxis
      myFacet <- input$facet
      
     # shinyjs::hide(id="plot_area") 
    #  shinyjs::show("plot_loading")
      message(paste("just starting"))
        
      df <- getFinalDF(plotChoice, myGroups, myX)
      df <- completeDF(df, myX)
      nums <- getNums()

      xlab <- subset(metadata.file, metadata.file$source_id %in% myX)
      xlab <- as.character(xlab[1,3])
      
      myPlot <- ggplot(data = df, aes_string(x = myX))
      
      if (plotChoice == 'groups') {
        myPlot <- myPlot + geom_histogram(stat = "bin", aes(fill = groups)) #+ scale_y_continuous(formatter = function(x) format(10 ^ x))
        if (length(levels(as.factor(df$groups))) > 12) {
          myPlot <- myPlot + theme(legend.position="none")
        }
        if (myFacet != 'none') {
          myPlot <- myPlot + facet_wrap(reformulate(myFacet), ncol = 1) 
           # scale_fill_brewer(palette = cbPalette)
        }
      } else {
        if (myX %in% nums$source_id) {
          myPlot <- myPlot + geom_histogram(stat = "bin", fill = "#56B4E9")
          myPlot <- myPlot + geom_vline(aes(xintercept = mean(df[[myX]], na.rm = T)), color = "#D55E00", linetype = "dashed", size = 1)
          if (myFacet != 'none') {
            myPlot <- myPlot + facet_wrap(reformulate(myFacet), ncol = 3)   
          }
        } else {
          myPlot <- myPlot + geom_histogram(stat = "count", fill = "#56B4E9")
          myPlot <- myPlot + scale_x_discrete(label=abbreviate) 
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
      
      myPlot <- myPlot + theme_bw()
      myPlot <- myPlot + labs(y = "Count", x = xlab)
      message(paste("c'est fini"))
     
    #  shinyjs::hide("plot_loading", anim = TRUE, animType = "fade")
    #  shinyjs::show("plot_area")

      myPlot
    })

})
