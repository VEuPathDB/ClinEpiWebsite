## server.r

require(shiny)
require(data.table)
require(plotly)
require(viridisLite)

shinyServer(function(input, output, session) {
  
  event.file <- NULL
  event.file.exists <- NULL
  prtcpnt.file <- NULL
  house.file <- NULL
  house.file.exists <- NULL
  metadata.file <- NULL
  singleVarData <- NULL
  longitudinal <- NULL
  prevFacet <- NULL
  current <- NULL
  facetInfo <- NULL
  xaxisInfo <- NULL
  attribute.file <- NULL  

  filesFetcher <- reactive({

    if (is.null(attribute.file)) {

      attribute_temp <- try(fread(
        getWdkDatasetFile('attributes.tab', session, FALSE, dataStorageDir),
        na.strings = c("N/A", "na", "")))
      metadata_temp <- try(fread(
        getWdkDatasetFile('ontologyMetadata.tab', session, FALSE, dataStorageDir),
        na.strings = c("N/A", "na", "")))

    if (grepl("Error", attribute_temp[1])){
      stop("Error: Attributes file missing or unreadable!")
    } else {
      attributes.file <<- attribute_temp
      names(attributes.file) <<-  gsub(" ", "_", gsub("\\[|\\]", "", names(attributes.file)))
      #names(attributes.file)[names(attributes.file) == 'Search_Weight'] <<- 'search_weight'
      attributes.file <<- cbind(attributes.file, custom = "Selected")
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

        #add user defined group
        #metadata.file <<- rbind(metadata.file, list("search_weight", "Strategy Step 1", "string", "none"))
        if (colnames(attributes.file)[1] == 'Participant_Id') {
          metadata.file <<- rbind(metadata.file, list("custom", "User Defined Participants", "string", "none"))
        } else {
          metadata.file <<- rbind(metadata.file, list("custom", "User Defined Observations", "string", "none"))
        }
      }     
 
    }
  })
  
  singleVarDataFetcher <- function(){
    filesFetcher()
    
    model.prop <- fread("../../../../../../config/ClinEpiDB/model.prop", sep = "=", header = FALSE, blank.lines.skip = TRUE)

    #this temporary until i figure how i'm supposed to do it. 
    #will also need to be able to identify one dataset from another, and which to grab.
    mirror.dir <- paste0(model.prop$V2[model.prop$V1 == "WEBSERVICEMIRROR"], "ClinEpiDB")
    contents <- list.files(mirror.dir)
    builds <- contents[grepl("build-", contents)]
    num <- sort(builds)[length(builds)]
    #get datasetName
    custom.props <- try(fread(
        getWdkDatasetFile('customProps.txt', session, FALSE, dataStorageDir)))
    datasetName <- colnames(custom.props)
    mirror.dir <- paste0(mirror.dir, "/", num, "/", datasetName, "/shiny/")

    prtcpnt_temp <- try(fread(paste0(mirror.dir,"shiny_participants.txt"), na.strings = c("N/A", "na", "")))
    house_temp <- try(fread(paste0(mirror.dir, "shiny_households.txt"), na.strings = c("N/A", "na", "")))
    event_temp <- try(fread(paste0(mirror.dir, "shiny_obsevations.txt"), na.strings = c("N/A", "na", "")))

    longitudinal <<- fread("../../lib/longitudinal.tab")
    longitudinal <<- longitudinal[longitudinal$dataset_name == datasetName]
    longitudinal <<- setDT(longitudinal)[, lapply(.SD, function(x) unlist(tstrsplit(x, "|", fixed=TRUE))),
                        by = setdiff(names(longitudinal), "columns")][!is.na(longitudinal$columns)]

    if (grepl("Error", prtcpnt_temp[1])){
      stop("Error: Participant file missing or unreadable!")
    } else {
      prtcpnt.file <<- prtcpnt_temp
      names(prtcpnt.file) <<-  gsub(" ", "_", gsub("\\[|\\]", "", names(prtcpnt.file)))
      names(prtcpnt.file)[names(prtcpnt.file) == 'SOURCE_ID'] <<- 'Participant_Id'
      setkey(prtcpnt.file, Participant_Id)

      if (colnames(attributes.file)[1] == 'Participant_Id') {
        prtcpnt.file <<- merge(prcpnt.file, attributes.file, by = "Participant_Id", all = TRUE)
        naToZero(prtcpnt.file, col = "custom")
        prtcpnt.file$custom[prtcpnt.file$custom == 0] <<- "Not Selected"
      }
    }

    if (grepl("Error", house_temp[1])){
      message("Warning: Household file not found or unreadable.")
    } else {
      house.file <<- house_temp
      names(house.file) <<-  gsub(" ", "_", gsub("\\[|\\]", "", names(house.file)))
      names(house.file)[names(house.file) == 'SOURCE_ID'] <<- 'Participant_Id'
      setkey(house.file, Participant_Id)
    }

    if (grepl("Error", event_temp[1])){
      message("Warning: Events file not found or unreadable.")
    } else {
      event.file <<- event_temp
      names(event.file) <<-  gsub(" ", "_", gsub("\\[|\\]", "", names(event.file)))
      names(event.file)[names(event.file) == 'SOURCE_ID'] <<- 'Participant_Id'
      names(event.file)[names(event.file) == 'NAME'] <<- 'Observation_Id'

      #merge attributes column onto data table
      if (colnames(attributes.file)[1] == 'Observation_Id') {
        event.file <<- merge(event.file, attributes.file, by = "Observation_Id", all = TRUE)
        naToZero(event.file, col = "custom")
        event.file$custom[event.file$custom == 0] <<- "Not Selected"
      }
      #naToZero(singleVarData, col = "search_weight")
    }

    #remove non-unique column names and merge them to one data table to return
    drop <- c("PAN_ID", "PAN_TYPE_ID", "PAN_TYPE", "DESCRIPTION")
    #consider moving drop to event.file TODO
    prtcpnt.file <<- prtcpnt.file[, (drop):=NULL]
    
    if (exists("event.file")) {
      if (!is.null(event.file) & nrow(event.file) > 1) {
        merge1 <- merge(event.file, prtcpnt.file, by = "Participant_Id")
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
        singleVarData <<- merge(merge1, house.file, by = "Participant_Id")
        house.file.exists <<- TRUE
      } else {
        singleVarData <<- merge1
        house.file.exists <<- FALSE
      }
    } else {
      singleVarData <<- merge1
      house.file.exists <<- FALSE
    }

    metadata.file <<- metadata.file[metadata.file$source_id %in% colnames(singleVarData), ]
    
    #for all dates convert strings to date format
    dates <- getDates(metadata.file)$source_id
    for (col in dates) set(singleVarData, j=col, value=as.Date(singleVarData[[col]], format = "%d-%b-%y"))

    singleVarData
  }
  
  output$title <- renderUI({
    singleVarDataFetcher()

    current <<- callModule(timeline, "timeline", singleVarData, longitudinal, metadata.file)
print("checkpoint")
    facetInfo <<- callModule(customGroups, "facet", groupLabel = facetLabel, metadata.file = metadata.file, useData = facetData, singleVarData = singleVarData, event.file = event.file, selected = reactive("EUPATH_0000452"), groupsType = reactive(input$facetType))
    
    xaxisInfo <<- callModule(customGroups, "group", groupLabel = groupLabel, metadata.file = metadata.file, useData = groupData, singleVarData = singleVarData, event.file = event.file, selected = selectedGroup, groupsType = reactive(input$xaxis)) 

    titlePanel("Distributions of Observations for Selected Participants")
  })
  
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
        selectInput(inputId = "xaxis",
                    label = "X-Axis:",
                    choices = c("All possible" = "direct"),
                    selected = "direct",
                    width = '100%')
      }
    })
    
    output$facet_type <- renderUI({
      selectInput(inputId = "facetType",
                  label = "Facet:",
                  choices = c("All possible" = "direct", "Make my own" = "makeGroups", "None" = "none"),
                  selected = "direct",
                  width = '100%')
    })
    
    groupLabel <- reactive({
      if (is.null(input$xaxis)) {
        return()
      } else {
        groupsType <- input$xaxis
      }
      
      label = "bins for"
      
      return(label)
    })
    
    groupData <- reactive({
      if (is.null(input$xaxis)) {
        return()
      } else {
        groupsType <- input$xaxis
      }
      #temporary until i figure out how to plot histograms with dates in plotly
      dates <- getDates(metadata.file)$source_id
      
      #if (groupsType == "direct") {
      #  if (length(dates > 0)) {
       #   ptmp <- prtcpnt.file[, -dates, with=FALSE]
      #  } else {
      #    ptmp <- prtcpnt.file
      #  }
      #  if (house.file.exists) {
       #   if (length(dates > 0)) {
      #      htmp <- house.file[, -dates, with=FALSE]
      #    } else {
      #      htmp <- house.file
      #    }
       #   useData <- list(ptmp, htmp)
       # } else {
      #    useData <- list(ptmp)
      #  }
      #} else {
        if (length(dates > 0)) {
          stmp <- singleVarData[, -dates, with=FALSE]
        } else {
          stmp <- singleVarData
        }
        useData <- list(stmp)
      #}  
      
      return(useData)
    })
    
    selectedGroup <- reactive({
      if (is.null(input$xaxis)) {
        return()
      } else {
        groupsType <- input$xaxis
      }
      
      if (groupsType == "direct") {
        selected <- "EUPATH_0000744"
      } else {
        selected <- "EUPATH_0000704"
      }  
      
      return(selected)
    })
    
    facetLabel <- reactive({
      if (is.null(input$facetType)) {
        return()
      } else {
        facetType <- input$facetType
      }
      
      label = ""
      if (facetType == "direct") {
        label <- "facets for"
      } else if (facetType != "none") {
        label <- "facet where:"
      }
      
      return(label)
    })
    
    facetData <- reactive({
      if (is.null(input$facetType)) {
        return()
      } else {
        facetType <- input$facetType
      }
      
      #if (facetType == "direct") {
      #  if (house.file.exists) {
      #    useData <- list(prtcpnt.file, house.file)
      #  } else {
      #    useData <- list(prtcpnt.file)
      #  }
      #} else {
        useData <- list(singleVarData)
      #}
      
      return(useData)
    })
    
    selectedFacet <- reactive({
      if (is.null(input$facetType)) {
        return()
      } else {
        groupsType <- input$facetType
      }
      
      if (groupsType == "direct") {
        selected <- "EUPATH_0000452"
      } else {
        selected <- "EUPATH_0000704"
      }  
      
      return(selected)
    })
    
    output$distribution <- renderPlotly({
      message("render plot!")
      if (is.null(input$xaxis)) {
        return()
      }
      if (is.null(input$plotChoice)) {
        return()
      }
      if (is.null(input$facetType)) {
        return()
      }
      myX <- input$xaxis
      if (myX == "direct" | myX == "makeGroups") {
        if (is.null(xaxisInfo$group)) {
          return()
        } else {
          myX <- xaxisInfo$group
        }
      }
      plotChoice <- input$plotChoice
      if (input$facetType == "none") {
        myFacet <- "none"
      } else {
        myFacet <- facetInfo$group
      }
      facetType = input$facetType
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
      myPlot <- myPlot + labs(y = "", x = "")
      
      if (plotChoice == 'groups') {
        #myPlot <- myPlot + geom_tooltip(aes(fill = groups, y = .3 * ..count.., tooltip=groups), alpha = .2, real.geom=geom_histogram)  
        myPlot <- myPlot + geom_density(aes(fill = groups, y = .3 * ..count.., text = paste0("Group: ", group , "<br>", "Count: ", .3 * ..count..)), alpha = .2) 
        #myPlot <- myPlot + scale_y_continuous(formatter = function(x) format(10 ^ x))
        if (length(levels(as.factor(df$groups))) > 12) {
          myPlot <- myPlot + theme(legend.position="none")
        }
        if (facetType == 'direct') {
          myPlot <- myPlot + facet_wrap(reformulate(myFacet), ncol = 1)
           # scale_fill_brewer(palette = cbPalette)
        } else if (facetType == 'makeGroups') {
          myPlot <- myPlot + facet_wrap(~ FACET, ncol = 1)
        }
      } else {
        #consider when facetType is makeGroups to use geom_density instead ??
        if (myX %in% nums$source_id | myX %in% dates$source_id) {
          #myPlot <- myPlot + geom_tooltip(aes(tooltip=paste0("count: ", ..count..)), fill = "#56B4E9", real.geom="geom_histogram")
          myPlot <- myPlot + geom_histogram(aes(text = paste0("Count: ", ..count..)), stat = "bin", fill = viridis(1, end = .25, direction = -1))
          myPlot <- myPlot + geom_vline(aes(xintercept = mean(df[[myX]], na.rm = T), text = paste0("mean:", mean(df[[myX]], na.rm = T))), color = viridis(1, begin = .75), linetype = "dashed", size = 1)
          if (facetType == 'direct') {
            myPlot <- myPlot + facet_wrap(reformulate(myFacet), ncol = 1) 
            # scale_fill_brewer(palette = cbPalette)
          } else if (facetType == 'makeGroups') {
            myPlot <- myPlot + facet_wrap(~ FACET, ncol = 1)
          }
        } else {
          #myPlot <- myPlot + geom_tooltip(aes(tooltip=paste0("count: ", ..count..)), stat = "count", fill = "#56B4E9", real.geom="geom_histogram")
          myPlot <- myPlot + geom_histogram(aes(text = paste0("Count: ", ..count..)), stat = "count", fill = viridis(1, end = .25, direction = -1))
          myPlot <- myPlot + theme(axis.text.x = element_text(angle = 90, hjust = 1))
          if(length(levels(as.factor(df[[myX]]))) < 7) {
            if (facetType == 'direct') {
              myPlot <- myPlot + facet_wrap(reformulate(myFacet), ncol = 1) 
              # scale_fill_brewer(palette = cbPalette)
            } else if (facetType == 'makeGroups') {
              myPlot <- myPlot + facet_wrap(~ FACET, ncol = 1)
            }
          } else {
            if (facetType == 'direct') {
              myPlot <- myPlot + facet_wrap(reformulate(myFacet), ncol = 1) 
              # scale_fill_brewer(palette = cbPalette)
            } else if (facetType == 'makeGroups') {
              myPlot <- myPlot + facet_wrap(~ FACET, ncol = 1)
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
    #now just need to edit this and the plotly output to handle the makegroups option in ui
    plotData <- debounce(reactive({
      if (is.null(input$xaxis)) {
        return()
      }
      if (is.null(input$plotChoice)) {
        return()
      }
      if (is.null(input$facetType)) {
        return()
      }
      facetType <- input$facetType
      myX <- input$xaxis
      if (myX == "direct" | myX == "makeGroups") {
        if (is.null(xaxisInfo$group)) {
          return()
        } else {
          myX <- xaxisInfo$group
        }
      }
      plotChoice <- input$plotChoice
      if (input$facetType == "none") {
        myFacet <- "none"
      } else {
        myFacet <- facetInfo$group
      }
      facet_stp1 <- facetInfo$group_stp1
      facet_stp3 <- facetInfo$group_stp3
      facet_stp4 <- facetInfo$group_stp4
      facet_stp2 <- facetInfo$group_stp2
      prevFacet <<- myFacet
      myGroups <- input$groups
      myTimeframe <- current$timeframe
      selected <- current$longitudinal     
 
      strings <- subset(metadata.file, metadata.file$type == "string", "source_id")
      if (plotChoice == 'groups') {
        data <- groupsDataFetcher(myGroups, myX)
        #subset data
        #which cols can be used for this will have to change. too specific right now
        if (!is.null(selected)) {
          if (!is.null(myTimeframe)) {
            data <- subsetDataFetcher(myTimeframe[1], myTimeframe[2], data, selected)
          }
        }
        col = 'groups'
        data <- setDT(data)[, lapply(.SD, function(x) unlist(tstrsplit(x, " | ", fixed=TRUE))), 
                            by = setdiff(names(data), eval(col))][!is.na(eval(col))]
      } else {
        data <- singleVarData
        #subset data
        #which cols can be used for this will have to change. too specific right now
        if (!is.null(selected)) {
          if (!is.null(myTimeframe)) {
            data <- subsetDataFetcher(myTimeframe[1], myTimeframe[2], singleVarData)
          } 
        }
        if (myX %in% strings$source_id) {
          data <- setDT(data)[, lapply(.SD, function(x) unlist(tstrsplit(x, " | ", fixed=TRUE))), 
                              by = setdiff(names(data), eval(myX))][!is.na(eval(myX))]
        }
      }
     
      nums <- getNums(metadata.file)
      dates <- getDates(metadata.file)      

      #for handling facets, this works for direct. need if statement
      if (facetType == "direct") {
        if (myFacet %in% nums$source_id | myFacet %in% dates$source_id) {
          data[[myFacet]] <- cut(data[[myFacet]],4)
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
          #add makeGroups data to df and return
          colnames(outData) <- c("Participant_Id", "FACET")
          #will need a var called label that changes based on what the facet steps are. the below only works for strings.
          #if (any(colnames(event.file) %in% myFacet)) {
          #  naToZero(outData, "FACET")
          #}
          message(paste("levels facet:", levels(as.factor(outData$FACET))))
          outData <- transform(outData, "FACET" = ifelse(as.numeric(FACET) == 0, label[2], label[1]))
          # outData$FACET <- factor(outData$FACET, levels(c("Other", facet_stp2)))
          message(paste("levels facet:", levels(as.factor(outData$FACET))))
          print(head(data))
          print(head(outData))
          data <- merge(data, outData, by = "Participant_Id", all = TRUE)
          print(head(data))
          message(paste("levels facet:", levels(as.factor(data$FACET))))
        }
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
