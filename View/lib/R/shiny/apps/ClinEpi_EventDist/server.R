## server.r

require(shiny)
require(data.table)
require(plotly)
require(viridisLite)

shinyServer(function(input, output, session) {
  
  metadata.file <- NULL
  singleVarData <- NULL
  longitudinal.file <- NULL
  prevFacet <- NULL
  current <- NULL
  facetInfo <- NULL
  facet2Info <- NULL
  xaxisInfo <- NULL
  attributes.file <- NULL  
  propUrl <- NULL
  properties <- NULL
  longitudinal1 <- NULL
  longitudinal2 <- NULL
  isParticipant <- NULL
  model.prop <- NULL
  getMyX <- reactiveValues()
  getMyFacet <- reactiveValues()
  getMyFacet2 <- reactiveValues()
  prtcpntView <- reactiveValues()
  prtcpntView$val <- TRUE

  filesFetcher <- reactive({
    if (is.null(propUrl)) {
      propUrl <<- getPropertiesUrl(session)
      properties <- try(fread(propUrl))
      if (grepl("Error", properties)) {
        properties <<- NULL
      }
    }
    message(paste("propUrl:", propUrl))
    
    if (is.null(attributes.file)) {
      
      attribute_temp <- try(fread(
        getWdkDatasetFile('attributes.tab', session, FALSE, dataStorageDir),
        na.strings = c("N/A", "na", "")))
      metadata_temp <- try(fread(
          getWdkDatasetFile('ontologyMetadata.tab', session, FALSE, dataStorageDir),
          ))
      model.prop_temp <- try(fread(
        getWdkDatasetFile('model.prop', session, FALSE, dataStorageDir),
        sep="=", header=FALSE, fill=TRUE))
 
      if (grepl("Error", model.prop_temp[1])){
        stop("Error: model.prop file missing or unreadable!")
      } else {
        #not sure if we'll need to do anything else here yet
        model.prop <<- model.prop_temp
      }
      
      if (grepl("Error", attribute_temp[1])){
        stop("Error: Attributes file missing or unreadable!")
      } else {
        attributes.file <<- attribute_temp
        names(attributes.file) <<-  gsub(" ", "_", gsub("\\[|\\]", "", names(attributes.file)))
        project.id <<- attributes.file$project_id[1]
        #names(attributes.file)[names(attributes.file) == 'Search_Weight'] <<- 'search_weight'
        attributes.file <<- cbind(attributes.file, custom = "Selected")
      }
      
      if (grepl("Error", metadata_temp[1])){
        stop("Error: Metadata file missing or unreadable!")
      } else {
        metadata.file <<- metadata_temp
        names(metadata.file) <<- gsub(" ", "_", tolower(gsub("\\[|\\]", "", names(metadata.file))))
        setkey(metadata.file, source_id)
      
        if ('Participant_Id' %in% colnames(attributes.file)) {
          isParticipant <<- TRUE
          metadata.file <<- rbind(metadata.file, list("custom", "Selected Participants", "string", "Participants", "Participant"))
          metadata.file <<- rbind(metadata.file, list("dontcare", "Participants", "string", "Search Results", "Participant"))
          metadata.file <<- rbind(metadata.file, list("dontcare2", "Dynamic Attributes", "string", "Search Results", "Participant"))
          metadata.file <<- rbind(metadata.file, list("dontcare3", "Search Results", "string", "null", "Participant"))
          metadata.file <<- rbind(metadata.file, list("Avg_Female_Anopheles", "Avg Female Anopheles", "number", "Dynamic Attributes", "Participant"))
          metadata.file <<- rbind(metadata.file, list("Matching_Observations_/_Year", "Matching Observations / Year", "number", "Dynamic Attributes", "Participant"))
          metadata.file <<- rbind(metadata.file, list("Years_of_Observation", "Years of Observations", "number", "Dynamic Attributes", "Participant"))
          } else {
          isParticipant <<- FALSE
          metadata.file <<- rbind(metadata.file, list("custom", "Selected Observations", "string", "Observations", "Observation"))
          metadata.file <<- rbind(metadata.file, list("dontcare", "Observations", "string", "Search Results", "Observation"))
          metadata.file <<- rbind(metadata.file, list("dontcare2", "Search Results", "string", "null", "Observation"))
        }  
      }
    }
  })
  
  singleVarDataFetcher <- function(){
    filesFetcher()
    
    #build up mirror.dir path
    mirror.dir <- paste0(model.prop$V2[model.prop$V1 == "WEBSERVICEMIRROR"], "ClinEpiDB") 
    num <- paste0("build-", model.prop$V2[model.prop$V1 == 'buildNumber'])
    custom.props <- try(fread(
        getWdkDatasetFile('customProps.txt', session, FALSE, dataStorageDir)))
    datasetName <- colnames(custom.props)
    mirror.dir <- paste0(mirror.dir, "/", num, "/", datasetName, "/shiny/")
    #message(mirror.dir)
    singleVarData <<- fread(paste0(mirror.dir, "shiny_masterDataTable.txt"))
    
    if ('Participant_Id' %in% colnames(attributes.file)) {
      singleVarData <<- merge(singleVarData, attributes.file, by = "Participant_Id", all = TRUE)
      naToZero(singleVarData, col = "custom")
      singleVarData$custom[singleVarData$custom == 0] <<- "Not Selected"
    }
    if ('Observation_Id' %in% colnames(attributes.file)) {
      singleVarData <<- merge(singleVarData, attributes.file, by = "Observation_Id", all = TRUE)
      naToZero(singleVarData, col = "custom")
      singleVarData$custom[singleVarData$custom == 0] <<- "Not Selected"
    }

    longitudinal.file <<- fread("../../lib/longitudinal.tab")
    longitudinal.file <<- longitudinal.file[longitudinal.file$dataset_name == datasetName]
    longitudinal.file <<- setDT(longitudinal.file)[, lapply(.SD, function(x) unlist(tstrsplit(x, "|", fixed=TRUE))),
                        by = setdiff(names(longitudinal.file), "columns")][!is.na(longitudinal.file$columns)]

    #remove non-unique column names and merge them to one data table to return
    drop <- c("PAN_ID", "PAN_TYPE_ID", "PAN_TYPE", "DESCRIPTION")
    #consider moving drop to event.file TODO
    singleVarData <<- singleVarData[, !drop, with = FALSE]
 
    #for all dates convert strings to date format
    dates <- getDates(metadata.file)$source_id
    dates <- dates[dates %in% colnames(singleVarData)]
    for (col in dates) set(singleVarData, j=col, value=as.Date(singleVarData[[col]], format = "%d-%b-%y"))

    nums <- getNums(metadata.file)$source_id
    if (!nrow(longitudinal.file) == 0) {
      if (all(longitudinal.file$columns %in% dates) | all(longitudinal.file$columns %in% nums)) {
        numTimelines <- 1
      } else {
        numTimelines <- 2
      }
      if (numTimelines == 1) {
        longitudinal1 <<- longitudinal.file$columns
        longitudinal2 <<- NULL
      } else {
        longitudinal1 <<- subset(longitudinal.file, longitudinal.file$columns %in% dates)$columns
        longitudinal2 <<- subset(longitudinal.file, longitudinal.file$columns %in% nums)$columns
      }
    }

    singleVarData
  }
  
  output$title <- renderText({
    withProgress(message = 'Loading...', value = 0, style = "old", {
      if (is.null(singleVarData)) {
        singleVarDataFetcher()
      }
      incProgress(.45)
      current <<- callModule(timeline, "timeline", singleVarData, longitudinal.file, metadata.file)
      incProgress(.15)
      xaxisInfo <<- callModule(customGroups, "group", groupLabel = groupLabel, metadata.file = metadata.file, include = groupData, singleVarData = singleVarData, selected = selectedGroup, groupsType = reactive(input$xaxis), groupsTypeID = "input$xaxis", moduleName = "xaxisInfo", prtcpntView = reactive(prtcpntView$val))
      if (is.null(properties)) {
        getMyX$val <- selectedGroup()
      } else {
        getMyX$val <- properties$selected[properties$input == "xaxisInfo$group"]
      }
      incProgress(.25)
      facetInfo <<- callModule(customGroups, "facet", groupLabel = facetLabel, metadata.file = metadata.file, include = facetData, singleVarData = singleVarData, selected = selectedFacet, groupsType = reactive(input$facetType), groupsTypeID = "input$facetType", moduleName = "facetInfo", prtcpntView = reactive(prtcpntView$val))
      if (is.null(properties)) {
        getMyFacet$val <- selectedFacet()
      } else {
        getMyFacet$val <- properties$selected[properties$input == "facetInfo$group"]
      }
      facet2Info <<- callModule(customGroups, "facet2", groupLabel = facet2Label, metadata.file = metadata.file, include = facet2Data, singleVarData = singleVarData, selected = selectedFacet2, groupsType = reactive(input$facet2Type), groupsTypeID = "input$facet2Type", moduleName = "facet2Info", prtcpntView = reactive(prtcpntView$val))
      if (is.null(properties)) {
        getMyFacet2$val <- selectedFacet2()
      } else {
        getMyFacet2$val <- properties$selected[properties$input == "facet2Info$group"]
      }
      incProgress(.15)
    })
    c("Data Distributions")
  })
 
  output$prtcpntViewSwitch <- renderUI({
    if (isParticipant != TRUE) {
      tagList(
        box(width = 6, status = "primary", title = "Unit of Analysis",
            radioButtons(inputId = "prtcpntViewSwitch",
                         label = NULL,
                         choiceNames = c("Participant View", "Observation View"),
                         choiceValues = c(TRUE, FALSE),
                         selected = "FALSE",
                         inline = TRUE)
        )
      )
    }
  })

  observeEvent(input$prtcpntViewSwitch, {
    if (input$prtcpntViewSwitch == "TRUE" | input$prtcpntViewSwitch == TRUE) {
      prtcpntView$val <- TRUE
    } else {
      prtcpntView$val <- FALSE
    }
  })
    
   output$choose_xaxis <- renderUI({
      
        mySelected <- properties$selected[properties$input == "input$xaxis"]
        
        if (is.null(properties)) {
          selectInput(inputId = "xaxis",
                      label = NULL,
                      choices = c("All possible" = "direct"),
                      selected = "direct",
                      width = '100%')
        } else {
          selectInput(inputId = "xaxis",
                      label = NULL,
                      choices = c("All possible" = "direct"),
                      selected = mySelected,
                      width = '100%')
        }
    })
    
    output$facet_type <- renderUI({
      mySelected <- properties$selected[properties$input == "input$facetType"]

      if (is.null(properties)) {
        if (isParticipant) {
          selectInput(inputId = "facetType",
                      label = NULL,
                      choices = c("All possible" = "direct", "Make my own" = "makeGroups", "None" = "none"),
                      selected = "direct",
                      width = '100%')
        } else {
          selectInput(inputId = "facetType",
                      label = NULL,
                      choices = c("All possible" = "direct", "Make my own" = "makeGroups", "None" = "none"),
                      selected = "makeGroups",
                      width = '100%')
        }
      } else {
        selectInput(inputId = "facetType",
                    label = NULL,
                    choices = c("All possible" = "direct", "Make my own" = "makeGroups", "None" = "none"),
                    selected = mySelected,
                    width = '100%')
      }
    })
    
    output$facet2_type <- renderUI({
      mySelected <- properties$selected[properties$input == "input$facet2Type"]
      
      if (is.null(properties)) {
        selectInput(inputId = "facet2Type",
                    label = NULL,
                    choices = c("All possible" = "direct", "Make my own" = "makeGroups", "None" = "none"),
                    selected = "none",
                    width = '100%')
      } else {
        selectInput(inputId = "facet2Type",
                    label = NULL,
                    choices = c("All possible" = "direct", "Make my own" = "makeGroups", "None" = "none"),
                    selected = mySelected,
                    width = '100%')
      }
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
      
      if (length(dates > 0)) {
        stmp <- singleVarData[, -dates, with=FALSE]
      } else {
        stmp <- singleVarData
      }
      useData <- list(stmp)
            
      return(c("all"))
    })
    
    selectedGroup <- reactive({
      if (is.null(input$xaxis)) {
        return()
      } else {
        groupsType <- input$xaxis
      }
      
      if ("EUPATH_0000338" %in% metadata.file$source_id) {
        selected <- "EUPATH_0000338"
      } else {
        include <- groupData()
        if (include != "all") {
          temp <- metadata.file[metadata.file$category %in% include]
        } else {
          temp <- metadata.file
        }
        myCols <- colnames(singleVarData)
        temp <- temp[temp$source_id %in% myCols]
        parents <- temp$parent
        leaves <- temp[!temp$property %in% parents]
        leaves <- leaves[order(leaves$property),]
        leaves <- leaves$source_id
        selected <- leaves[1]
      }  
      
      return(selected)
    })
   
    selectedFacet <- reactive({
      if (is.null(input$facetType)) {
        return()
      } else {
        facetType <- input$facetType
      }

      if (facetType == "direct") {
        #selected <- "custom"
        selected <- "custom"
      } else if (facetType == "makeGroups") {
        if (isParticipant) {
          if ("EUPATH_0000054" %in% colnames(singleVarData)) {
            selected <- "EUPATH_0000054"
          } else {
            include <- facetData()
            if (include != "all") {
              temp <- metadata.file[metadata.file$category %in% include]
            } else {
              temp <- metadata.file
            }
            myCols <- colnames(singleVarData)
            temp <- temp[temp$source_id %in% myCols]
            parents <- temp$parent
            leaves <- temp[!temp$property %in% parents]
            leaves <- leaves[order(leaves$property),]
            leaves <- leaves$source_id
            selected <- leaves[1]
          }
        } else {
          selected <- "custom"
        }
      } else {
        selected <- ""
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
      
      if (facetType == "direct") {
        dates <- getDates(metadata.file)$source_id
        #ptmp <- prtcpnt.file[, !dates, with = FALSE]
        if ("Household" %in% metadata.file$category) {
          #htmp <- house.file[, !dates, with = FALSE]
          include <- c("Participant", "Household")
        } else {
          include <- c("Participant")
        }
      } else {
        include <- c("all")
      }     
 
      return(include)
    })
    
    selectedFacet2 <- reactive({
      if (is.null(input$facet2Type)) {
        return()
      } else {
        facet2Type <- input$facet2Type
      }
      
      if (facet2Type == "direct") {
        selected <- "custom"
      } else if (facet2Type == "makeGroups") {
        if (isParticipant) {
          if ("EUPATH_0000054" %in% colnames(singleVarData)) {
            selected <- "EUPATH_0000054"
          } else {
            include <- facet2Data()
            if (include != "all") {
              temp <- metadata.file[metadata.file$category %in% include]
            } else {
              temp <- metadata.file
            }
            myCols <- colnames(singleVarData)
            temp <- temp[temp$source_id %in% myCols]
            parents <- temp$parent
            leaves <- temp[!temp$property %in% parents]
            leaves <- leaves[order(leaves$property),]
            leaves <- leaves$source_id
            selected <- leaves[1]
          }
        } else {
          selected <- "custom"
        }
      } else {
        selected <- ""
      }
      
      return(selected)
    })
    
    facet2Label <- reactive({
      if (is.null(input$facet2Type)) {
        return()
      } else {
        facet2Type <- input$facet2Type
      }
      
      label = ""
      if (facet2Type == "direct") {
        label <- "facets for"
      } else if (facet2Type != "none") {
        label <- "facet where:"
      }
      
      return(label)
    })
    
    facet2Data <- reactive({
      if (is.null(input$facet2Type)) {
        return()
      } else {
        facet2Type <- input$facet2Type
      }
      
      if (facet2Type == "direct") {
        dates <- getDates(metadata.file)$source_id
        #ptmp <- prtcpnt.file[, !dates, with = FALSE]
        if ("Household" %in% metadata.file$category) {
          #htmp <- house.file[, !dates, with = FALSE]
          include <- c("Participant", "Household")
        } else {
          include <- c("Participant")
        }
      } else {
        include <- c("all")
      }     
      
      return(include)
    })
    
    observeEvent(xaxisInfo$group, {
      if (length(get_selected(xaxisInfo$group, format="names")) != 0) {

        mySelected <- get_selected(xaxisInfo$group, format="names")[[1]]
        myProp <- mySelected[1]
        myParent <- unlist(attributes(mySelected))[length(unlist(attributes(mySelected)))]
        nextX <- metadata.file$source_id[metadata.file$property == myProp & metadata.file$parent == myParent]

        nextX <- unique(nextX)

        if (length(nextX) != 1) {
          message("Warning: non-unique source_ids returned ", nextX)
        }

        if (is.null(getMyX$val)) {
          getMyX$val <- nextX
        } else if (getMyX$val != nextX) {
          getMyX$val <- nextX
        }
      }
    })

    observeEvent(facetInfo$group, {
      if (length(get_selected(facetInfo$group, format="names")) != 0) {

        mySelected <- get_selected(facetInfo$group, format="names")[[1]]
        myProp <- mySelected[1]
        myParent <- unlist(attributes(mySelected))[length(unlist(attributes(mySelected)))]
        nextFacet <- metadata.file$source_id[metadata.file$property == myProp & metadata.file$parent == myParent]

        nextFacet <- unique(nextFacet)

        if (length(nextFacet) != 1) {
          message("Warning: non-unique source_ids returned ", nextFacet)
        }

        if (is.null(getMyFacet$val)) {
          getMyFacet$val <- nextFacet
          print("resetting myFacet")
        } else if (getMyFacet$val != nextFacet) {
          getMyFacet$val <- nextFacet
          print("resetting myFacet")
        }
      }
    })
    
    observeEvent(facet2Info$group, {
      if (length(get_selected(facet2Info$group, format="names")) != 0) {
       
        mySelected <- get_selected(facet2Info$group, format="names")[[1]]
        myProp <- mySelected[1]
        myParent <- unlist(attributes(mySelected))[length(unlist(attributes(mySelected)))]
        nextFacet <- metadata.file$source_id[metadata.file$property == myProp & metadata.file$parent == myParent]
 
        nextFacet <- unique(nextFacet)

        if (length(nextFacet) != 1) {
          message("Warning: non-unique source_ids returned ", nextFacet)
        } 

        if (is.null(getMyFacet2$val)) {
          getMyFacet2$val <- nextFacet
          print("resetting myFacet2")
        } else if (getMyFacet2$val != nextFacet) {
          getMyFacet2$val <- nextFacet
          print("resetting myFacet2")
        }
      }
    })

    #tried to wrap these three into one observer and it broke.. look again later
    observeEvent(getMyX$val, {
      #execute javascript to virtually click outside the dropdown
      print("clicking!!!!!!!!!!!")
      js$virtualBodyClick();
    })

    observeEvent(getMyFacet$val, {
      #execute javascript to virtually click outside the dropdown
      print("clicking!!!!!!!!!!!")
      js$virtualBodyClick();
    })
    
    observeEvent(getMyFacet2$val, {
      #execute javascript to virtually click outside the dropdown
      print("clicking!!!!!!!!!!!")
      js$virtualBodyClick();
    })

    aggKey <- reactive({
      myPrtcpntView <- prtcpntView$val
      
      if (myPrtcpntView == TRUE) {
        aggKey <- c("Participant_Id")
      } else {
        aggKey <- c("Participant_Id", longitudinal1)
      }
      
      return(aggKey)
    })
    
    output$individualPlot_stp1 <- renderUI({
      if (is.null(input$facetType)) {
        return()
      }
      if (input$facetType == "none") {
        myFacet <- "none"
      } else {
        myFacet <- getMyFacet$val
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
      facetVals <- unique(df[,myFacet, with=FALSE])
      facetVals <- unlist(facetVals)
      names(facetVals) <- facetVals
      
      if (is.null(properties)) {
        mySelected <- c("abc")
      } else {
        mySelected <- properties$selected[properties$input == 'input$individualPlot_stp1']
      }      
 
      selectInput(inputId = "individualPlot_stp1",
                  label = "Facet Plot (1) value:",
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
        myFacet2 <- getMyFacet2$val
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
      facet2Vals <- unique(df[,myFacet2, with=FALSE])
      facet2Vals <- unlist(facet2Vals)
      names(facet2Vals) <- facet2Vals
      
      if (is.null(properties)) {
        mySelected <- c("abc")
      } else {
        mySelected <- properties$selected[properties$input == 'input$individualPlot_stp2']
      }      

      selectInput(inputId = "individualPlot_stp2",
                  label = "Facet Plot (2) value:",
                  choices = facet2Vals,
                  selected = mySelected)
    })
    
    output$individual_distribution <- renderPlotly({
      if (is.null(input$xaxis)) {
        return()
      }
      if (is.null(input$facetType)) {
        return()
      }
      if (is.null(input$facet2Type)) {
        return()
      }
      if (is.null(input$individualPlot_stp1) & is.null(input$individualPlot_stp2)) {
        return()
      }
      myX <- input$xaxis
      if (myX == "direct" | myX == "makeGroups") {
        if (is.null(getMyX$val)) {
          return()
        } else {
          myX <- getMyX$val
        }
      }
      if (input$facetType == "none") {
        myFacet <- "none"
      } else {
        myFacet <- getMyFacet$val
        if (is.null(input$individualPlot_stp1)) {
          return()
        }
      }
      facetType <- input$facetType
      if (input$facet2Type == "none") {
        myFacet2 <- "none"
      } else {
        myFacet2 <- getMyFacet2$val
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
      
      if (myX == 'ageDays') {
        xlab <- "Age in Days"
      } else if (myX == 'zscore') {
        xlab <- "Z-score"
      } else {
        xlab <- subset(metadata.file, metadata.file$source_id %in% myX)
        xlab <- as.character(xlab[1,2])
      }
     
      df <- plotData()
      df <- completeDT(df, myX)

      if (!is.null(iPlot_stp1)) {
        keep <- c(df[, myFacet, with=FALSE] == iPlot_stp1)
        df <- df[keep,]
      }
      if (!is.null(iPlot_stp2)) {
        keep2 <- c(df[, myFacet2, with=FALSE] == iPlot_stp2)
        df <- df[keep2,]
      }
        
      myPlot <- ggplot(data = df, aes_string(x = myX))
      myPlot <- myPlot + theme_bw()
      myPlot <- myPlot + labs(y = "", x = "")
      
      if ((myX %in% nums$source_id | myX %in% dates$source_id) & myX != myFacet) {
        myPlot <- myPlot + geom_histogram(aes(text = paste0("Count: ", ..count..)), stat = "bin", fill = viridis(1, end = .25, direction = -1))
      } else {
        myPlot <- myPlot + geom_histogram(aes(text = paste0("Count: ", ..count..)), stat = "count", fill = viridis(1, end = .25, direction = -1))
        myPlot <- myPlot + theme(axis.text.x = element_text(angle = 90, hjust = 1))
      }
      
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
                         "Count",
                         rep(" ", 10),
                         "\n"),
                       collapse = ""),
        size = 14
      ) 
      
      myPlotly <- ggplotly(myPlot, tooltip = c("text"))
      myPlotly <- plotly:::config(myPlotly, displaylogo = FALSE, collaborate = FALSE)
      myPlotly <- layout(myPlotly, margin = list(l = 70, r = 0, b = 200, t = 40), 
                         xaxis = x_list, 
                         yaxis = y_list,
                         legend = list(x = 100, y = .5))
      
      myPlotly
    })
    
    output$distribution <- renderPlotly({
      message("render plot!")
      if (is.null(input$xaxis)) {
        return()
      }
      if (is.null(input$facetType)) {
        return()
      }
      if (is.null(input$facet2Type)) {
        return()
      }
      myX <- input$xaxis
      if (myX == "direct" | myX == "makeGroups") {
        if (is.null(getMyX$val)) {
          return()
        } else {
          myX <- getMyX$val
        }
      }
      if (input$facetType == "none") {
        myFacet <- "none"
      } else {
        myFacet <- getMyFacet$val
      }
      facetType <- input$facetType
      if (input$facet2Type == "none") {
        myFacet2 <- "none"
      } else {
        myFacet2 <- getMyFacet2$val
      }
      facet2Type <- input$facet2Type
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
      
        #consider when facetType is makeGroups to use geom_density instead ??
        if ((myX %in% nums$source_id | myX %in% dates$source_id) & myX != myFacet) {
          myPlot <- myPlot + geom_histogram(aes(text = paste0("Count: ", ..count..)), stat = "bin", fill = viridis(1, end = .25, direction = -1))
          myPlot <- myPlot + geom_vline(aes(xintercept = mean(df[[myX]], na.rm = T), text = paste0("mean:", mean(df[[myX]], na.rm = T))), color = viridis(1, begin = .75), linetype = "dashed", size = 1)
        
          if (facetType == "none" & facet2Type != "none") {
            myFacet <- myFacet2
            facetType <- facet2Type
            facet2Type <- "none"
          }
          print(facetType)
          print(facet2Type)
          if (facet2Type == "none") {
            if (facetType == 'direct') {
              myPlot <- myPlot + facet_wrap(reformulate(myFacet), ncol = 1) 
              # scale_fill_brewer(palette = cbPalette)
            } else if (facetType == 'makeGroups') {
              myPlot <- myPlot + facet_wrap(~ FACET, ncol = 1)
            }
          } else {
            if (facetType == "makeGroups") {
              myFacet <- "FACET"
            }
            if (facet2Type == "makeGroups") {
              myFacet2 <- "FACET2"
            }
            print(reformulate(myFacet,myFacet2))
            myPlot <- myPlot + facet_grid(reformulate(myFacet, myFacet2))
          }
          
        } else {
          myPlot <- myPlot + geom_histogram(aes(text = paste0("Count: ", ..count..)), stat = "count", fill = viridis(1, end = .25, direction = -1))
          myPlot <- myPlot + theme(axis.text.x = element_text(angle = 90, hjust = 1))
          if (facetType == "none" & facet2Type != "none") {
            myFacet <- myFacet2
            facetType <- facet2Type
            facet2Type <- "none"
          }
          print(facetType)
          print(facet2Type)
          if (facet2Type == "none") {
            if (facetType == 'direct') {
              myPlot <- myPlot + facet_wrap(reformulate(myFacet), ncol = 1) 
              # scale_fill_brewer(palette = cbPalette)
            } else if (facetType == 'makeGroups') {
              myPlot <- myPlot + facet_wrap(~ FACET, ncol = 1)
            }
          } else {
            if (facetType == "makeGroups") {
              myFacet <- "FACET"
            }
            if (facet2Type == "makeGroups") {
              myFacet2 <- "FACET2"
            }
            print(reformulate(myFacet,myFacet2))
            myPlot <- myPlot + facet_grid(reformulate(myFacet, myFacet2))
          }
          myPlot <- myPlot + theme(axis.text.x = element_text(angle = 45, hjust = 1))
        }
            
      message(paste("c'est fini"))
     
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
                       "Count",
                       rep(" ", 10),
                       "\n"),
                       collapse = ""),
        size = 14
      ) 
      
      myPlotly <- ggplotly(myPlot, tooltip = c("text"))
      myPlotly <- plotly:::config(myPlotly, displaylogo = FALSE, collaborate = FALSE)
      myPlotly <- layout(myPlotly, margin = list(l = 70, r = 0, b = 200, t = 40), 
                                   xaxis = x_list, 
                                   yaxis = y_list,
                                   legend = list(x = 100, y = .5))
      
      myPlotly
    })
    
    observeEvent(plotData(), {
      plotData <- plotData()
      if (is.null(plotData)) {
        return()
      }
      if (input$facetType == "none") {
        myFacet <- "none"
      } else {
        myFacet <- getMyFacet$val
      }
      if (input$facet2Type == "none") {
        myFacet2 <- "none"
      } else {
        myFacet2 <- getMyFacet2$val
      }
      myX <- getMyX$val
      if ("FACET" %in% colnames(data)) {
        myFacet <- "FACET"
      }
      if ("FACET2" %in% colnames(data)) {
        myFacet2 <- "FACET2"
      }
      myPrtcpntView <- prtcpntView$val

      if (myFacet2 != "none") {
        if (myFacet != "none") {
          dt_len <- uniqueN(plotData[, myFacet2, with=FALSE])
          dt_list <- unique(plotData[, myFacet2, with=FALSE]) 
        } else {
          myFacet <- myFacet2
          myFacet2 <- "none"
          dt_len <- 1
          dt_list <- NULL
        }
      } else {
        dt_len <- 1
        dt_list <- NULL
      }
      
      nums <- getNums(metadata.file)
      dates <- getDates(metadata.file)
      
      createUI <- function(id, data, facets) {
        
        if (myPrtcpntView == TRUE) {
          colLabel <- "# Participants"
          colVal <- length(unique(data$Participant_Id))
          countFun <- function(x) {length(unique(x))}
        } else {
          colLabel <- "# Observations"
          colVal <- nrow(data)
          aggStr <- myFacet
          countFun <- function(x) {length(x)}
        }
        
        if (myFacet == "none") {
          tableData <- data.table("Facet" = "All", colLabel = colVal)
          if (myX %in% nums) {
            tableData <- cbind(tableData, "Mean" = round(mean(data[[myX]], na.rm = TRUE),4))
            tableData <- cbind(tableData, "Median" = median(data[[myX]], na.rm = TRUE))
            tableData <- cbind(tableData, "Range" = paste(min(data[[myX]], na.rm = TRUE), "-", max(data[[myX]], na.rm = TRUE)))
            tableData <- cbind(tableData, "SD" = round(sd(data[[myX]], na.rm = TRUE),4))
            tableData <- cbind(tableData, "IQR" = round(IQR(data[[myX]], na.rm = TRUE),4))
          }
        } else {
          aggStr <- paste0("Participant_Id ~ ", myFacet)
          aggStr2 <- paste0(myX, " ~ ", myFacet)
          #will need to change first arg based on all possible or makeGroups
          tableData <- aggregate(as.formula(aggStr), data, FUN = countFun)
          if (myX %in% nums) {
            mean <- aggregate(as.formula(aggStr2), data, FUN = function(x){round(mean(x),4)})
            tableData <- merge(tableData, mean, by = myFacet)
            median <- aggregate(as.formula(aggStr2), data, median)
            tableData <- merge(tableData, median, by = myFacet)
            range <- aggregate(as.formula(aggStr2), data, FUN = function(x){paste(min(data[[myX]], na.rm = TRUE), "-", max(data[[myX]], na.rm = TRUE))})
            tableData <- merge(tableData, range, by = myFacet)
            mySD <- aggregate(as.formula(aggStr2), data, FUN = function(x){round(sd(x),4)})
            tableData <- merge(tableData, mySD, by = myFacet)
            myIQR <- aggregate(as.formula(aggStr2), data, FUN = function(x){round(IQR(x),4)})
            tableData <- merge(tableData, myIQR, by = myFacet)
            colnames(tableData) <- c("Facets", colLabel, "Mean", "Median", "Range", "SD", "IQR")
          } else {
            colnames(tableData) <- c("Facets", colLabel)
          }
        }
        
        if (length(colnames(tableData)) == 7) {
          myTargets <- c(1,2,3,4,5,6)
        } else {
          myTargets <- c(1)
        }
       
        if (length(facets) > 0) {
          myCaption <- paste("Facet(s):",paste(facets, collapse = " and "))
        } else {
          myCaption <- ""
        }
         
        output[[id]] <- DT::renderDataTable(datatable(tableData,
                                                      caption = myCaption,
                                                      width = '100%',
                                                      rownames = FALSE,
                                                      options = list(
                                                        columnDefs = list(list(className = 'dt-right', targets = myTargets))
                                                      )
        ))
      }
     
      output$dt <- renderUI({
        lapply(1:dt_len, function(i) {
          id <- paste0("dt", i)
          if (!is.null(dt_list)) {
            keep <- c(plotData[, myFacet2, with=FALSE] == c(dt_list[i]))
            facets <- c(dt_list[i])
            data <- plotData[keep,]
          } else {
            data <- plotData
          }
          createUI(id, data, facets)
        })
      })
    })
    
    
    #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    #datagrabbed through reactive expression for better control of reactive context
    #now just need to edit this and the plotly output to handle the makegroups option in ui
    plotData <- debounce(reactive({
      if (is.null(input$xaxis)) {
        return()
      }
      if (is.null(input$facetType)) {
        return()
      }
      facetType <- input$facetType
      facet2Type <- input$facet2Type
      xType <- input$xaxis
      myX <- input$xaxis
      if (myX == "direct" | myX == "makeGroups") {
        if (is.null(getMyX$val)) {
          return()
        } else {
          myX <- getMyX$val
        }
      }
      if (input$facetType == "none") {
        myFacet <- "none"
      } else {
        myFacet <- getMyFacet$val
      }
      facet_stp1 <- facetInfo$group_stp1
      facet_stp3 <- facetInfo$group_stp3
      facet_stp4 <- facetInfo$group_stp4
      facet_stp2 <- facetInfo$group_stp2
      prevFacet <<- myFacet
      if (input$facet2Type == "none") {
        myFacet2 <- "none"
      } else {
        myFacet2 <- getMyFacet2$val
      }
      facet2_stp1 <- facet2Info$group_stp1
      facet2_stp3 <- facet2Info$group_stp3
      facet2_stp4 <- facet2Info$group_stp4
      facet2_stp2 <- facet2Info$group_stp2
      prevFacet2 <<- myFacet2
      myGroups <- input$groups
      myTimeframe1 <- current$range1
      myTimeframe2 <- current$range2
 
      if (is.null(facetType)) {
        return()
      } else {
        if (facetType == "makeGroups") {
          if (is.null(facet_stp1)) {
            return()
          } else {
            if (facet_stp1 == 'any' | facet_stp1 == 'all') {
              if (is.null(facet_stp2)) {
                return()
              } else {
                if (facet_stp2 %in% c("lessThan", "greaterThan", "equals")) {
                  if (is.null(facet_stp3)) {
                    return()
                  }
                }
              }
            }
          } 
        } else if (facetType == "direct") {
          if (is.null(myFacet)) {
            return()
          }
        } 
      }
      
      if (is.null(facet2Type)) {
        return()
      } else {
        if (facet2Type == "makeGroups") {
          if (is.null(facet2_stp1)) {
            return()
          } else {
            if (facet2_stp1 == 'any' | facet2_stp1 == 'all') {
              if (is.null(facet2_stp2)) {
                return()
              } else {
                if (facet2_stp2 %in% c("lessThan", "greaterThan", "equals")) {
                  if (is.null(facet2_stp3)) {
                    return()
                  }
                }
              }
            }
          } 
        } else if (facet2Type == "direct") {
          if (is.null(myFacet2)) {
            return()
          }
        } 
      }

      #first thing is to save properties 
      longitudinalText <- longitudinalText(myTimeframe1, myTimeframe2)
      facetText <- groupText("facetInfo", myFacet, facet_stp1, facet_stp2, facet_stp3, facet_stp4)
      facet2Text <- groupText("facet2Info", myFacet2, facet2_stp1, facet2_stp2, facet2_stp3, facet2_stp4)

      text <- paste0("input\tselected\n",
                     longitudinalText,
                     facetText,
                     facet2Text,
                     "xaxisInfo$group\t", myX, "\n",
                     "input$facetType\t", facetType, "\n",
                     "input$facet2Type\t", facet2Type, "\n",
                     "input$xaxis\t", xType, "\n",
                     "input$individualPlot_stp1\t", input$individualPlot_stp1, "\n",
                     "input$individualPlot_stp2\t", input$individualPlot_stp2 
                    )

message("propUrl: ", propUrl)
      PUT(propUrl, body = "")
      PUT(propUrl, body = text)

      strings <- subset(metadata.file, metadata.file$type == "string", "source_id")
      
        data <- singleVarData
        #subset data
        if (!is.null(longitudinal1)) {
          if (!is.null(myTimeframe1)) {
            data <- subsetDataFetcher(myTimeframe1[1], myTimeframe1[2], singleVarData, longitudinal1)
            message("subsetting data by first longitudinal variable..")
            if (nrow(data) == 0) {
              message("subset failed, returning")
              return()
            }
          }
          if (!is.null(longitudinal2)) {
            if (!is.null(myTimeframe2)) {
              data <- subsetDataFetcher(myTimeframe2[1], myTimeframe2[2], data, longitudinal2)
              message("subsetting data by second longitudinal variable..")
              if (nrow(data) == 0) {
                message("subset failed, returning")
                return()
              }
            }
          }
        }
        if (myX %in% strings$source_id) {
          #data <- setDT(data)[, lapply(.SD, function(x) unlist(tstrsplit(x, " | ", fixed=TRUE))), 
          #                    by = setdiff(names(data), eval(myX))][!is.na(eval(myX))]
          if (any(grepl("|", data[[myX]], fixed=TRUE))) {
            data <- separate_rows(data, myX, sep = "[|]+")
          }
        }
     
      nums <- getNums(metadata.file)
      dates <- getDates(metadata.file)      

      #for handling facets, this works for direct. need if statement
      if (facetType == "direct") {
        if (myFacet %in% nums$source_id | myFacet %in% dates$source_id) {
          data[[myFacet]] <- rcut_number(data[[myFacet]],4)
        }
        displayLabel <- metadata.file$property[metadata.file$source_id == myFacet]
        data[[myFacet]] <- paste0(displayLabel, ": ", data[[myFacet]])  
      } else if (facetType == "makeGroups") {
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
          aggKey <- aggKey()
          outData <- makeGroups(data, metadata.file, myFacet, facet_stp1, facet_stp2, facet_stp3, facet_stp4, aggKey)
          observations <- metadata.file$source_id[metadata.file$category == "Observation"]
          observations <- observations[observations %in% colnames(singleVarData)]
          label <- makeGroupLabel(myFacet, metadata.file, facet_stp1, facet_stp2, facet_stp3, facet_stp4, event.list = observations)
          #add makeGroups data to df and return
          colnames(outData) <- c(aggKey, "FACET")
          outData <- transform(outData, "FACET" = ifelse(as.numeric(FACET) == 0, label[2], label[1]))
          data <- merge(data, outData, by = aggKey, all = TRUE)
        }
      }
      
      if (facet2Type == "direct") {
        if (myFacet2 %in% nums$source_id | myFacet2 %in% dates$source_id) {
          data[[myFacet2]] <- rcut_number(data[[myFacet2]],4)
        }
        displayLabel <- metadata.file$property[metadata.file$source_id == myFacet2]
        data[[myFacet2]] <- paste0(displayLabel, ": ", data[[myFacet2]])  
      } else if (facet2Type == "makeGroups") {
        numeric <- c("lessThan", "greaterThan", "equals")
        anthro <- c("percentDays", "delta", "direct")
        if (facet2Type != "none") {
          if (is.null(facet2_stp1)) {
            return()
          } else {
            if (facet2_stp1 %in% numeric) {
              if (is.null(facet2_stp2)) {
                return()
              }
            }
            if (facet2_stp1 %in% anthro) {
              if (facet2_stp1 == "percentDays") {
                if (is.null(facet2_stp4)) {
                  return()
                }
              } else {
                if (is.null(facet2_stp3)) {
                  return()
                }
              }
            }
          }
          aggKey <- aggKey()
          outData <- makeGroups(data, metadata.file, myFacet2, facet2_stp1, facet2_stp2, facet2_stp3, facet2_stp4, aggKey)
          observations <- metadata.file$source_id[metadata.file$category == "Observation"]
          observations <- observations[observations %in% colnames(singleVarData)]
          label <- makeGroupLabel(myFacet2, metadata.file, facet2_stp1, facet2_stp2, facet2_stp3, facet2_stp4, event.list = observations)
          #add makeGroups data to df and return
          colnames(outData) <- c(aggKey, "FACET2")
          outData <- transform(outData, "FACET2" = ifelse(as.numeric(FACET2) == 0, label[2], label[1]))
          data <- merge(data, outData, by = aggKey, all = TRUE)
        }
      }
      
      if (prtcpntView$val == TRUE) {
        if (facetType == "makeGroups") {
          facetCol = "FACET"
        } else if (myX == myFacet | myFacet == "none" | myFacet == "") {
          facetCol = c()
        }  else {
          facetCol = myFacet
        }
        if (facet2Type == "makeGroups") {
          facet2Col = "FACET2" 
        } else if (myX == myFacet2 | myFacet2 == "none" | myFacet2 == "") {
          facet2Col = c()
        } else {
          facet2Col = myFacet2
        }
        
        myCols <- c("Participant_Id", myX, facetCol, facet2Col)
        
        data <- data[, myCols, with = FALSE]
        data <- unique(data)
      }

      data
      
    }), 2000)
    
})
