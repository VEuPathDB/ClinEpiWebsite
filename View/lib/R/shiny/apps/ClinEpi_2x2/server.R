# server.r

#the sliders only show transparent for the first two to appear, names not reused/ dont refer only to whats on screen at the time
#do we want to move naToZero before the merge and only apply to outdata? seems NA for prtcpnt and house info are real NAs

shinyServer(function(input, output, session) {
 
  attributes.file <- NULL
  metadata.file <- NULL
  singleVarData <- NULL
  longitudinal.file <- NULL
  current <- NULL
  attrInfo <- NULL
  outInfo <- NULL
  facetInfo <- NULL
  facet2Info <- NULL 
  attributes.file <- NULL
  propUrl <- NULL
  properties <- NULL
  longitudinal1 <- NULL
  longitudinal2 <- NULL
  project.id <- NULL
  isParticipant <- NULL
  model.prop <- NULL
  getMyAttr <- reactiveValues()
  getMyOut <- reactiveValues()
  prtcpntView <- reactiveValues()
  prtcpntView$val <- TRUE
  getMyFacet <- reactiveValues()
  getMyFacet2 <- reactiveValues()
  metadata.classes <- NULL

  filesFetcher <- reactive({
  if (is.null(propUrl)) {
     propUrl <<- getPropertiesUrl(session)
     properties <<- try(fread(propUrl))
    if (grepl("Error", properties)) {
      properties <<- NULL
    }
  }
  message(paste("propUrl:", propUrl))

  if (is.null(attributes.file)) {

     attribute_temp <- try(fread(
        getWdkDatasetFile('attributes.tab', session, FALSE, dataStorageDir),
        header=TRUE,
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
      
        metadata.classes <<- metadata.file 

        if ('Participant_Id' %in% colnames(attributes.file)) {
          attributes.file <- attributes.file[, Participant_Id:=as.character(Participant_Id)]
          isParticipant <<- TRUE
          metadata.file <<- rbind(metadata.file, list("custom", "Selected Participants", "string", "Participants", "Participant"))
          metadata.file <<- rbind(metadata.file, list("dontcare", "Participants", "string", "Search Results", "Participant"))
          metadata.file <<- rbind(metadata.file, list("dontcare2", "Dynamic Attributes", "string", "Search Results", "Participant"))
          metadata.file <<- rbind(metadata.file, list("dontcare3", "Search Results", "string", "null", "Participant"))
          metadata.file <<- rbind(metadata.file, list("Avg_Female_Anopheles", "Avg Female Anopheles", "number", "Dynamic Attributes", "Participant"))
          metadata.file <<- rbind(metadata.file, list("Matching_Observations_/_Year", "Matching Observations / Year", "number", "Dynamic Attributes", "Participant"))
          metadata.file <<- rbind(metadata.file, list("Years_of_Observation", "Years of Observations", "number", "Dynamic Attributes", "Participant"))
          } else {
          attributes.file <- attributes.file[, Observation_Id:=as.character(Observation_Id)]
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
    classes <- metadata.classes$type[metadata.classes$category != "Entomological measurements"]
    names(classes) <- metadata.classes$source_id[metadata.classes$category != "Entomological measurements"]
    classes[classes == "string"] <- "character"
    classes[classes == "number"] <- "double"
    classes <- classes[!classes == "null"]
    classes[classes == "date"] <- "character"
    classes <- c(classes, "Participant_Id" = "character", "Observation_Id" = "character")
    classes <- classes[!duplicated(names(classes))]
    rm(metadata.classes)

    singleVarData <<- fread(paste0(mirror.dir, "shiny_masterDataTable.txt"), colClasses = classes)
   
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
    singleVarData <<- singleVarData[, (drop):=NULL]
    
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
        longitudinal1 <<- longitudinal.file$columns[1]
        longitudinal2 <<- NULL
      } else {
        longitudinal1 <<- subset(longitudinal.file, longitudinal.file$columns %in% dates)$columns[1]
        longitudinal2 <<- subset(longitudinal.file, longitudinal.file$columns %in% nums)$columns[1]
      }
    }

    singleVarData
  }
 
  output$title <- renderText({
    withProgress(message = 'Loading... May take a minute', value = 0, style = "old", {
      if (is.null(singleVarData)) {
        singleVarDataFetcher()
      }
      incProgress(.45)
      timelineInit()
      incProgress(.15)
      attrInit()
      outInit()
      incProgress(.25)
      facetInit()
      facet2Init()
      incProgress(.15)
    })
    c("Contingency Tables")
  })

  timelineInit <- reactive({
    current <<- callModule(timeline, "timeline", singleVarData, longitudinal.file, metadata.file)
  })

  attrInit <- reactive({
    attrInfo <<- callModule(customGroups, "attr", groupLabel = reactive(NULL), metadata.file = metadata.file, include = reactive(c("all")), singleVarData = singleVarData, selected = selectedAttr, moduleName = "attrInfo", prtcpntView = reactive(prtcpntView$val))
      if (is.null(properties)) {
        getMyAttr$val <- selectedAttr()
      } else {
        getMyAttr$val <- properties$selected[properties$input == "attrInfo$group"]
      }
  })

  outInit <- reactive({
    outInfo <<- callModule(customGroups, "out", groupLabel = reactive(NULL), include = reactive(c("all")), metadata.file = metadata.file, singleVarData = singleVarData, selected = reactive("custom"), moduleName = "outInfo", prtcpntView = reactive(prtcpntView$val))
      if (is.null(properties)) {
        getMyOut$val <- "custom"
      } else {
        getMyOut$val <- properties$selected[properties$input == "outInfo$group"]
      }
  })

  facetInit <- reactive({
    facetInfo <<- callModule(customGroups, "facet", groupLabel = facetLabel, metadata.file = metadata.file, include = facetData, singleVarData = singleVarData, selected = selectedFacet, groupsType = reactive(input$facetType), groupsTypeID = "input$facetType", moduleName = "facetInfo", prtcpntView = reactive(prtcpntView$val))
      if (is.null(properties)) {
        getMyFacet$val <- selectedFacet()
      } else {
        getMyFacet$val <- properties$selected[properties$input == "facetInfo$group"]
      }
  })

  facet2Init <- reactive({
     facet2Info <<- callModule(customGroups, "facet2", groupLabel = facet2Label, metadata.file = metadata.file, include = facet2Data, singleVarData = singleVarData, selected = selectedFacet2, groupsType = reactive(input$facet2Type), groupsTypeID = "input$facet2Type", moduleName = "facet2Info", prtcpntView = reactive(prtcpntView$val))
      if (is.null(properties)) {
        getMyFacet2$val <- selectedFacet2()
      } else {
        getMyFacet2$val <- properties$selected[properties$input == "facet2Info$group"]
      }
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

  selectedAttr <- reactive({
    if ("EUPATH_0000338" %in% colnames(singleVarData)) {
      selected <- "EUPATH_0000338"
    } else {
        temp <- metadata.file
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

    observeEvent(attrInfo$group, {
      if (length(get_selected(attrInfo$group, format="names")) != 0) {

        mySelected <- get_selected(attrInfo$group, format="names")[[1]]
        myProp <- mySelected[1]
        myParent <- unlist(attributes(mySelected))[length(unlist(attributes(mySelected)))]
        if (length(myParent) != 0) {
          nextAttr <- metadata.file$source_id[metadata.file$property == myProp & metadata.file$parent == myParent]
        } else {
          nextAttr <- metadata.file$source_id[metadata.file$property == myProp & (metadata.file$parent == "null" | metadata.file$parent == "" | is.null(metadata.file$parent))]
        }
        nextAttr <- unique(nextAttr)

        if (length(nextAttr) != 1) {
          message("Warning: non-unique source_ids returned ", nextAttr)
        } 

        #if (is.null(getMyAttr$val)) {
          getMyAttr$val <- nextAttr
        #} else if (getMyAttr$val != nextAttr) {
        #  getMyAttr$val <- nextAttr
        #}
      }
    })

    observeEvent(outInfo$group, {
      if (length(get_selected(outInfo$group, format="names")) != 0) {

        mySelected <- get_selected(outInfo$group, format="names")[[1]]
        myProp <- mySelected[1]
        myParent <- unlist(attributes(mySelected))[length(unlist(attributes(mySelected)))]
        if (length(myParent) != 0) {
          nextOut <- metadata.file$source_id[metadata.file$property == myProp & metadata.file$parent == myParent]
        } else {
          nextOut <- metadata.file$source_id[metadata.file$property == myProp & (metadata.file$parent == "null" | metadata.file$parent == "" | is.null(metadata.file$parent))]
        }
        nextOut <- unique(nextOut)

        if (length(nextOut) != 1) {
          message("Warning: non-unique source_ids returned ", nextOut)
        }

        #if (is.null(getMyOut$val)) {
          getMyOut$val <- nextOut
        #} else if (getMyOut$val != nextOut) {
        #  getMyOut$val <- nextOut
        #}
      }
    })

    #tried to wrap these three into one observer and it broke.. look again later
    observeEvent(getMyAttr$val, {
      #execute javascript to virtually click outside the dropdown
      print("clicking!!!!!!!!!!!")
      js$virtualBodyClick();
    })

    observeEvent(getMyOut$val, {
      #execute javascript to virtually click outside the dropdown
      print("clicking!!!!!!!!!!!")
      js$virtualBodyClick();
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
        label <- "strata for"
      } else if (facetType != "none") {
        label <- "strata where:"
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
        #selected <- "custom"
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
        label <- "strata for"
      } else if (facet2Type != "none") {
        label <- "strata where:"
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
    
    observeEvent(facetInfo$group, {
      if (length(get_selected(facetInfo$group, format="names")) != 0) {
       
        mySelected <- get_selected(facetInfo$group, format="names")[[1]]
        myProp <- mySelected[1]
        myParent <- unlist(attributes(mySelected))[length(unlist(attributes(mySelected)))]
        if (length(myParent) != 0) {
          nextFacet <- metadata.file$source_id[metadata.file$property == myProp & metadata.file$parent == myParent]
        } else {
          nextFacet <- metadata.file$source_id[metadata.file$property == myProp & (metadata.file$parent == "null" | metadata.file$parent == "" | is.null(metadata.file$parent))]
        }
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
        if (length(myParent) != 0) {
          nextFacet <- metadata.file$source_id[metadata.file$property == myProp & metadata.file$parent == myParent]
        } else {
          nextFacet <- metadata.file$source_id[metadata.file$property == myProp & (metadata.file$parent == "null" | metadata.file$parent == "" | is.null(metadata.file$parent))]
        }  
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
      
      if (prtcpntView$val == TRUE) {
        aggKey <- c("Participant_Id")
      } else {
        #aggKey <- c("Observation_Id")
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
      df <- as.data.table(df)
      facetVals <- unique(df[,myFacet, with=FALSE])
      facetVals <- unlist(facetVals)
      names(facetVals) <- facetVals
      
      if (is.null(properties)) {
        mySelected <- c("abc")
      } else {
        mySelected <- properties$selected[properties$input == 'input$individualPlot_stp1']
      }     
 
      selectInput(inputId = "individualPlot_stp1",
                  label = "Stratify Plot (1) value:",
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
      df <- as.data.table(df)
      facet2Vals <- unique(df[,myFacet2, with=FALSE])
      facet2Vals <- unlist(facet2Vals)
      names(facet2Vals) <- facet2Vals
      
      if (is.null(properties)) {
        mySelected <- c("abc")
      } else {
        mySelected <- properties$selected[properties$input == 'input$individualPlot_stp2']
      }     
 
      selectInput(inputId = "individualPlot_stp2",
                  label = "Stratify Plot (2) value:",
                  choices = facet2Vals,
                  selected = mySelected)
    })
    
    output$individual_plot <- renderPlotly({
      if (is.null(input$facetType)) {
        return()
      }
      if (is.null(input$facet2Type)) {
        return()
      }
      if (is.null(input$individualPlot_stp1) & is.null(input$individualPlot_stp2)) {
        return()
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
      
      df <- plotData()
      df <- as.data.table(df)
      
      if (!is.null(iPlot_stp1)) {
        keep <- c(df[, myFacet, with=FALSE] == iPlot_stp1)
        df <- df[keep,]
      }
      if (!is.null(iPlot_stp2)) {
        keep2 <- c(df[, myFacet2, with=FALSE] == iPlot_stp2)
        df <- df[keep2,]
      }
      
      var1 <- getMyAttr$val
      var2 <- getMyOut$val 
      
      #define axis labels here
      xlab <- metadata.file$property[metadata.file$source_id == var2]
      ylab <- "Proportion"
      
      df$Var2Label <- gsub(xlab, "", df$Var2Label)

      #plot here
      myPlot <- ggplot(data = df, aes(x = Var2Label, y = Proportion, fill = Var1Label))
      myPlot <- myPlot + theme_bw()
      myPlot <- myPlot + labs(y = "", x = "")
      
      myPlot <- myPlot + geom_bar(stat = "identity", position = "fill")
      
      myPlot <- myPlot + scale_fill_manual(name = "", values = viridis(2, begin = .25, end = .75))
      myPlot <- myPlot + theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
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
                         ylab,
                         rep(" ", 10),
                         "\n"),
                       collapse = ""),
        size = 14
      )

      maxChars <- max(nchar(as.vector(df$Var1Label)))
      
      if (is.na(maxChars)) {
        legend_list <- list(x = 100, y = .8)
      } else {  
        if (maxChars <= 35) {
          legend_list <- list(x = 100, y = .8)       
        } else {
          legend_list <- list(x = .5, y = -.5) 
        }
      }

      #myPlotly <- ggplotly(myPlot, tooltip = c("text", "x"))
      myPlotly <- ggplotly(myPlot, width = (0.75*as.numeric(input$dimension[1])), height = as.numeric(input$dimension[2]))
      myPlotly <- plotly:::config(myPlotly, displaylogo = FALSE, collaborate = FALSE)
      legend.title <- metadata.file$property[metadata.file$source_id == var1]
      legend.title <- gsub('(.{1,35})(\\s|$)', '\\1\n', legend.title)
      myPlotly <- add_annotations(myPlotly, text = legend.title, xref="paper",
                                  x=1.02, xanchor = "left",
                                  y=.7, yanchor = "bottom",
                                  legendtitle=TRUE, showarrow=FALSE)
      myPlotly <- layout(myPlotly, margin = list(l = 70, r = 50, b = 200, t = 40), 
                         xaxis = x_list, 
                         yaxis = y_list,
                         legend = legend_list,
                         autosize=TRUE)
      
      myPlotly
    })
    
    output$plot <- renderPlotly({
      print("about to render plot")
        df <- plotData()
        if (is.null(df)) {
          message("plotData returned null!")
          return()
        }
        if (is.null(input$facetType)) {
          return()
        }
        if (is.null(input$facet2Type)) {
          return()
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
        if ("FACET" %in% colnames(df)) {
          myFacet <- "FACET"
        }
        if ("FACET2" %in% colnames(df)) {
          myFacet2 <- "FACET2"
        }
       
        var1 <- getMyAttr$val
        var2 <- getMyOut$val 
 
        #define axis labels here
        xlab <- c(metadata.file$property[metadata.file$source_id == var2][1])
        ylab <- "Proportion"
        
        #determine width of bars for outcome
        #outPos <- tableData[1,3 , with = FALSE]
        #outNeg <- tableData[2,3 , with = FALSE]
        #total <- tableData$Totals[3]
        #OPprop <- outPos / total
        #ONprop <- outNeg / total
        #width <- c( OPprop*1.9, ONprop*1.9, OPprop*1.9, ONprop*1.9)
        #df$width <- width
   
        df$Var2Label <- gsub(xlab, "", df$Var2Label)
        
        #plot here
        myPlot <- ggplot(data = df, aes(x = Var2Label, y = Proportion, fill = Var1Label))
        myPlot <- myPlot + theme_bw()
        myPlot <- myPlot + labs(y = "", x = "")
        
        myPlot <- myPlot + geom_bar(stat = "identity", position = "fill")
        
        if (myFacet2 == "none") {
          if (myFacet != 'none') {
            myPlot <- myPlot + facet_wrap(reformulate(myFacet), ncol = 1) 
          }
        } else {
          if (myFacet != 'none') {
            myPlot <- myPlot + facet_grid(reformulate(myFacet, myFacet2))
          } else {
            myPlot <- myPlot + facet_wrap(reformulate(myFacet2), ncol = 1) 
          }
        }
        
        myPlot <- myPlot + scale_fill_manual(name = "", values = viridis(2, begin = .25, end = .75))
        myPlot <- myPlot + theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
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
                         ylab,
                         rep(" ", 10),
                         "\n"),
                         collapse = ""),
          size = 14
        )       
        maxChars <- max(nchar(as.vector(df$Var1Label)))
        if (is.na(maxChars)) {
          legend_list <- list(x = 100, y = .8)
        } else {
          if (maxChars <= 35) {
            legend_list <- list(x = 100, y = .8)
          } else {
            legend_list <- list(x = .5, y = -.5)
          }
        } 

        #myPlotly <- ggplotly(myPlot, tooltip = c("text", "x"))
        myPlotly <- ggplotly(myPlot, width = (0.75*as.numeric(input$dimension[1])), height = as.numeric(input$dimension[2]))
        myPlotly <- plotly:::config(myPlotly, displaylogo = FALSE, collaborate = FALSE)
        legend.title <- metadata.file$property[metadata.file$source_id == var1]
        legend.title <- gsub('(.{1,35})(\\s|$)', '\\1\n', legend.title)
        myPlotly <- add_annotations(myPlotly, text = legend.title, xref="paper",
                                    x=1.02, xanchor = "left",
                                    y=.7, yanchor = "bottom",
                                    legendtitle=TRUE, showarrow=FALSE)
        myPlotly <- layout(myPlotly, margin = list(l = 70, r = 50, b = 200, t = 40), 
                                     xaxis = x_list, 
                                     yaxis = y_list,
                                     legend = legend_list,
                                     autosize=TRUE)
        
        myPlotly
      
    })
    
    observeEvent(plotData(), {
      plotData <- as.data.table(plotData())
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
      if ("FACET" %in% colnames(plotData)) {
        myFacet <- "FACET"
      }
      if ("FACET2" %in% colnames(plotData)) {
        myFacet2 <- "FACET2"
      }
      myPrtcpntView <- prtcpntView$val
      
      if (myFacet2 != "none") {
        if (myFacet != "none") {
          dt_len <- uniqueN(plotData[, c(myFacet, myFacet2), with=FALSE])
          dt_list <- unique(plotData[, c(myFacet, myFacet2), with=FALSE])
        } else {
          dt_len <- uniqueN(plotData[, myFacet2, with=FALSE])
          dt_list <- unique(plotData[, myFacet2, with=FALSE])
          myFacet <- myFacet2
        }
      } else {
        if (myFacet != "none") {
          dt_len <- uniqueN(plotData[, myFacet, with=FALSE])
          dt_list <- unique(plotData[, myFacet, with=FALSE])
        } else {
          dt_len <- 1
          dt_list <- NULL
        }
      }
      
      createTableUI <- function(id, data, facets) {
        
        print(data)
        #get or
        #TODO double check i've pulled these out right !!!!
        APOP <- data$Proportion[data$Variable1 == "Attribute+" & data$Variable2 == "Outcome+"]
        AFOP <- data$Proportion[data$Variable1 == "Attribute-" & data$Variable2 == "Outcome+"]
        APOF <- data$Proportion[data$Variable1 == "Attribute+" & data$Variable2 == "Outcome-"]
        AFOF <- data$Proportion[data$Variable1 == "Attribute-" & data$Variable2 == "Outcome-"]
        
        APtotal <- APOP + APOF
        AFtotal <- AFOP + AFOF
        OPtotal <- APOP + AFOP
        OFtotal <- APOF + AFOF
        total <- APOP + APOF + AFOP + AFOF
        
        OP <- c(APOP, AFOP, OPtotal)
        OF <- c(APOF, AFOF, OFtotal)
        totals <- c(APtotal, AFtotal, total)
        
        OPLabel <- data$Var2Label[data$Variable2 == "Outcome+"][1]
        OFLabel <- data$Var2Label[data$Variable2 == "Outcome-"][1]
        APLabel <- data$Var1Label[data$Variable1 == "Attribute+"][1]
        AFLabel <- data$Var1Label[data$Variable1 == "Attribute-"][1]
        tableData <- data.table(col1 = OP, col2 = OF, "Totals" = totals)
        colnames(tableData) <- c(as.vector(OPLabel), as.vector(OFLabel), "Totals")
        rownames(tableData) <- c(as.vector(APLabel), as.vector(AFLabel), "Totals")
        
        if (all(sort(colnames(tableData)) == c("No", "Totals", "Yes"))) {
          setcolorder(tableData, c("Yes", "No", "Totals")) 
        }
        if (all(sort(rownames(tableData)) == c("No", "Totals", "Yes"))) {
          setroworder(tableData, c(2,1,3))
          rownames(tableData) <- c("Yes", "No", "Totals")
        }
        
        if (length(facets) > 0) {
          myCaption <- paste("Strata:",paste(facets, collapse = " and "))
        } else {
          myCaption <- ""
        }
        
        output[[id]] <- DT::renderDataTable(datatable(tableData,
                                                      caption = myCaption,
                                                      width = '100%',
                                                      options = list(
                                                      sDom = '<"top"><"bottom">',
                                                      autoWidth = TRUE, 
                                                      columnDefs = list(list(width = '250px', className = 'dt-right', targets = c(1,2,3)))
                                                      )
        ))
      }
      
      output$table <- renderUI({
        lapply(1:dt_len, function(i) {
          id <- paste0("table", i)
          facets <- c()
          if (!is.null(dt_list)) {
            if (length(dt_list) == 1) {
              keep <- c(plotData[, myFacet, with=FALSE] == c(dt_list[i]))
              facets <- c(dt_list[i])
              data <- plotData[keep,] 
            } else {
              keep1 <- c(plotData[, myFacet, with=FALSE] == c(dt_list[i][, myFacet, with=FALSE]))
              keep2 <- c(plotData[, myFacet2, with=FALSE] == c(dt_list[i][, myFacet2, with=FALSE]))
              keep <- keep1 & keep2
              facets <- c(c(dt_list[i][, myFacet, with=FALSE]), c(dt_list[i][, myFacet2, with=FALSE]))
              data <- plotData[keep,]
            }
          } else {
            data <- plotData
            facets <- c()
          }
          createTableUI(id, data, facets)
        })
      })
    })
    
    observeEvent(plotData(), {
      plotData <- as.data.table(plotData())
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
      if ("FACET" %in% colnames(plotData)) {
        myFacet <- "FACET"
      }
      if ("FACET2" %in% colnames(plotData)) {
        myFacet2 <- "FACET2"
      }
      myPrtcpntView <- prtcpntView$val
      
      if (myFacet2 != "none") {
        if (myFacet != "none") {
          dt_len <- uniqueN(plotData[, c(myFacet, myFacet2), with=FALSE])
          dt_list <- unique(plotData[, c(myFacet, myFacet2), with=FALSE])
        } else {
          dt_len <- uniqueN(plotData[, myFacet2, with=FALSE])
          dt_list <- unique(plotData[, myFacet2, with=FALSE])
          myFacet <- myFacet2
        }
      } else {
        if (myFacet != "none") {
          dt_len <- uniqueN(plotData[, myFacet, with=FALSE])
          dt_list <- unique(plotData[, myFacet, with=FALSE])
        } else {
          dt_len <- 1
          dt_list <- NULL
        }
      }
      
      createUI <- function(id, data, facets) {
        
        print(data)
        #get or
        #TODO double check i've pulled these out right !!!!
        a <- data$Proportion[data$Variable1 == "Attribute+" & data$Variable2 == "Outcome+"]
        b <- data$Proportion[data$Variable1 == "Attribute-" & data$Variable2 == "Outcome+"]
        c <- data$Proportion[data$Variable1 == "Attribute+" & data$Variable2 == "Outcome-"]
        d <- data$Proportion[data$Variable1 == "Attribute-" & data$Variable2 == "Outcome-"]
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
        
        #get pval
        df <- data.frame(col1 = c(a,c), col2 =c(b,d))
        p <- chisq.test(df)
        p <- p$p.value
        p <- round(p, digits=4)
        if (p != "NaN" & p < 0.0001) {
          p <- "<0.0001"
        }
        #make stats table
        odds.ratio <- c(OR, paste(ORlo, "-", ORhi))
        relative.risk <- c(RR, paste(RRlo, "-", RRhi))
        p.val <- c(p, "N/A")
        stats <- data.table("p-value" = p.val, "Odds Ratio" = as.character(odds.ratio), "Relative Risk" = as.character(relative.risk))
        rownames(stats) <- c("Statistics", "95% Confidence Interval")
        
        if (length(facets) > 0) {
          myCaption <- paste("Strata:",paste(facets, collapse = " and "))
        } else {
          myCaption <- ""
        }
        
        output[[id]] <- DT::renderDataTable(datatable(stats,
                                                      caption = myCaption,
                                                      width = '100%',
                                                      options = list(
                                                      sDom = '<"top"><"bottom">',
                                                      autoWidth = TRUE, 
                                                      columnDefs = list(list(width = '250px', className = 'dt-right', targets = c(1,2,3)))
                                                      )
        ))
      }
      
      output$statsTable <- renderUI({
        lapply(1:dt_len, function(i) {
          id <- paste0("statsTable", i)
          facets <- c()
          if (!is.null(dt_list)) {
            if (length(dt_list) == 1) {
              keep <- c(plotData[, myFacet, with=FALSE] == c(dt_list[i]))
              facets <- c(dt_list[i])
              data <- plotData[keep,] 
            } else {
              keep1 <- c(plotData[, myFacet, with=FALSE] == c(dt_list[i][, myFacet, with=FALSE]))
              keep2 <- c(plotData[, myFacet2, with=FALSE] == c(dt_list[i][, myFacet2, with=FALSE]))
              keep <- keep1 & keep2
              facets <- c(c(dt_list[i][, myFacet, with=FALSE]), c(dt_list[i][, myFacet2, with=FALSE]))
              data <- plotData[keep,]
            }
          } else {
            data <- plotData
            facets <- c()
          }
          createUI(id, data, facets)
        })
      })
    })
    
    #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    #values grabbed through reactive functions for better control of reactive context

    #all the work will be done here in prepping data
    plotData <- reactive({
      #collecting inputs 
      myTimeframe1 <- current$range1
      myTimeframe2 <- current$range2
      if (is.null(getMyAttr$val)) {
        message("attr group is null")
        return()
      } else {
        message("setting myAttr")
        myAttr <- getMyAttr$val
      }
      if (is.null(getMyOut$val)) {
        print("out group is null")
        return()
      } else {
        print("setting myOut")
        myOut <- getMyOut$val
      }
      if (is.null(input$facetType)) {
        return()
      }
      if (is.null(input$facet2Type)) {
        return()
      }
      facetType <- input$facetType
      facet2Type <- input$facet2Type
      
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
      
      print("have all inputs")
      
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
      } else {
        data <- singleVarData
      }
 
      go <- TRUE
      
      if (is.null(outInfo$group_stp1) | is.null(attrInfo$group_stp1)) {
        print("out or attr stp1 is null")
        go <- FALSE
      } else {
        if (outInfo$group_stp1 == 'any' | outInfo$group_stp1 == 'all') {
          if (is.null(outInfo$group_stp2)) {
            return()
          } else {
            if (outInfo$group_stp2 %in% c("lessThan", "greaterThan", "equals")) {
              if (is.null(outInfo$group_stp3)) {
                return()
              }
            }
          }
        }
        if (attrInfo$group_stp1 == 'any' | attrInfo$group_stp1 == 'all') {
          if (is.null(attrInfo$group_stp2)) {
            return()
          } else {
            if (attrInfo$group_stp2 %in% c("lessThan", "greaterThan", "equals")) {
              if (is.null(attrInfo$group_stp3)) {
                return()
              }
            }
          }
        }
      }
     
      #once last field is populated .. GO
      if (go) {
        message("GO!!")
        #grab validated inputs
        out_stp1 <- outInfo$group_stp1
        out_stp3 <- outInfo$group_stp3
        out_stp4 <- outInfo$group_stp4
        out_stp2 <- outInfo$group_stp2
        attr_stp1 <- attrInfo$group_stp1
        attr_stp2 <- attrInfo$group_stp2
        attr_stp3 <- attrInfo$group_stp3
        attr_stp4 <- attrInfo$group_stp4
  
        #first thing is to save properties
        longitudinalText <- longitudinalText(myTimeframe1, myTimeframe2)
        attrText <- groupText("attrInfo", myAttr, attr_stp1, attr_stp2, attr_stp3, attr_stp4)
        outText <- groupText("outInfo", myOut, out_stp1, out_stp2, out_stp3, out_stp4)  
        facetText <- groupText("facetInfo", myFacet, facet_stp1, facet_stp2, facet_stp3, facet_stp4)
        facet2Text <- groupText("facet2Info", myFacet2, facet2_stp1, facet2_stp2, facet2_stp3, facet2_stp4)

        text <- paste0("input\tselected\n",
                       longitudinalText,
                       attrText,
                       outText,
                       facetText,
                       facet2Text,
                       "input$facetType\t", input$facetType, "\n",
                       "input$facet2Type\t", input$facet2Type
                       #"input$individualPlot_stp1\t", input$individualPlot_stp1, "\n",
                       #"input$individualPlot_stp2\t", input$individualPlot_stp2 
                      )

        PUT(propUrl, body = "")
        PUT(propUrl, body = text)   
 
        #get attr col
        attrData <- completeDT(data, myAttr)
        attrData <- getFinalDT(attrData, metadata.file, myAttr)
        #myCols <- c("Participant_Id", myAttr)
        aggKey <- aggKey()
        myCols <- c(aggKey, myAttr)
        attrData <- attrData[, myCols, with=FALSE]
        
        numeric <- c("lessThan", "greaterThan", "equals")
        anthro <- c("percentDays", "delta", "direct")
        nums <- getNums(metadata.file)
        dates <- getDates(metadata.file)
        
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
        attrData <- makeGroups(attrData, metadata.file, myAttr, attr_stp1, attr_stp2, attr_stp3, attr_stp4, aggKey)
        observations <- metadata.file$source_id[metadata.file$category == "Observation"]
        observations <- observations[observations %in% colnames(singleVarData)]
        attrLabel <- makeGroupLabel(myAttr, metadata.file, attr_stp1, attr_stp2, attr_stp3, attr_stp4, observations)
        #colnames(attrData) <- c("Participant_Id", "Attribute")
        colnames(attrData) <- c(aggKey, "Attribute")
       print(attrData)
        #get outcome data
        #may not need to do the splitting on pipes. grepl will still return true for it.
        outData <- completeDT(data, myOut)
        outData <- getFinalDT(outData, metadata.file, myOut)
        #myCols <- c("Participant_Id", myOut)
        myCols <- c(aggKey, myOut)
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
        
        outData <- makeGroups(outData, metadata.file, myOut, out_stp1, out_stp2, out_stp3, out_stp4, aggKey)
        observations <- metadata.file$source_id[metadata.file$category == "Observation"]
        observations <- observations[observations %in% colnames(singleVarData)]
        outLabel <- makeGroupLabel(myOut, metadata.file, out_stp1, out_stp2, out_stp3, out_stp4, observations)
        #colnames(outData) <- c("Participant_Id", "Outcome")
        colnames(outData) <- c(aggKey, "Outcome")
        print(outData)
        
        aggKey <- aggKey()
        facetData <- data.table()
        facet2Data <- data.table()
        
        #for handling facets, this works for direct. need if statement
        if (facetType == "direct") {
          myCols <- c(aggKey, myFacet)
          facetData <- data[, myCols, with=FALSE]
          if (myFacet %in% nums$source_id | myFacet %in% dates$source_id) {
            facetData[[myFacet]] <- rcut_number(facetData[[myFacet]],4)
          }
          displayLabel <- metadata.file$property[metadata.file$source_id == myFacet]
          facetData[[myFacet]] <- paste0(displayLabel, ": ", facetData[[myFacet]])
          facetData <- unique(facetData)          
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
        
            facetData <- makeGroups(data, metadata.file, myFacet, facet_stp1, facet_stp2, facet_stp3, facet_stp4, aggKey)
            observations <- metadata.file$source_id[metadata.file$category == "Observation"]
            observations <- observations[observations %in% colnames(singleVarData)]
            label <- makeGroupLabel(myFacet, metadata.file, facet_stp1, facet_stp2, facet_stp3, facet_stp4, event.list = observations)
            #add makeGroups data to df and return
            colnames(facetData) <- c(aggKey, "FACET")
            facetData <- transform(facetData, "FACET" = ifelse(as.numeric(FACET) == 0, label[2], label[1]))
          }
        }
        
        if (facet2Type == "direct") {
          myCols <- c(aggKey, myFacet2)
          facet2Data <- data[, myCols, with=FALSE]
          if (myFacet2 %in% nums$source_id | myFacet2 %in% dates$source_id) {
            facet2Data[[myFacet2]] <- rcut_number(facet2Data[[myFacet2]],4)
          }
          displayLabel <- metadata.file$property[metadata.file$source_id == myFacet2]
          facet2Data[[myFacet2]] <- paste0(displayLabel, ": ", facet2Data[[myFacet2]])
          facetData <- unique(facetData)
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
          
            facet2Data <- makeGroups(data, metadata.file, myFacet2, facet2_stp1, facet2_stp2, facet2_stp3, facet2_stp4, aggKey)
            observations <- metadata.file$source_id[metadata.file$category == "Observation"]
            observations <- observations[observations %in% colnames(singleVarData)]
            label <- makeGroupLabel(myFacet2, metadata.file, facet2_stp1, facet2_stp2, facet2_stp3, facet2_stp4, event.list = observations)
            #add makeGroups data to df and return
            colnames(facet2Data) <- c(aggKey, "FACET2")
            facet2Data <- transform(facet2Data, "FACET2" = ifelse(as.numeric(FACET2) == 0, label[2], label[1]))
          }
        }
        print(head(facetData))
        print(head(facet2Data))
        #merge on participant id and keep all prtcpnts.
        #data <- merge(attrData, outData, by = "Participant_Id", all = TRUE)
        data <- merge(attrData, outData, by = aggKey, all = TRUE)
        if (nrow(facetData) > 0) {
          data <- merge(data, facetData, by = aggKey, all = TRUE)
        }
        if (nrow(facet2Data) > 0) {
          data <- merge(data, facet2Data, by = aggKey, all = TRUE) 
        }
        
        #replace NA with 0, essentially assuming that no reporting is negative reporting
        #TODO remove this and just ignore NAs instead
        naToZero(data)
        
        #format into 2x2
        data <- transform(data, "APOP" = ifelse( Attribute == 1 & Outcome == 1, 1, 0))
        data <- transform(data, "APOF" = ifelse( Attribute == 1 & Outcome == 0, 1, 0))
        data <- transform(data, "AFOP" = ifelse( Attribute == 0 & Outcome == 1, 1, 0))
        data <- transform(data, "AFOF" = ifelse( Attribute == 0 & Outcome == 0, 1, 0))
        
        #TODO use aggregate here so we can sum based on facets
        if (facetType == "makeGroups") {
          facetCol = "FACET"
        } else if (myFacet == "none") {
          facetCol = c()
        }  else {
          facetCol = myFacet
        }
        if (facet2Type == "makeGroups") {
          facet2Col = "FACET2" 
        } else if (myFacet2 == "none") {
          facet2Col = c()
        } else {
          facet2Col = myFacet2
        }
        facets <- c(facetCol, facet2Col)
        if (length(facets) == 0) {
          APOP <- sum(data$APOP)
          APOF <- sum(data$APOF)
          AFOP <- sum(data$AFOP)
          AFOF <- sum(data$AFOF)
          
          Proportion <- c(APOP,APOF,AFOP,AFOF)
          Variable1 <- c("Attribute+", "Attribute+", "Attribute-", "Attribute-")
          Variable2 <- c("Outcome+", "Outcome-", "Outcome+", "Outcome-")
          returnData <- data.table(Proportion, Variable1, Variable2)
        } else {
          returnData <- aggregate(as.formula(paste("APOP ~ ", paste(facets, collapse = "+"))), data, sum)
          APOF <- aggregate(as.formula(paste("APOF ~ ", paste(facets, collapse = "+"))), data, sum)
          returnData <- merge(returnData, APOF, by = facets)
          AFOP <- aggregate(as.formula(paste("AFOP ~ ", paste(facets, collapse = "+"))), data, sum)
          returnData <- merge(returnData, AFOP, by = facets)
          AFOF <- aggregate(as.formula(paste("AFOF ~ ", paste(facets, collapse = "+"))), data, sum)
          returnData <- merge(returnData, AFOF, by = facets)
          
          returnData <- gather(returnData, key, Proportion, -facets)
          returnData <- transform(returnData, "Variable1" = ifelse( key == "APOP" | key == "APOF", "Attribute+", "Attribute-"))
          returnData <- transform(returnData, "Variable2" = ifelse( key == "AFOP" | key == "APOP", "Outcome+", "Outcome-"))
        }
        #data$key <- NULL
        
        #add labels
        returnData <- transform(returnData, "Var1Label" = ifelse( Variable1 == "Attribute+", attrLabel[1], attrLabel[2]))
        returnData <- transform(returnData, "Var2Label" = ifelse( Variable2 == "Outcome+", outLabel[1], outLabel[2]))
        
        print(head(returnData))
        returnData
      } 
      
    })

})
