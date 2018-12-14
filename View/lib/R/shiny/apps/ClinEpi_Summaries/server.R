## server.r

#need to look at dates options at min yaxis and facet line dont seem to work
#for facets ui: consider selecting more than one group/ check boxes ??
#make sure levels for numeric groups always start with the group that has square bracket in front (meaning smallest -> largest)
#figure out when we need naToZero function when building own groups and facets. imagine need it for events stuffs but not others ??
# for some reason delta laz > -.5 returns fewer ppl than >-1.. seems should be reversed

shinyServer(function(input, output, session) {
  
  metadata.file <- NULL
  singleVarData <- NULL
  longitudinal.file <- NULL
  current <- NULL
  facetInfo <- NULL
  facet2Info <- NULL
  groupInfo <- NULL
  attributes.file <- NULL
  propUrl <- NULL
  properties <- NULL
  longitudinal1 <- NULL
  longitudinal2 <- NULL
  #check next three, not sure they need to be here
  project.id <- NULL
  isParticipant <- NULL
  model.prop <- NULL
  getMyY <- reactiveValues()
  getMyGroups <- reactiveValues()
  getMyFacet <- reactiveValues()
  getMyFacet2 <- reactiveValues()
  legendTitle <- NULL
  prtcpntView <- reactiveValues()
  prtcpntView$val <- TRUE
  metadata.classes <- NULL
  contLongitudinal <- FALSE

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
          attributes.file <<- attributes.file[, Participant_Id:=as.character(Participant_Id)]
          isParticipant <<- TRUE
          metadata.file <<- rbind(metadata.file, list("custom", "Selected Participants", "string", "Participants", "Participant"))
          metadata.file <<- rbind(metadata.file, list("dontcare", "Participants", "string", "Search Results", "Participant"))
          metadata.file <<- rbind(metadata.file, list("dontcare2", "Dynamic Attributes", "string", "Search Results", "Participant"))
          metadata.file <<- rbind(metadata.file, list("dontcare3", "Search Results", "string", "null", "Participant"))
          metadata.file <<- rbind(metadata.file, list("Avg_Female_Anopheles", "Avg Female Anopheles", "number", "Dynamic Attributes", "Participant"))
          metadata.file <<- rbind(metadata.file, list("Matching_Observations_/_Year", "Matching Observations / Year", "number", "Dynamic Attributes", "Participant"))
          metadata.file <<- rbind(metadata.file, list("Years_of_Observation", "Years of Observations", "number", "Dynamic Attributes", "Participant"))
          } else {
          attributes.file <<- attributes.file[, Observation_Id:=as.character(Observation_Id)]
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
 
    #specific for gems, temporary fix for house obs so its not treated independantly of other obs
    if (grepl("GEMS", datasetName)) {
      obs <- singleVarData[!is.na(singleVarData$BFO_0000015),]
      obs <- obs[,which(unlist(lapply(obs, function(x)!all(is.na(x))))),with=F]
      houseObs <- singleVarData[is.na(singleVarData$BFO_0000015),]
      houseObs$BFO_0000015 <- houseObs$EUPATH_0015467
      houseObs <- houseObs[,which(unlist(lapply(houseObs, function(x)!all(is.na(x))))),with=F]
      myCols <- colnames(obs)[colnames(obs) %in% colnames(houseObs) & !colnames(obs) %in% c("Participant_Id", "BFO_0000015")]
      houseObs <- houseObs[, !myCols, with=FALSE]
      myStaticCols <- c("Participant_Id", "OBI_0001627")
      static <- houseObs[, myStaticCols, with=FALSE]
      static <- unique(static[!is.na(static$OBI_0001627),])
      houseObs <- houseObs[, !c("OBI_0001627"), with=FALSE]
      houseObs <- merge(static, houseObs, by = "Participant_Id")
      singleVarData <<- merge(obs, houseObs, by = c("Participant_Id", "BFO_0000015"))
    }
   
   if (grepl("PRISM", datasetName)) {
      obs <- singleVarData[is.na(singleVarData$EUPATH_0000054),]
      houseObs <- singleVarData[!is.na(singleVarData$EUPATH_0000054),]
      obs <- obs[,which(unlist(lapply(obs, function(x)!all(is.na(x))))),with=F]
      houseObs <- houseObs[,which(unlist(lapply(houseObs, function(x)!all(is.na(x))))),with=F]
      myCols <- colnames(houseObs)[colnames(houseObs) %in% colnames(obs) & !colnames(houseObs) %in% c("Participant_Id")]
      houseObs <- houseObs[, !myCols, with=FALSE]
      obs <- unique(obs)
      metadata.file <<- metadata.file[metadata.file$category != "Entomological measurements",]
      keep <- c("Participant_Id", colnames(houseObs)[colnames(houseObs) %in% metadata.file$source_id])
      houseObs <- houseObs[, keep, with=FALSE]
      houseObs <- unique(houseObs)
      singleVarData <<- merge(obs, houseObs, by = "Participant_Id")     
    } 
 
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
    strings <- getStrings(metadata.file)$source_id
    if (!nrow(longitudinal.file) == 0) {
      if (all(longitudinal.file$columns %in% dates) | all(longitudinal.file$columns %in% nums) | all(longitudinal.file$columns %in% strings)) {
        numTimelines <- 1
        if (!any(longitudinal.file$columns %in% strings)) {
          contLongitudinal <<- TRUE
        }     
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
  
  #ui stuffs
  output$title <- renderText({
    withProgress(message = 'Loading... May take a minute', value = 0, style = "old", {
      if (is.null(singleVarData)) {
        singleVarDataFetcher()
      } 
      incProgress(.45)
      timelineInit()
      incProgress(.15)
      groupInit()
      incProgress(.25)
      facetInit()
      facet2Init()
      incProgress(.15)
    })
    c("Data Summaries")
  })

  timelineInit <- reactive({
    current <<- callModule(timeline, "timeline", singleVarData, longitudinal.file, metadata.file)
  })

  groupInit <- reactive({
    groupInfo <<- callModule(customGroups, "group", groupLabel = groupLabel, metadata.file = metadata.file, include = groupData, singleVarData = singleVarData, selected = selectedGroup, groupsType = reactive(input$groupsType), groupsTypeID = "input$groupsType", moduleName = "groupInfo", prtcpntView = reactive(prtcpntView$val))
      if (is.null(properties)) {
        getMyGroups$val <- selectedGroup()
      } else {
        getMyGroups$val <- properties$selected[properties$input == "groupInfo$group"]
      }
  })

  facetInit <- reactive({
    facetInfo <<- callModule(customGroups, "facet", groupLabel = facetLabel, metadata.file = metadata.file, include = facetData, singleVarData = singleVarData, selected = selectedFacet, groupsType = reactive(input$facetType), groupsTypeID = "input$facetType", moduleName = "facetInfo", prtcpntView = reactive(prtcpntView$val))
      if (is.null(properties)) {
        getMyFacet$val <- "custom"
      } else {
        getMyFacet$val <- properties$selected[properties$input == "facetInfo$group"]
      }
  })

  facet2Init <- reactive({
    facet2Info <<- callModule(customGroups, "facet2", groupLabel = facet2Label, metadata.file = metadata.file, include = facet2Data, singleVarData = singleVarData, selected = selectedFacet2, groupsType = reactive(input$facet2Type), groupsTypeID = "input$facet2Type", moduleName = "facet2Info", prtcpntView = reactive(prtcpntView$val))
      if (is.null(properties)) {
        getMyFacet2$val <- "custom"
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

    output$xaxis_var <- renderUI({
      if (is.null(longitudinal2)) {
        return()
      }

      if (is.null(properties)) {
        radioButtons(inputId = "xaxisVar",
                     label = NULL,
                     choices = list("My Date Variable" = "dateVar", "My Age Variable" = "ageVar"),
                     selected = "dateVar",
                     inline = TRUE,
                     width = '100%')
      } else {
        radioButtons(inputId = "xaxisVar",
                     label = NULL,
                     choices = list("My Date Variable" = "dateVar", "My Age Variable" = "ageVar"),
                     selected = properties$selected[properties$input == "input$xaxisVar"],
                     inline = TRUE,
                     width = '100%')
      }
    })
    
    output$xaxis_stp2 <- renderUI({
      if (!contLongitudinal) {
        return()
      }
     
      myMin <- 2
      myMax <- 40      

      if (is.null(properties)) {
        if (uniqueN(singleVarData[[longitudinal1]]) > 500) {
          mySelected <- 24
        } else {
          mySelected <- 12
          myMax <- 12 
        }  
      } else {
        mySelected <- properties$selected[properties$input == "input$xaxis_stp2"]
      } 
 
      sliderInput(inputId = "xaxis_stp2",
                  min = myMin,
                  max = myMax,
                  value = mySelected,
                  step = 1,
                  label = "number of bins:")
    })
 
    output$xaxisBox <- renderUI({
       if (!contLongitudinal) {
         return()
       }

       if (is.null(longitudinal2)) {
         tagList(
           box(width = 6, status = "primary", title = "X-Axis",
                        uiOutput("xaxis_stp2")
                    )
         )
       } else { 
         tagList(
           box(width = 6, status = "primary", title = "X-Axis",
                        uiOutput("xaxis_var"),
                        uiOutput("xaxis_stp2")
                    )
         ) 
       }
    })

    output$groupBox <- renderUI({

      if (!contLongitudinal) {
        box(width = 6, status = "primary", title = "X-Axis",
                     uiOutput("groups_type"),
                     customGroupsUI("group", colWidth = 12)
                 )
      } else {
        box(width = 6, status = "primary", title = "Stratify Line",
                     uiOutput("groups_type"),
                     customGroupsUI("group", colWidth = 12)
                 )
      }
    })
 
    output$groups_type <- renderUI({
      mySelected <- properties$selected[properties$input == "input$groupsType"]

      if (is.null(properties)) {
          selectInput(inputId = "groupsType",
                      label = NULL,
                      choices = c("All possible" = "direct", "Make my own" = "makeGroups", "None" = "none"),
                      selected = "direct",
                      width = '100%')
      } else {
          selectInput(inputId = "groupsType",
                      label = NULL,
                      choices = c("All possible" = "direct", "Make my own" = "makeGroups", "None" = "none"),
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
    
#TODO figure how to remove dates. (and why we're doing this...)
    facetData <- reactive({
      if (is.null(input$facetType)) {
        return()
      } else {
        facetType <- input$facetType
      }
      
      if (facetType == "direct") {
        dates <- getDates(metadata.file)$source_id
        #ptmp <- prtcpnt.file[, !dates, with = FALSE]
        #if (house.file.exists) {
          #htmp <- house.file[, !dates, with = FALSE]
          include <- c("Participant", "Household")
        #} else {
        #  include <- c("Participant")
        #}
      } else {
        include <- c("all")
      }
      
      return(include)
    })
    
    output$facet2_type <- renderUI({
      mySelected <- properties$selected[properties$input == "input$facet2Type"]
      if (is.null(properties)) {
        if (isParticipant) {
          selectInput(inputId = "facet2Type",
                      label = NULL,
                      choices = c("All possible" = "direct", "Make my own" = "makeGroups", "None" = "none"),
                      selected = "direct",
                      width = '100%')
        } else {
          selectInput(inputId = "facet2Type",
                      label = NULL,
                      choices = c("All possible" = "direct", "Make my own" = "makeGroups", "None" = "none"),
                      selected = "makeGroups",
                      width = '100%')
        }
      } else {
        selectInput(inputId = "facet2Type",
                    label = NULL,
                    choices = c("All possible" = "direct", "Make my own" = "makeGroups", "None" = "none"),
                    selected = mySelected,
                    width = '100%')
      }
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
    
    #TODO figure how to remove dates. (and why we're doing this...)
    facet2Data <- reactive({
      if (is.null(input$facet2Type)) {
        return()
      } else {
        facet2Type <- input$facet2Type
      }
      
      if (facet2Type == "direct") {
        dates <- getDates(metadata.file)$source_id
        #ptmp <- prtcpnt.file[, !dates, with = FALSE]
        #if (house.file.exists) {
        #htmp <- house.file[, !dates, with = FALSE]
        include <- c("Participant", "Household")
        #} else {
        #  include <- c("Participant")
        #}
      } else {
        include <- c("all")
      }
      
      return(include)
    })
    
    groupLabel <- reactive({
        if (is.null(input$groupsType)) {
          return()
        } else {
          groupsType <- input$groupsType
        }
        longitudinal <- longitudinal1
        if (!is.null(input$xaxisVar)) {
          if (input$xaxisVar == "ageVar") {
            longitudinal <- longitudinal2
          }
        }
      
        if (contLongitudinal) {
          if (groupsType == "direct") {
            label = "strata for"
          } else {
            label = "strata where:"
          }
        } else {
          if (groupsType == "direct") {
            label = "x-axis categories for"
          } else {
            label = "x-axis category where:"
          }
        }
        
        return(label)
    })
   
#figure how to remove dates 
    groupData <- reactive({
      if (is.null(input$groupsType)) {
        return()
      } else {
        groupsType <- input$groupsType
      }
  
      if (groupsType == "direct") {
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
    
    selectedGroup <- reactive({
      if (is.null(input$groupsType)) {
        return()
      } else {
        groupsType <- input$groupsType
      }
      
      if ("EUPATH_0000054" %in% colnames(singleVarData)) {
        selected <- "EUPATH_0000054"
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
        #remove dates
        dates <- getDates(metadata.file)$source_id
        leaves <- leaves[!leaves %in% dates]
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
            #remove dates
            dates <- getDates(metadata.file)$source_id
            leaves <- leaves[!leaves %in% dates]
            selected <- leaves[1]
          }
        } else {
          selected <- "custom"
        }
      } else {
        selected <- ""
      }
#message("selected Facet:", selected)
      return(selected)
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
            #remove dates
            dates <- getDates(metadata.file)$source_id
            leaves <- leaves[!leaves %in% dates]
            selected <- leaves[1]
          }
        } else {
          selected <- "custom"
        }
      } else {
        selected <- ""
      }
      #message("selected Facet:", selected)
      return(selected)
    })
 
    observeEvent(groupInfo$group, {
      if (length(get_selected(groupInfo$group, format="names")) != 0) {

        mySelected <- get_selected(groupInfo$group, format="names")[[1]]
        myProp <- mySelected[1]
        myParent <- unlist(attributes(mySelected))[length(unlist(attributes(mySelected)))]
        if (length(myParent) != 0) {
          nextGroup <- metadata.file$source_id[metadata.file$property == myProp & metadata.file$parent == myParent]
        } else {
          nextGroup <- metadata.file$source_id[metadata.file$property == myProp & (metadata.file$parent == "null" | metadata.file$parent == "" | is.null(metadata.file$parent))]
        }
        nextGroup <- unique(nextGroup)

        if (length(nextGroup) != 1) {
          message("Warning: non-unique source_ids returned ", nextGroup)
        }

        #if (is.null(getMyGroups$val)) {
          getMyGroups$val <- nextGroup
        #} else if (getMyGroups$val != nextGroup) {
        #  getMyGroups$val <- nextGroup
        #}
      }
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

message("nextFacet: ", nextFacet)
        #if (is.null(getMyFacet$val)) {
          getMyFacet$val <- nextFacet
        #} else if (getMyFacet$val != nextFacet) {
        #  getMyFacet$val <- nextFacet
        #}
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

        message("nextFacet: ", nextFacet)
        #if (is.null(getMyFacet2$val)) {
          getMyFacet2$val <- nextFacet
        #} else if (getMyFacet2$val != nextFacet) {
        #  getMyFacet2$val <- nextFacet
        #}
      }
    })
    
    output$yaxis <- renderTree({
     
      longitudinal <- longitudinal1
      if (!is.null(input$xaxisVar)) {
        if (input$xaxisVar == "ageVar") {
          longitudinal <- longitudinal2
        }
      }

      #dont remember why this is
      if (contLongitudinal) {
        include <- c("Observation", "Sample")
      } else {
        include <- c("all")
      }

      outChoiceList <- getUIList(data = singleVarData, metadata.file = metadata.file, include = include)
      
      outChoiceList
    })

    output$choose_yaxis <- renderUI({
      myLabel <- getYaxisLabel()
      
      tagList(
        div(
          dropdownButton(label=myLabel, 
                         status = "default", 
                         tags$div(
                           class = "treeContainer",
                           shinyTree("yaxis", search = TRUE)
                         )),
          style="margin-bottom: 10px"
        )
      )
    })

    getYaxisLabel <- reactive({
      myY <- getMyY$val
      
      if (is.null(myY)) {
        if (is.null(properties)) {
          label <- "Please select one"
        } else {
          myYSourceId <- properties$selected[properties$input == "input$yaxis"]
          label <- metadata.file$property[metadata.file$source_id == myYSourceId]
          getMyY$val <- myYSourceId
        }
      } else {
        label <- metadata.file$property[metadata.file$source_id == myY]
      }
      
      label
    })
    
    observeEvent(input$yaxis, {
      if (length(get_selected(input$yaxis, format="names")) != 0) {
        nextY <- metadata.file$source_id[metadata.file$property == get_selected(input$yaxis, format="names")[1][[1]]][1]
      
        #if (is.null(getMyY$val)) {
          getMyY$val <- nextY
          print("resetting myY")
        #} else if (getMyY$val != nextY) {
        #  getMyY$val <- nextY
        #  print("resetting myY")
        #}
      }
    })

    #tried to wrap these three into one observer and it broke.. look again later
    observeEvent(getMyY$val, {
      #execute javascript to virtually click outside the dropdown
      print("clicking!!!!!!!!!!!")
      js$virtualBodyClick();
    })
    
    observeEvent(getMyGroups$val, {
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
    
    output$yaxis_stp1 <- renderUI({
      if (is.null(getMyY$val)) {
        return()
      }
      myY <- getMyY$val
      myPrtcpntView <- prtcpntView$val
      myYSelected <- properties$selected[properties$input == "input$yaxis"]
      mySelected <- properties$selected[properties$input == "input$yaxis_stp1"]

      attrStp1List <- getUIStp1List(singleVarData, myY)
      nums <- getNums(metadata.file)
      
      dontUseProps <- FALSE
      if (is.null(properties)) {
        dontUseProps <- TRUE
      } else {
        if (myY != myYSelected) {
          dontUseProps = TRUE
        }
      }      

      if (myPrtcpntView == TRUE) {
        if (dontUseProps) {
          if (!myY %in% nums$source_id) {
            selectInput(inputId = "yaxis_stp1",
                        label = "are / is",
                        choices = list("always" = "all", "ever" = "any"),
                        selected = "any",
                        width = '100%')
          }
        } else {
          if (!myY %in% nums$source_id) {
            selectInput(inputId = "yaxis_stp1",
                        label = "are / is",
                        choices = list("always" = "all", "ever" = "any"),
                        selected = mySelected,
                        width = '100%')
          }
        }
      } else {
        maxInputs <- length(attrStp1List)
        if (dontUseProps) {
          if (!myY %in% nums$source_id) {
            selectizeInput(inputId = "yaxis_stp1",
                           label = "are / is",
                           choices = attrStp1List,
                           selected = "EUPATH_0000338",
                           width = '100%',
                           multiple = TRUE,
                           options = list(maxItems = maxInputs,
                                          placeholder = '-Selected Items Will Appear Here-'))
          }
        } else {
          if (!myY %in% nums$source_id) {
            selectizeInput(inputId = "yaxis_stp1",
                           label = "are / is",
                           choices = attrStp1List,
                           selected = mySelected,
                           width = '100%',
                           multiple = TRUE,
                           options = list(maxItems = maxInputs,
                                          placeholder = '-Selected Items Will Appear Here-'))
          }
        }
      }
      

    })
   
    output$yaxis_stp2 <- renderUI({
      if (is.null(getMyY$val)) {
        return()
      }
      myY <- getMyY$val
      myPrtcpntView <- prtcpntView$val
      
      myYSelected <- properties$selected[properties$input == "input$yaxis"]
      mySelected <- properties$selected[properties$input == "input$yaxis_stp2"]

      attrStp1List <- getUIStp1List(singleVarData, myY)
      nums <- getNums(metadata.file)

      dontUseProps <- FALSE
      if (is.null(properties)) {
        dontUseProps <- TRUE
      } else {
        if (myY != myYSelected) {
          dontUseProps = TRUE
        }
      }
 
      if(myPrtcpntView == TRUE) {
        maxInputs <- length(attrStp1List)
        if (dontUseProps) {
          if (!myY %in% nums$source_id) {
            selectizeInput(inputId = "yaxis_stp2",
                           label = NULL,
                           choices = attrStp1List,
                           selected = "EUPATH_0000338",
                           width = '100%',
                           multiple = TRUE,
                           options = list(maxItems = maxInputs,
                                          placeholder = '-Selected Items Will Appear Here-'))
          }
        } else {
          if (!myY %in% nums$source_id) {
            selectizeInput(inputId = "yaxis_stp2",
                           label = NULL,
                           choices = attrStp1List,
                           selected = mySelected,
                           width = '100%',
                           multiple = TRUE,
                           options = list(maxItems = maxInputs,
                                          placeholder = '-Selected Items Will Appear Here-'))
          }
        } 
      }

    })
 
    output$yaxis_stp3 <- renderUI({
      if (is.null(getMyY$val)) {
        return()
      } else {
        myY <- getMyY$val
      }
      nums <- getNums(metadata.file)
      longitudinal <- longitudinal1
      if (!is.null(input$xaxisVar)) {
        if (input$xaxisVar == "ageVar") {
          longitudinal <- longitudinal2
        }
      }
      myYSelected <- properties$selected[properties$input == "input$yaxis"]
      mySelected <- properties$selected[properties$input == "input$yaxis_stp3"]      

      dontUseProps <- FALSE
      if (is.null(properties)) {
        dontUseProps <- TRUE
      } else {
        if (myY != myYSelected) {
          dontUseProps = TRUE
        }
      }

      if (dontUseProps) {
        if (myY %in% nums$source_id) {
          if (contLongitudinal) {
            radioButtons(inputId = "yaxis_stp3",
                         label = "Display as:",
                         choices = list("Mean" = "mean", "Smoothed Conditional Mean" = "smooth"),
                         selected = "smooth",
                         width = '100%',
                         inline = TRUE)
          } else {
            #later think up better way. shouldnt show something with only one option. but this is a required input later on. will need if statements below instead.
            radioButtons(inputId = "yaxis_stp3",
                         label = "Display as:",
                         choices = list("Mean" = "mean"),
                         selected = "mean",
                         width = '100%',
                         inline = TRUE)
          }
        } else {
          if (prtcpntView$val == TRUE) {
            if (is.null(input$yaxis_stp2)) {
              return()
            } 
          } else {
            if (is.null(input$yaxis_stp1)) {
              return()
            }
          }
          
          radioButtons(inputId = "yaxis_stp3",
                       label = "Display as:",
                       choices = list("Count" = "count", "Proportion" = "proportion"),
                       selected = "proportion",
                       width = '100%',
                       inline = TRUE)
        }
      } else {
        if (myY %in% nums$source_id) {
          if (contLongitudinal) {
            radioButtons(inputId = "yaxis_stp3",
                         label = "Display as:",
                         choices = list("Mean" = "mean", "Smoothed Conditional Mean" = "smooth"),
                         selected = mySelected,
                         width = '100%',
                         inline = TRUE)
          } else {
            #later think up better way. shouldnt show something with only one option. but this is a required input later on. will need if statements below instead.
            radioButtons(inputId = "yaxis_stp3",
                         label = "Display as:",
                         choices = list("Mean" = "mean"),
                         selected = mySelected,
                         width = '100%',
                         inline = TRUE)
          }
        } else {
          if (prtcpntView$val == TRUE) {
            if (is.null(input$yaxis_stp2)) {
              return()
            } 
          } else {
            if (is.null(input$yaxis_stp1)) {
              return()
            }
          }
          radioButtons(inputId = "yaxis_stp3",
                       label = "Display as:",
                       choices = list("Count" = "count", "Proportion" = "proportion"),
                       selected = mySelected,
                       width = '100%',
                       inline = TRUE)
        }
      }
      
    })
    
    aggKey <- reactive({
      myPrtcpntView <- prtcpntView$val
      
      if (myPrtcpntView == TRUE) {
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
        return() 
      }

      myFacet <- "FACET" 
      #TODO try to save this globally and reactively have it update?? cause right now its called in three different places..
      df <- plotData()
      
      facetVals <- unique(df[,myFacet, with=FALSE])
      facetVals <- unlist(facetVals)
      names(facetVals) <- facetVals
      
      if (is.null(properties)) {
        message("iPlot1 no properties")
        selectInput(inputId = "individualPlot_stp1",
                    label = "Stratify Plot (1) value:",
                    choices = facetVals)
      } else {
        message("iPlot1 has properties")
        mySelected <- properties$selected[properties$input == 'input$individualPlot_stp1']
        selectInput(inputId = "individualPlot_stp1",
                    label = "Stratify Plot (1) value:",
                    choices = facetVals,
                    selected = mySelected)
      }
    })
    
    output$individualPlot_stp2 <- renderUI({
      if (is.null(input$facet2Type)) {
        return()
      }
      if (input$facet2Type == "none") {
        myFacet2 <- "none"
      } else {
        myFacet2 <- "FACET2"
      }
      facet2Type <- input$facet2Type
      
      if (myFacet2 == "none") {
        return()
      }
      
      #TODO try to save this globally and reactively have it update?? cause right now its called in three different places..
      df <- plotData()
     
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
      if (is.null(input$yaxis_stp3)) {
        return()
      } else {
        plotType <- input$yaxis_stp3
      }
      longitudinal <- longitudinal1
      if (!is.null(input$xaxisVar)) {
        xaxisVar <- input$xaxisVar
        if (xaxisVar == "ageVar") {
          longitudinal <- longitudinal2
        }
      } 
      xaxis_bins <- input$xaxis_stp2
      if (input$facetType == "none") {
        myFacet <- "none"
      } else {
        myFacet <- "FACET"
      }
      facetType <- input$facetType
      if (input$facet2Type == "none") {
        myFacet2 <- "none"
      } else {
        myFacet2 <- "FACET2"
      }
      facet2Type <- input$facet2Type
      
      iPlot_stp1 <- input$individualPlot_stp1
      iPlot_stp2 <- input$individualPlot_stp2
      
      dates <- getDates(metadata.file)
      nums <- getNums(metadata.file)
      #get data from plotData here
      df <- plotData()
      
      if (is.null(df)) {
        message("plotData returned null!")
        return()
      } 
      
      names(df)[names(df) == 'GROUPS'] <- 'LINES'
      
      if (!is.null(iPlot_stp1)) {
        keep <- c(df[, myFacet, with=FALSE] == iPlot_stp1)
        df <- df[keep,]
      }
      if (!is.null(iPlot_stp2)) {
        keep2 <- c(df[, myFacet2, with=FALSE] == iPlot_stp2)
        df <- df[keep2,]
      }
      
      if (contLongitudinal) {
        #define axis labels here
        xAxisType <- metadata.file$type[metadata.file$source_id == longitudinal]
        if (xAxisType == "number") {
          xlab = "Age"
        } else {
          xlab = "Time"
        }
        
        yaxis_stp1 <- input$yaxis_stp1
        yaxis_stp2 <- input$yaxis_stp2
        if (prtcpntView$val != TRUE) {
          yaxis_stp2 <- input$yaxis_stp1
          yaxis_stp1 <- "any"
        }
        
        ylab <- makeGroupLabel(getMyY$val, metadata.file, yaxis_stp1, yaxis_stp2, NULL, NULL, NULL, useGroup = TRUE)[1]
        message(ylab)
        if (plotType == "proportion") {
          ylab <- paste("Proportion where", ylab)
        } else if (plotType == "count") {
          ylab <- paste("Count where", ylab)
        } else {
          ylab <- paste("Mean where", ylab)
          df$YAXIS <- as.numeric(df$YAXIS)
        }
        ylab <- gsub('(.{1,65})(\\s|$)', '\\1\n', ylab)
        message(ylab)
        
        #format xaxis ticks
        if (longitudinal %in% nums$source_id) {
          df$XAXIS <- as.numeric(gsub("\\[|\\]", "", sub(".*,", "", df$XAXIS)))
        } else {
          df$XAXIS <- as.factor(df$XAXIS)
          levels(df$XAXIS) <- sort(levels(df$XAXIS))
        }
        
        #plot here
        myPlot <- ggplot(data = df, aes(x = XAXIS, y = YAXIS, group = LINES,  color = LINES))
        myPlot <- myPlot + theme_bw()
        myPlot <- myPlot + labs(y = "", x = "")
        message(paste("plot type:", plotType))
        #add the lines
        if (plotType == "proportion" | plotType == "count") {
          myPlot <- myPlot + geom_point()
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
        maxChars <- max(nchar(as.vector(df$LINES)))
        
        #find num colors needed
        if (numColors > 2) { 
          myPlot <- myPlot + scale_color_manual(name = "", values = viridis(numColors))
        } else if (numColors == 2) {
          myPlot <- myPlot + scale_color_manual(name = "", values = viridis(numColors, begin = .25, end = .75))
        } else {
          myPlot <- myPlot + scale_color_manual(name = "", values = viridis(numColors, begin = .5))
        }
        
        if (!longitudinal %in% nums$source_id) {
          myPlot <- myPlot + theme(axis.text.x = element_text(angle = 45, hjust = 1))
        }
        
      } else {
        
        names(df)[names(df) == 'LINES'] <- 'XAXIS'
        # if y axis is numeric box plots otherwise bar pltos.
        #define axis labels here
        xlab <- ""
        yaxis_stp1 <- input$yaxis_stp1
        yaxis_stp2 <- input$yaxis_stp2
        if (prtcpntView$val != TRUE) {
          yaxis_stp2 <- input$yaxis_stp1
          yaxis_stp1 <- "any"
        }
        #test if numeric, if yes then "Mean" else proportion if vals between 0 and 1 otherwise "Count"
        ylab <- makeGroupLabel(getMyY$val, metadata.file, yaxis_stp1, yaxis_stp2, NULL, NULL, NULL, useGroup = TRUE)[1]
        if (plotType == "proportion") {
          ylab <- paste("Proportion where", ylab)
        } else if (plotType == "count") {
          ylab <- paste("Count where", ylab)
        } else {
          ylab <- paste("Mean where", ylab)
          df$YAXIS <- as.numeric(df$YAXIS)
        }
        ylab <- gsub('(.{1,45})(\\s|$)', '\\1\n', ylab)
        df$XAXIS <- as.factor(df$XAXIS) 
        #plot here
        myPlot <- ggplot(data = df, aes(x = XAXIS, y = YAXIS, fill = XAXIS))
        myPlot <- myPlot + theme_bw()
        myPlot <- myPlot + labs(y = "", x = "")
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
        maxChars <- max(nchar(as.vector(df$XAXIS)))

        #find num colors needed
        if (numColors > 2) { 
          myPlot <- myPlot + scale_fill_manual(name = "", values = viridis(numColors))
        } else if (numColors == 2) {
          
          myPlot <- myPlot + scale_fill_manual(name = "", values = viridis(numColors, begin = .25, end = .75))
        } else {
          
          myPlot <- myPlot + scale_fill_manual(name = "", values = viridis(numColors, begin = .5))
        }
        
      }
      
      #should keep playing with this vs doing it with ggplot syntax. 
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

      if (is.na(maxChars)) {
        legend_list <- list(x=100, y=.5)
      } else {
        if (maxChars > 35) {
          legend_list <- list(x = .5, y = -.8)
        } else {
          legend_list <- list(x=100, y=.5)
        }
      }
      
      myPlotly <- ggplotly(myPlot, tooltip = c("text", "x", "y"), width = (0.75*as.numeric(input$dimension[1])), height = as.numeric(input$dimension[2]))
      if (is.null(legendTitle)) {
        legend.title <- "All"
      } else {
        legend.title <- metadata.file$property[metadata.file$source_id == legendTitle]
        legend.title <- gsub('(.{1,15})(\\s|$)', '\\1\n', legend.title)
      }
      myPlotly <- add_annotations(myPlotly, text = legend.title, xref="paper",
                                  x=1.02, xanchor = "left",
                                  y=.3, yanchor = "bottom",
                                  legendtitle=TRUE, showarrow=FALSE)
      myPlotly <- plotly:::config(myPlotly, displaylogo = FALSE, collaborate = FALSE)
      myPlotly <- layout(myPlotly, margin = list(l = 70, r = 50, b = 150, t = 40),
                         xaxis = x_list, 
                         yaxis = y_list,
                         legend = legend_list,
                         autosize=TRUE)
      
      myPlotly
      
    })
    
    output$plot <- renderPlotly({
      if (is.null(input$yaxis_stp3)) {
        return()
      } else {
        plotType <- input$yaxis_stp3
      }
      longitudinal <- longitudinal1
      if (!is.null(input$xaxisVar)) {
        xaxisVar <- input$xaxisVar
        if (xaxisVar == "ageVar") {
          longitudinal <- longitudinal2
        }
      } 
      xaxis_bins <- input$xaxis_stp2
      if (input$facetType == "none") {
        myFacet <- "none"
      } else {
        myFacet <- "FACET"
      }
      if (input$facet2Type == "none") {
        myFacet2 <- "none"
      } else {
        myFacet2 <- "FACET2"
      }
      dummy <- getMyFacet$val
	dummy <- getMyFacet2$val   

      dates <- getDates(metadata.file)
      nums <- getNums(metadata.file)
        df <- plotData()
        if (is.null(df)) {
          message("plotData returned null!")
          return()
        } 
        
        names(df)[names(df) == 'GROUPS'] <- 'LINES'

        if (contLongitudinal) {
          xAxisType <- metadata.file$type[metadata.file$source_id == longitudinal]
          if (xAxisType == "number") {
            xlab = "Age"
          } else {
            xlab = "Time"
          }
         
          yaxis_stp1 <- input$yaxis_stp1
          yaxis_stp2 <- input$yaxis_stp2
          if (prtcpntView$val != TRUE) {
            yaxis_stp2 <- input$yaxis_stp1
            yaxis_stp1 <- "any"
          }
          
          ylab <- makeGroupLabel(getMyY$val, metadata.file, yaxis_stp1, yaxis_stp2, NULL, NULL, NULL, useGroup = TRUE)[1]
          if (plotType == "proportion") {
            ylab <- paste("Proportion where", ylab)
          } else if (plotType == "count") {
            ylab <- paste("Count where", ylab)
          } else {
            ylab <- paste("Mean where", ylab)
            df$YAXIS <- as.numeric(df$YAXIS)
          }

          ylab <- gsub('(.{1,65})(\\s|$)', '\\1\n', ylab)

          #format xaxis ticks
          if (longitudinal %in% nums$source_id) {
            df$XAXIS <- as.numeric(gsub("\\[|\\]", "", sub(".*,", "", df$XAXIS)))
          } else {
            df$XAXIS <- as.factor(df$XAXIS)
            levels(df$XAXIS) <- sort(levels(df$XAXIS))
          }

          #plot here
          myPlot <- ggplot(data = df, aes(x = XAXIS, y = YAXIS, group = LINES,  color = LINES))
          myPlot <- myPlot + theme_bw()
          myPlot <- myPlot + labs(y = "", x = "")

          #add the lines
          if (plotType == "proportion" | plotType == "count") {
            myPlot <- myPlot + geom_point()
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
          maxChars <- max(nchar(as.vector(df$LINES)))          

          #find num colors needed
          if (numColors > 2) { 
            myPlot <- myPlot + scale_color_manual(name = "", values = viridis(numColors))
          } else if (numColors == 2) {
            myPlot <- myPlot + scale_color_manual(name = "", values = viridis(numColors, begin = .25, end = .75))
          } else {
            myPlot <- myPlot + scale_color_manual(name = "", values = viridis(numColors, begin = .5))
          }

          if (!longitudinal %in% nums$source_id) {
            myPlot <- myPlot + theme(axis.text.x = element_text(angle = 45, hjust = 1))
          }

        } else {
         
          names(df)[names(df) == 'LINES'] <- 'XAXIS'
          # if y axis is numeric box plots otherwise bar pltos.
          #define axis labels here
          xlab <- ""
          yaxis_stp1 <- input$yaxis_stp1
          yaxis_stp2 <- input$yaxis_stp2
          if (prtcpntView$val != TRUE) {
            yaxis_stp2 <- input$yaxis_stp1
            yaxis_stp1 <- "any"
          }
          #test if numeric, if yes then "Mean" else proportion if vals between 0 and 1 otherwise "Count"
          ylab <- makeGroupLabel(getMyY$val, metadata.file, yaxis_stp1, yaxis_stp2, NULL, NULL, NULL, useGroup = TRUE)[1]
          if (plotType == "proportion") {
            ylab <- paste("Proportion where", ylab)
          } else if (plotType == "count") {
            ylab <- paste("Count where", ylab)
          } else {
            ylab <- paste("Mean where", ylab)
            df$YAXIS <- as.numeric(df$YAXIS)
          }
          ylab <- gsub('(.{1,45})(\\s|$)', '\\1\n', ylab)
          df$XAXIS <- as.factor(df$XAXIS) 
          #plot here
          myPlot <- ggplot(data = df, aes(x = XAXIS, y = YAXIS, fill = XAXIS))
          myPlot <- myPlot + theme_bw()
          myPlot <- myPlot + labs(y = "", x = "")
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
          maxChars <- max(nchar(as.vector(df$XAXIS)))

          #find num colors needed
          if (numColors > 2) { 
            myPlot <- myPlot + scale_fill_manual(name = "", values = viridis(numColors))
          } else if (numColors == 2) {
            
            myPlot <- myPlot + scale_fill_manual(name = "", values = viridis(numColors, begin = .25, end = .75))
          } else {
            
            myPlot <- myPlot + scale_fill_manual(name = "", values = viridis(numColors, begin = .5))
          }

        }
        if (myFacet != "none" | myFacet2 != "none") {
          if (myFacet == "none" & myFacet2 != "none") {
            myFacet <- myFacet2
            myFacet2 <- "none"
          }
         
          if (myFacet2 == "none") {
            myPlot <- myPlot + facet_wrap(reformulate(myFacet), ncol = 1)
          } else {
            myPlot <- myPlot + facet_grid(reformulate(myFacet, myFacet2))
          } 
        }       
 
        #should keep playing with this vs doing it with ggplot syntax. 
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
        if (is.na(maxChars)) {
          legend_list <- list(x=100, y=.5)
        } else {
          if (maxChars > 35) {
            legend_list <- list(x = .5, y = -.8)
          } else {
            legend_list <- list(x=100, y=.5)
          }
        }      
  
        myPlotly <- ggplotly(myPlot, tooltip = c("text", "x", "y"), width = (0.75*as.numeric(input$dimension[1])), height = as.numeric(input$dimension[2]))
        if (is.null(legendTitle)) {
          legend.title <- "All"
        } else {
          legend.title <- metadata.file$property[metadata.file$source_id == legendTitle]
          legend.title <- gsub('(.{1,15})(\\s|$)', '\\1\n', legend.title)
        }
        myPlotly <- add_annotations(myPlotly, text = legend.title, xref="paper",
                                    x=1.02, xanchor = "left",
                                    y=.3, yanchor = "bottom",
                                    legendtitle=TRUE, showarrow=FALSE)
        myPlotly <- plotly:::config(myPlotly, displaylogo = FALSE, collaborate = FALSE)
        myPlotly <- layout(myPlotly, margin = list(l = 70, r = 50, b = 150, t = 40),
                                     xaxis = x_list, 
                                     yaxis = y_list,
                                     legend = legend_list,
                                     autosize=TRUE)
        
        myPlotly
      
    })
    
    observeEvent(tableData(), {
      plotData <- tableData()
      if (is.null(plotData)) {
        return()
      }
      if (input$facetType == "none") {
        myFacet <- "none"
      } else {
        myFacet <- "FACET"
      }
      if (input$facet2Type == "none") {
        myFacet2 <- "none"
      } else {
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
          countFun <- function(x){ length(unique(x)) }
          colName <- "# Participants: "
        } else {
          countFun <- function(x){ length(x) }
          colName <- "# Observations: "
        }
        
        #and still need to remove duplicates across time.
        if (any(colnames(data) %in% "FACET")) {
          data <- reshape(aggregate(Participant_Id ~ FACET + GROUPS, data, FUN = countFun ), 
                          timevar = "FACET", idvar = "GROUPS", v.names = "Participant_Id", direction = "wide")
          colnames(data)[1] <- "Line"
          colnames(data) <- gsub("Participant_Id.", colName, colnames(data))
          #give totals
          if (length(data) > 2) {
            data[, "Totals"] <- rowSums(data[, -1], na.rm=TRUE)
          }
          rownames(data) <- data[,1]
          data[,1] <- NULL
          data["Totals" ,] <- colSums(data, na.rm=TRUE)
          data <- cbind("Line" = rownames(data), data)
        } else {
          data <- aggregate(Participant_Id ~ GROUPS, data, countFun )
          colnames(data) <- c("Line", colName)
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
        
        longitudinal <- longitudinal1
        if (!is.null(input$xaxisVar)) {
          if (input$xaxisVar == "ageVar") {
            longitudinal <- longitudinal2
          }
        }
        #temp placeholder for checking if data has time vars for x axis
        if (!contLongitudinal) {
          names(data)[names(data) == 'Line'] <- 'X-Axis'
        } 
        
        if (length(facets) > 0) {
          myCaption <- paste("Strata:",paste(facets, collapse = " and "))
        } else {
          myCaption <- ""
        }
        
        output[[id]] <- DT::renderDataTable(datatable(data,
                                                      caption = myCaption,
                                                      width = '100%',
                                                      rownames = FALSE
                                                      #options = list(
                                                      #  columnDefs = list(list(className = 'dt-right', targets = myTargets))
                                                      #)
        ))
      }
      
      output$table <- renderUI({
        lapply(1:dt_len, function(i) {
          id <- paste0("table", i)
          if (!is.null(dt_list)) {
            keep <- c(plotData[, myFacet2, with=FALSE] == c(dt_list[i]))
            facets <- c(dt_list[i])
            data <- plotData[keep,]
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
    tableData <- debounce(reactive({
      
      #collecting inputs 
      mySubset <- current$subset
      myTimeframe1 <- current$range1
      myTimeframe2 <- current$range2
      longitudinal <- longitudinal1
      if (!is.null(input$xaxisVar)) {
        if (input$xaxisVar == "ageVar") {
          longitudinal <- longitudinal2
        }
      }
      groupsType <- input$groupsType
      facetType <- input$facetType
      facet2Type <- input$facet2Type
      myFacet <- getMyFacet$val
      myFacet2 <- getMyFacet2$val
      myY <- getMyY$val
      yaxis_stp1 <- input$yaxis_stp1
      yaxis_stp2 <- input$yaxis_stp2
      if (prtcpntView$val != TRUE) {
        yaxis_stp2 <- input$yaxis_stp1
        yaxis_stp1 <- "any"
      }
      yaxis_stp3 <- input$yaxis_stp3
      myGroups <- getMyGroups$val 
      #grab optional inputs
      groups_stp1 <- groupInfo$group_stp1
      groups_stp3 <- groupInfo$group_stp3
      groups_stp4 <- groupInfo$group_stp4
      groups_stp2 <- groupInfo$group_stp2
      facet_stp1 <- facetInfo$group_stp1
      facet_stp3 <- facetInfo$group_stp3
      facet_stp4 <- facetInfo$group_stp4
      facet_stp2 <- facetInfo$group_stp2
      facet2_stp1 <- facet2Info$group_stp1
      facet2_stp3 <- facet2Info$group_stp3
      facet2_stp4 <- facet2Info$group_stp4
      facet2_stp2 <- facet2Info$group_stp2
      yaxis_stp1 <- input$yaxis_stp1
      message("have all inputs for plotData")
      #subset data
      if (!is.null(longitudinal1)) {
        #should never have both subset and timeframes..
        if (!is.null(mySubset)) {
          data <- subsetDataFetcher(keep = mySubset, myData = singleVarData, col = longitudinal1)
          message("subsetting data by non-continuous longitudinal variable..")
          if (nrow(data) == 0) {
            message("subset failed, returning")
            return()
          }
        } else {
          data <- singleVarData
        }     
        if (!is.null(myTimeframe1)) {
          data <- subsetDataFetcher(min = myTimeframe1[1], max = myTimeframe1[2], myData = data, col = longitudinal1)
          message("subsetting data by first longitudinal variable..")
          if (nrow(data) == 0) {
            message("subset failed, returning")
            return()
          }
        }
        if (!is.null(longitudinal2)) {
          if (!is.null(myTimeframe2)) {
            data <- subsetDataFetcher(min = myTimeframe2[1], max = myTimeframe2[2], myData = data, col = longitudinal2)
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
  
      strings <- subset(metadata.file, metadata.file$type == "string", "source_id")
      go <- TRUE
      
      if (is.null(groupsType)) {
        go <- FALSE
      } else {
        if (groupsType == "makeGroups") {
          if (is.null(groups_stp1)) {
            go <- FALSE
          } else {
            if (groups_stp1 == 'any' | groups_stp1 == 'all') {
              if (is.null(groups_stp2)) {
                return()
              } else {
                if (groups_stp2 %in% c("lessThan", "greaterThan", "equals")) {
                  if (is.null(groups_stp3)) {
                    return()
                  }
                }
              }
            }
          }         
        } else if (groupsType == "direct") {
          if (is.null(myGroups)) {
            return()
          }
        } 
      }
      if (is.null(facetType)) {
        go <- FALSE
      } else {
        if (facetType == "makeGroups") {
          if (is.null(facet_stp1)) {
            go <- FALSE
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
        go <- FALSE
      } else {
        if (facet2Type == "makeGroups") {
          if (is.null(facet2_stp1)) {
            go <- FALSE
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
      if (is.null(myY)) {
        go <- FALSE
      } else {
        print(yaxis_stp1)
        if (myY %in% strings$source_id) {
          if (is.null(yaxis_stp2)) {
            go <- FALSE
          }
        }
      }
      if (is.null(yaxis_stp3)) {
        go <- FALSE
      }
      
      #once last field is populated .. GO
      if (go) {
        message("GO!!")
        #may not need to do the splitting on pipes. grepl will still return true for it.
        #should have natozero before this for non-anthro events?? so an NA for diarrhea -> 0 ?? 

        #first thing is to save properties 
        longitudinalText <- longitudinalText(mySubset, myTimeframe1, myTimeframe2)
        facetText <- groupText("facetInfo", myFacet, facet_stp1, facet_stp2, facet_stp3, facet_stp4)
        facet2Text <- groupText("facet2Info", myFacet2, facet2_stp1, facet2_stp2, facet2_stp3, facet2_stp4)
        groupsText <- groupText("groupInfo", myGroups, groups_stp1, groups_stp2, groups_stp3, groups_stp4)

        #this needs revision since stp2 could have multiple values
        if (length(yaxis_stp2) > 1) {
          yaxisStp2Text <- ""
          for (i in seq(length(yaxis_stp2))) {
            yaxisStp2Text <- paste0(yaxisStp2Text,
            "input$yaxis_stp2\t", yaxis_stp2[i], "\n")
          }
        } else {
          yaxisStp2Text <- paste0("input$yaxis_stp2\t", yaxis_stp2, "\n")
        }
 
        text <- paste0("input\tselected\n",
                       longitudinalText,
                       facetText,
                       facet2Text,
                       groupsText,
                       "input$xaxisVar\t", input$xaxisVar, "\n",
                       "input$xaxis_stp2\t", input$xaxis_stp2, "\n", 
                       "input$groupsType\t", groupsType, "\n",
                       "input$facetType\t", facetType, "\n",
                       "input$facet2Type\t", facet2Type, "\n",  
                       "input$yaxis\t", myY, "\n",
                       "input$yaxis_stp1\t", yaxis_stp1, "\n",
                       yaxisStp2Text,
                       "input$yaxis_stp3\t", yaxis_stp3
                      # "input$individualPlot_stp1\t", input$individualPlot_stp1, "\n",
                      # "input$individualPlot_stp2\t", input$individualPlot_stp2
                   )

        PUT(propUrl, body = "")
        PUT(propUrl, body = text)
        message("saving properties")
        plotData <- completeDT(data, myY)
        plotData <- getFinalDT(plotData, metadata.file, myY)
        
        aggKey <- aggKey()
        
        if (contLongitudinal) {
          myCols <- c(aggKey, myY, longitudinal)
          tempData <- plotData[, myCols, with=FALSE] 
          colnames(tempData) <- c(aggKey, "YAXIS", "XAXIS")
        } else {
          myCols <- c(aggKey, myY)
          tempData <- plotData[, myCols, with=FALSE] 
          tempData <- unique(tempData)
          colnames(tempData) <- c(aggKey, "YAXIS")
        }
        
        if (groupsType == "direct") {
          message("groups is direct")
          myCols <- c(aggKey, myGroups)
          groupData <- plotData[, myCols, with=FALSE]
          groupData <- unique(groupData)
          colnames(groupData) <- c(aggKey, "GROUPS")
          tempData <- merge(tempData, groupData, by = aggKey)
        } 
        
        if (facetType == "direct") {
          message("facet is direct")
          myCols <- c(aggKey, myFacet)
          facetData <- plotData[, myCols, with=FALSE]
          facetData <- unique(facetData)
          colnames(facetData) <- c(aggKey, "FACET")
          tempData <- merge(tempData, facetData, by = aggKey)
        } 
        
        if (facet2Type == "direct") {
          message("facet2 is direct")
          myCols <- c(aggKey, myFacet2)
          facet2Data <- plotData[, myCols, with=FALSE]
          facet2Data <- unique(facet2Data)
          colnames(facet2Data) <- c(aggKey, "FACET2")
          tempData <- merge(tempData, facet2Data, by = aggKey)
        } 
        
        plotData <- tempData

        xaxis_bins <- input$xaxis_stp2
        if (contLongitudinal) {
          message(head(plotData$XAXIS))
          message(typeof(plotData$XAXIS))
          message(class(plotData$XAXIS))
          message(is.factor(plotData$XAXIS)) 
          binnedXaxis  <- cut(plotData$XAXIS, xaxis_bins)
          plotData$XAXIS <- NULL
          plotData$XAXIS <- binnedXaxis 
          message("binning xaxis data")
        }
        
        #bin facet if numeric and same for group 
        nums <- getNums(metadata.file)
        dates <- getDates(metadata.file)
        if (any(colnames(plotData) %in% "FACET")) {
          displayLabel <- metadata.file$property[metadata.file$source_id == myFacet]
          if (myFacet %in% nums$source_id | myFacet %in% dates$source_id) {
            message("bin facet cause its numeric")
            if (length(levels(as.factor(plotData$FACET))) >= 4) {
              plotData$FACET <- rcut_number(plotData$FACET, 3)
            } else {
              plotData$FACET <- as.factor(plotData$FACET)
            }
          }
          plotData$FACET <- paste0(displayLabel, ": ", plotData$FACET)
        } else {
          numeric <- c("lessThan", "greaterThan", "equals")
          anthro <- c("percentDays", "delta", "direct")
          if (facetType != "none") {
            if (is.null(facet_stp1)) {
              message("facet stp1 is null.. returning")
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
            outData <- makeGroups(data, metadata.file, myFacet, facet_stp1, facet_stp2, facet_stp3, facet_stp4, aggKey)
            observations <- metadata.file$source_id[metadata.file$category == "Observation"]
            observations <- observations[observations %in% colnames(singleVarData)]
            label <- makeGroupLabel(myFacet, metadata.file, facet_stp1, facet_stp2, facet_stp3, facet_stp4, event.list = observations, useGroup=TRUE)
            message("have custom facet! now merge..")
            #add makeGroups data to df and return
            colnames(outData) <- c(aggKey, "FACET")
            #will need a var called label that changes based on what the facet steps are. the below only works for strings.
            #if (any(colnames(event.file) %in% myFacet)) {
             # naToZero(outData, "FACET")
            #}
            outData <- transform(outData, "FACET" = ifelse(as.numeric(FACET) == 0, label[2], label[1]))
           # outData$FACET <- factor(outData$FACET, levels(c("Other", facet_stp2)))
            plotData <- merge(plotData, outData, by = aggKey, all = TRUE)
          }
        }
        
        if (any(colnames(plotData) %in% "FACET2")) {
          displayLabel <- metadata.file$property[metadata.file$source_id == myFacet2]
          if (myFacet2 %in% nums$source_id | myFacet2 %in% dates$source_id) {
            message("bin facet2 cause its numeric")
            if (length(levels(as.factor(plotData$FACET2))) >= 4) {
              plotData$FACET2 <- rcut_number(plotData$FACET2, 3)
            } else {
              plotData$FACET2 <- as.factor(plotData$FACET2)
            }
          }
          plotData$FACET2 <- paste0(displayLabel, ": ", plotData$FACET2)
        } else {
          numeric <- c("lessThan", "greaterThan", "equals")
          anthro <- c("percentDays", "delta", "direct")
          if (facet2Type != "none") {
            if (is.null(facet2_stp1)) {
              message("facet2 stp1 is null.. returning")
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
            outData <- makeGroups(data, metadata.file, myFacet2, facet2_stp1, facet2_stp2, facet2_stp3, facet2_stp4, aggKey)
            observations <- metadata.file$source_id[metadata.file$category == "Observation"]
            observations <- observations[observations %in% colnames(singleVarData)]
            label <- makeGroupLabel(myFacet2, metadata.file, facet2_stp1, facet2_stp2, facet2_stp3, facet2_stp4, event.list = observations, useGroup=TRUE)
            message("have custom facet2! now merge..")
            #add makeGroups data to df and return
            colnames(outData) <- c(aggKey, "FACET2")
            #will need a var called label that changes based on what the facet steps are. the below only works for strings.
            #if (any(colnames(event.file) %in% myFacet)) {
            # naToZero(outData, "FACET")
            #}
            outData <- transform(outData, "FACET2" = ifelse(as.numeric(FACET2) == 0, label[2], label[1]))
            # outData$FACET <- factor(outData$FACET, levels(c("Other", facet_stp2)))
            plotData <- merge(plotData, outData, by = aggKey, all = TRUE)
          }
        }
        
        #if groups col exists return
        if (any(colnames(plotData) %in% "GROUPS")) {
          if (myGroups %in% nums$source_id | myGroups %in% dates$source_id) {
            if (length(levels(as.factor(plotData$GROUPS))) >= 4) {
              message("need to bin dates")
              plotData$GROUPS <- rcut_number(plotData$GROUPS)
            } else {
              plotData$GROUPS <- as.factor(plotData$GROUPS)
            } 
          }
        } else {
          numeric <- c("lessThan", "greaterThan", "equals")
          anthro <- c("percentDays", "delta", "direct")
          if (groupsType != "none") {
            if (is.null(groups_stp1)) {
              message("groups stp1 is null... returning")
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
            outData <- makeGroups(data, metadata.file, myGroups, groups_stp1, groups_stp2, groups_stp3, groups_stp4, aggKey)
            observations <- metadata.file$source_id[metadata.file$category == "Observation"]
            observations <- observations[observations %in% colnames(singleVarData)]
            label <- makeGroupLabel(myGroups, metadata.file, groups_stp1, groups_stp2, groups_stp3, groups_stp4, event.list = observations)
            #if (any(colnames(event.file) %in% myGroups)) {
            #  naToZero(plotData, "GROUPS")
            #}
            message("have custom groups! now merge..")
            #add makeGroups data to df and return
            outData <- transform(outData, "GROUPS" = ifelse(as.numeric(GROUPS) == 0, label[2], label[1]))
            plotData <- merge(plotData, outData, by = aggKey, all = TRUE)
            print("NA in groups:", any(is.na(plotData$GROUPS)))
          } else {
            if (prtcpntView$val == TRUE) {
              plotData <- cbind(plotData, "GROUPS" = "All Participants")
            } else {
              plotData <- cbind(plotData, "GROUPS" = "All Observations")
            }
          }
        }
        plotData <- unique(plotData)
        plotData
      }
      
      #debounce will wait 2s with no changes to inputs before plotting.
    }), 2000)
      
    plotData <- reactive({  
        plotData <- tableData()
        if (is.null(plotData)) {
          return()
        } else {
          #collecting inputs .. i think these are the only ones i need here.. well see
          myY <- getMyY$val
          if (is.null(input$yaxis_stp3)) {
            return()
          } else {
            yaxis_stp3 <- input$yaxis_stp3
          }
          yaxis_stp1 <- input$yaxis_stp1
          yaxis_stp2 <- input$yaxis_stp2
          if (prtcpntView$val != TRUE) {
            yaxis_stp2 <- input$yaxis_stp1
            yaxis_stp1 <- "any"
          }
          
          strings <- getStrings(metadata.file)
          aggKey <- aggKey()
        #prepare for return
        
        #determine necessary column id vectors before start
        if (any(colnames(plotData) %in% "FACET") | any(colnames(plotData) %in% "FACET2")) {
          if (any(colnames(plotData) %in% "FACET")) {
            if (any(colnames(plotData) %in% "FACET2")) {
              facetCols <- c("FACET", "FACET2")
            } else {
              facetCols <- c("FACET")
            }
          } else {
            if (any(colnames(plotData) %in% "FACET2")) {
              facetCols <- c("FACET2")
            } 
          }
          facetStr <- paste(facetCols, collapse = " + ")
          aggStr2 <- paste("Participant_Id ~ GROUPS + ", facetStr)
          sumCols <- c("GROUPS", facetCols, "SUM")
          mergeBy <- c("GROUPS", facetCols)
          dropCols <- c(aggKey, "YAXIS")
          if (any(colnames(plotData) %in% "XAXIS")) {
            aggStr1 <- paste("YAXIS ~ GROUPS + XAXIS + ", facetStr)
            mergeBy2 <- c("GROUPS", "XAXIS", facetCols)
          } else {
            aggStr1 <- paste("YAXIS ~ GROUPS + ", facetStr)
            mergeBy2 <- c("GROUPS", facetCols)
          }
        } else {
          aggStr2 <- "Participant_Id ~ GROUPS"
          sumCols <- c("GROUPS", "SUM")
          mergeBy <- c("GROUPS")
          dropCols <- c(aggKey, "YAXIS")
          if (any(colnames(plotData) %in% "XAXIS")) {
            aggStr1 <- "YAXIS ~ GROUPS + XAXIS"
            mergeBy2 <- c("GROUPS", "XAXIS")  
          } else {
            aggStr1 <- "YAXIS ~ GROUPS"
            mergeBy2 <- c("GROUPS")
          }
        }
          
        myPrtcpntView <- prtcpntView$val
        if (myPrtcpntView == TRUE) {
          aggStr3 <- aggStr1
          aggStr1 <- paste0(aggStr1, " + Participant_Id")
          countFun <- function(x) {length(unique(x))}
        } else {
          aggStr3 <- aggStr1
          aggStr1 <- paste0(aggStr1, " + " , paste(aggKey, collapse = " + "))
          countFun <- function(x) {length(x)}
        }
        
        if (myY %in% strings$source_id) {
          mergeData <- NULL
          if (yaxis_stp1 == "any" | prtcpntView$val == FALSE) {
            #will have to replace all instances of myY with 1 and all else with 0 before can sum
            for (i in seq(length(yaxis_stp2))) {
              tempData <- transform(plotData, "YAXIS" = ifelse(YAXIS == yaxis_stp2[i], 1, 0))
              #the following to get proportions of prtcpnts with matching observatio rather than proportion of matching observations.
              tempData <- aggregate(as.formula(aggStr1), tempData, sum)
              tempData <- transform(tempData, "YAXIS"=ifelse(YAXIS >= 1, 1, 0))
              #tempData <- aggregate(as.formula(paste0(aggStr1, " + Participant_Id")), plotData, FUN = function(x){ if(yaxis_stp2[[i]] %in% x) {1} else {0} })
              if (is.null(mergeData)) {
                mergeData <- tempData
              } else {
                names(tempData)[names(tempData) == "YAXIS"] <- "prevY"
                print(colnames(tempData))
                cols <- c(aggKey, "prevY")
                tempData <- tempData[, cols]
                print(unique(tempData$prevY))
                print(unique(mergeData$YAXIS))
                mergeData <- merge(mergeData, tempData, by = aggKey)
                print(unique(mergeData$YAXIS))
                mergeData <- transform(mergeData, "YAXIS" = ifelse(prevY == 1 | YAXIS == 1, 1, 0))
                print(unique(mergeData$YAXIS))
                mergeData$prevY <- NULL
                #the following to get proportions of prtcpnts with matching observatio rather than proportion of matching observations.
                mergeData <- aggregate(as.formula(aggStr1), mergeData, sum)
                mergeData <- transform(mergeData, "YAXIS"=ifelse(YAXIS >= 1, 1, 0)) 
              }
            }
          } else {
              mergeData <- aggregate(as.formula(aggStr1), plotData, FUN = function(x){ ifelse(length(levels(as.factor(x))) == length(yaxis_stp2), all(sort(levels(as.factor(x))) == sort(yaxis_stp2)), FALSE) })
              mergeData <- transform(mergeData, "YAXIS" = ifelse(YAXIS == TRUE, 1, 0))
          }
          mergeData <- aggregate(as.formula(aggStr3), mergeData, sum)
          print(head(mergeData))
          if (yaxis_stp3 == "proportion") {
            print(unique(mergeData$YAXIS))
            groupSum <- as.data.table(aggregate(as.formula(aggStr2), plotData, FUN = countFun))
            print(unique(mergeData$YAXIS))
            colnames(groupSum) <- sumCols
            mergeData <- as.data.table(mergeData)
            mergeData <- merge(mergeData, groupSum, by = mergeBy)
            proportion <- mergeData$YAXIS / mergeData$SUM
            mergeData$YAXIS <- proportion
            mergeData$SUM <- NULL
          }
        }
        if (yaxis_stp3 == "smooth" | yaxis_stp3 == "mean") {
          plotData$Participant_Id <- NULL
          plotData <- unique(plotData)
        } else {
          plotData <- plotData[, -dropCols, with=FALSE]
          plotData <- unique(plotData)
          print(unique(mergeData$YAXIS))
          print(mergeBy2)
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
