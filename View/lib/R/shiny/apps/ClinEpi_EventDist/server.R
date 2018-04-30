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
  longitudinal.file <- NULL
  prevFacet <- NULL
  current <- NULL
  facetInfo <- NULL
  xaxisInfo <- NULL
  attributes.file <- NULL  
  propUrl <- NULL
  properties <- NULL
  plotChoice <- 'singleVar'
  longitudinal1 <- NULL
  longitudinal2 <- NULL
  project.id <- NULL
  isParticipant <- NULL
  model.prop <- NULL

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
        na.strings = c("N/A", "na", "")))
      metadata_temp <- try(fread(
        getWdkDatasetFile('ontologyMetadata.tab', session, FALSE, dataStorageDir),
        na.strings = c("N/A", "na", "")))
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

        #check for unique display names in metdata file.
        if (length(unique(metadata.file$property)) != nrow(metadata.file)) {
          true <- duplicated(metadata.file$property) | duplicated(metadata.file$property, fromLast = TRUE)
          metadata.file <<- transform(metadata.file, "property" = ifelse(true, paste0(property, ", ", parent), property))
        }

        #add user defined group
        #metadata.file <<- rbind(metadata.file, list("search_weight", "Strategy Step 1", "string", "none"))
        if ('Participant_Id' %in% colnames(attributes.file)) {
          isParticipant <<- TRUE
          metadata.file <<- rbind(metadata.file, list("custom", "Participant Search Results", "string", "none"))
          metadata.file <<- rbind(metadata.file, list("Avg_Female_Anopheles", "Avg Female Anopheles from Search Results", "number", "none"))
          metadata.file <<- rbind(metadata.file, list("Matching_Observations_/_Year", "Matching Observations / Year from Search Results", "number", "none"))
          metadata.file <<- rbind(metadata.file, list("Years_of_Observation", "Years of Observations from Search Results", "number", "none"))
        } else {
          isParticipant <<- FALSE
          metadata.file <<- rbind(metadata.file, list("custom", "Observation Search Results", "string", "none"))
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

    prtcpnt_temp <- try(fread(paste0(mirror.dir,"shiny_participants.txt"), na.strings = c("N/A", "na", "")))
    house_temp <- try(fread(paste0(mirror.dir, "shiny_households.txt"), na.strings = c("N/A", "na", "")))
    event_temp <- try(fread(paste0(mirror.dir, "shiny_obsevations.txt"), na.strings = c("N/A", "na", "")))

    longitudinal.file <<- fread("../../lib/longitudinal.tab")
    longitudinal.file <<- longitudinal.file[longitudinal.file$dataset_name == datasetName]
    longitudinal.file <<- setDT(longitudinal.file)[, lapply(.SD, function(x) unlist(tstrsplit(x, "|", fixed=TRUE))),
                        by = setdiff(names(longitudinal.file), "columns")][!is.na(longitudinal.file$columns)]
 
    if (grepl("Error", prtcpnt_temp[1])){
      stop("Error: Participant file missing or unreadable!")
    } else {
      prtcpnt.file <<- prtcpnt_temp
      names(prtcpnt.file) <<-  gsub(" ", "_", gsub("\\[|\\]", "", names(prtcpnt.file)))
      names(prtcpnt.file)[names(prtcpnt.file) == 'SOURCE_ID'] <<- 'Participant_Id'
      setkey(prtcpnt.file, Participant_Id)

      if ('Participant_Id' %in% colnames(attributes.file)) {
        prtcpnt.file <<- merge(prtcpnt.file, attributes.file, by = "Participant_Id", all = TRUE)
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
      if ('Observation_Id' %in% colnames(attributes.file)) {
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
  
  output$title <- renderUI({
    withProgress(message = 'Loading...', value = 0, style = "old", {
      singleVarDataFetcher()
      incProgress(.45)
      current <<- callModule(timeline, "timeline", singleVarData, longitudinal.file, metadata.file)
      incProgress(.15)
      xaxisInfo <<- callModule(customGroups, "group", groupLabel = groupLabel, metadata.file = metadata.file, useData = groupData, singleVarData = singleVarData, event.file = event.file, selected = selectedGroup, groupsType = reactive(input$xaxis), groupsTypeID = "input$xaxis", moduleName = "xaxisInfo")
      incProgress(.25)
      facetInfo <<- callModule(customGroups, "facet", groupLabel = facetLabel, metadata.file = metadata.file, useData = facetData, singleVarData = singleVarData, event.file = event.file, selected = selectedFacet, groupsType = reactive(input$facetType), groupsTypeID = "input$facetType", moduleName = "facetInfo")
      incProgress(.15)
    })
    titlePanel("Data Distributions")
  })
  
    output$choose_groups <- renderUI({
      if (is.null(input$xaxis)) {
        return()
      }
      myX <- input$xaxis
      
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
      
      if (plotChoice == 'groups') {
        #getXList(plotChoice)
        selectInput(inputId = "xaxis",
                    label = "X-Axis:",
                    choices = list('Z-score' = 'zscore', 'Age in Days' = 'ageDays'),
                    selected = 'zscore') 
      } else {
        mySelected <- properties$selected[properties$input == "input$xaxis"]
        
        if (is.null(properties)) {
          selectInput(inputId = "xaxis",
                      label = "X-Axis:",
                      choices = c("All possible" = "direct"),
                      selected = "direct",
                      width = '100%')
        } else {
          selectInput(inputId = "xaxis",
                      label = "X-Axis:",
                      choices = c("All possible" = "direct"),
                      selected = mySelected,
                      width = '100%')
        }
      }
    })
    
    output$facet_type <- renderUI({
      mySelected <- properties$selected[properties$input == "input$facetType"]

      if (is.null(properties)) {
        if (isParticipant) {
          selectInput(inputId = "facetType",
                      label = "Facet:",
                      choices = c("All possible" = "direct", "Make my own" = "makeGroups", "None" = "none"),
                      selected = "direct",
                      width = '100%')
        } else {
          selectInput(inputId = "facetType",
                      label = "Facet:",
                      choices = c("All possible" = "direct", "Make my own" = "makeGroups", "None" = "none"),
                      selected = "makeGroups",
                      width = '100%')
        }
      } else {
        selectInput(inputId = "facetType",
                    label = "Facet:",
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
            
      return(useData)
    })
    
    selectedGroup <- reactive({
      if (is.null(input$xaxis)) {
        return()
      } else {
        groupsType <- input$xaxis
      }
      
      if (groupsType == "direct") {
        selected <- "EUPATH_0000338"
      } else {
        selected <- "EUPATH_0000338"
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
          selected <- "EUPATH_0000054"
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
        ptmp <- prtcpnt.file[, !dates, with = FALSE]
        if (house.file.exists) {
          htmp <- house.file[, !dates, with = FALSE]
          useData <- list(ptmp, htmp)
        } else {
          useData <- list(ptmp)
        }
      } else {
        useData <- list(singleVarData)
      }     
 
      return(useData)
    })
    
    output$distribution <- renderPlotly({
      message("render plot!")
      if (is.null(input$xaxis)) {
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
      if (input$facetType == "none") {
        myFacet <- "none"
      } else {
        myFacet <- facetInfo$group
      }
      facetType <- input$facetType
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
        if ((myX %in% nums$source_id | myX %in% dates$source_id) & myX != myFacet) {
          myPlot <- myPlot + geom_histogram(aes(text = paste0("Count: ", ..count..)), stat = "bin", fill = viridis(1, end = .25, direction = -1))
          myPlot <- myPlot + geom_vline(aes(xintercept = mean(df[[myX]], na.rm = T), text = paste0("mean:", mean(df[[myX]], na.rm = T))), color = viridis(1, begin = .75), linetype = "dashed", size = 1)
          if (facetType == 'direct') {
            myPlot <- myPlot + facet_wrap(reformulate(myFacet), ncol = 1) 
            # scale_fill_brewer(palette = cbPalette)
          } else if (facetType == 'makeGroups') {
            myPlot <- myPlot + facet_wrap(~ FACET, ncol = 1)
          }
        } else {
          myPlot <- myPlot + geom_histogram(aes(text = paste0("Count: ", ..count..)), stat = "count", fill = viridis(1, end = .25, direction = -1))
          myPlot <- myPlot + theme(axis.text.x = element_text(angle = 90, hjust = 1))
          if (facetType == 'direct') {
            myPlot <- myPlot + facet_wrap(reformulate(myFacet), ncol = 1) 
            # scale_fill_brewer(palette = cbPalette)
          } else if (facetType == 'makeGroups') {
            myPlot <- myPlot + facet_wrap(~ FACET, ncol = 1)
          }
          myPlot <- myPlot + theme(axis.text.x = element_text(angle = 45, hjust = 1))
        }
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

    output$table <- DT::renderDataTable({
      data <- plotData()
      if (is.null(data)) {
        return()
      }
      if (input$facetType == "none") {
        myFacet <- "none"
      } else {
        myFacet <- facetInfo$group
      }
      myX <- xaxisInfo$group  
      if ("FACET" %in% colnames(data)) {
        myFacet <- "FACET"
      }
 
      nums <- getNums(metadata.file)$source_id
 
      if (myFacet == "none") {
        tableData <- data.table("Facet" = "All", "# Participants" = length(unique(data$Participant_Id)))
        if (myX %in% nums) {
          tableData <- cbind(tableData, "Mean" = round(mean(data[[myX]], na.rm = TRUE),4))
          tableData <- cbind(tableData, "Median" = median(data[[myX]], na.rm = TRUE))
          tableData <- cbind(tableData, "Range" = paste(min(data[[myX]], na.rm = TRUE), "-", max(data[[myX]], na.rm = TRUE)))
          tableData <- cbind(tableData, "SD" = round(sd(data[[myX]], na.rm = TRUE),4))
          tableData <- cbind(tableData, "IQR" = round(IQR(data[[myX]], na.rm = TRUE),4))
        }
      } else {
        aggStr <- paste0("Participant_Id ~ ", myFacet)
        aggStr2 <- paste0(myX, " ~", myFacet)
        #will need to change first arg based on all possible or makeGroups
        tableData <- aggregate(as.formula(aggStr), data, FUN = function(x){length(unique(x))})
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
          colnames(tableData) <- c("Facets", "# Participants", "Mean", "Median", "Range", "SD", "IQR")
        } else {
          colnames(tableData) <- c("Facets", "# Participants")
        }
      }

      if (length(colnames(tableData)) == 7) {
        myTargets <- c(1,2,3,4,5,6)
      } else {
        myTargets <- c(1)
      }

      datatable(tableData,
                width = '100%',
                rownames = FALSE,
                options = list(
                  columnDefs = list(list(className = 'dt-right', targets = myTargets))
                )
      )
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
      xType <- input$xaxis
      myX <- input$xaxis
      if (myX == "direct" | myX == "makeGroups") {
        if (is.null(xaxisInfo$group)) {
          return()
        } else {
          myX <- xaxisInfo$group
        }
      }
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

      #first thing is to save properties 
      longitudinalText <- longitudinalText(myTimeframe1, myTimeframe2)
      facetText <- groupText("facetInfo", myFacet, facet_stp1, facet_stp2, facet_stp3, facet_stp4)

      text <- paste0("input\tselected\n",
                     longitudinalText,
                     facetText,
                     "xaxisInfo$group\t", myX, "\n",
                     "input$facetType\t", facetType, "\n",
                     "input$xaxis\t", xType
                    )

      PUT(propUrl, body = "")
      PUT(propUrl, body = text)

      strings <- subset(metadata.file, metadata.file$type == "string", "source_id")
      if (plotChoice == 'groups') {
        data <- groupsDataFetcher(myGroups, myX)
        #subset data
        #which cols can be used for this will have to change. too specific right now
        #maybe change prev longitudinal var to longitudinal.file so can check if one of these if statements needs to be met
        if (!is.null(longitudinal1)) {
          if (!is.null(myTimeframe1)) {
            data <- subsetDataFetcher(myTimeframe1[1], myTimeframe1[2], data, longitudinal1)
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
        col = 'groups'
        #data <- setDT(data)[, lapply(.SD, function(x) unlist(tstrsplit(x, " | ", fixed=TRUE))), 
        #                    by = setdiff(names(data), eval(col))][!is.na(eval(col))]
        if (any(grepl("|", data[[col]], fixed=TRUE))) {
          data <- separate_rows(data, col, sep = "[|]+")
        }
      } else {
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
      }
     
      nums <- getNums(metadata.file)
      dates <- getDates(metadata.file)      

      #for handling facets, this works for direct. need if statement
      if (facetType == "direct") {
        if (myFacet %in% nums$source_id | myFacet %in% dates$source_id) {
          data[[myFacet]] <- cut(data[[myFacet]],4)
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
          outData <- makeGroups(data, metadata.file, myFacet, facet_stp1, facet_stp2, facet_stp3, facet_stp4)
          label <- makeGroupLabel(myFacet, metadata.file, facet_stp1, facet_stp2, facet_stp3, facet_stp4, event.list = colnames(event.file), useGroup = TRUE)
          #add makeGroups data to df and return
          colnames(outData) <- c("Participant_Id", "FACET")
          outData <- transform(outData, "FACET" = ifelse(as.numeric(FACET) == 0, label[2], label[1]))
          data <- merge(data, outData, by = "Participant_Id", all = TRUE)
        }
      }
    
      if (facetType == "makeGroups") {
        myCols <- c("Participant_Id", myX, "FACET") 
      } else {
        if (myX == myFacet | myFacet == "none") {
          myCols <- c("Participant_Id", myX)
        } else {
          myCols <- c("Participant_Id", myX, myFacet)
        }
      }
      data <- data[, myCols, with = FALSE]
      data <- unique(data)

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
