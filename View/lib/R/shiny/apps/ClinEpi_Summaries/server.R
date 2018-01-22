## server.r

#need to look at dates options at min yaxis and facet line dont seem to work
#for facets ui: consider selecting more than one group/ check boxes ??
#make sure levels for numeric groups always start with the group that has square bracket in front (meaning smallest -> largest)
#figure out when we need naToZero function when building own groups and facets. imagine need it for events stuffs but not others ??
# for some reason delta laz > -.5 returns fewer ppl than >-1.. seems should be reversed

shinyServer(function(input, output, session) {
  
  event.file <- NULL
  event.file.exists <- NULL
  prtcpnt.file <- NULL
  house.file <- NULL
  house.file.exists <- NULL
  metadata.file <- NULL
  singleVarData <- NULL
  longitudinal.file <- NULL
  current <- NULL
  facetInfo <- NULL
  groupInfo <- NULL
  attribute.file <- NULL
  propUrl <- NULL
  properties <- NULL  

  filesFetcher <- reactive({

    if (is.null(propUrl)) {
      propUrl <<- getPropertiesUrl(session)
      properties <<- try(fread(propUrl))

      if (grepl("Error", properties)) {
        properties <<- NULL
      }
    }
    message(paste("propUrl:", propUrl))

    if (is.null(attribute.file)) {

      attribute_temp <- try(fread(
        getWdkDatasetFile('attributes.tab', session, FALSE, dataStorageDir),
        na.strings = c("N/A", "na", "")))
      metadata_temp <- try(fread(
          getWdkDatasetFile('ontologyMetadata.tab', session, FALSE, dataStorageDir),
          ))
 
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
          metadata.file <<- rbind(metadata.file, list("custom", "Participant Search Results", "string", "none"))
          metadata.file <<- rbind(metadata.file, list("Avg_Female_Anopheles", "Avg Female Anopheles from Search Results", "number", "none"))
          metadata.file <<- rbind(metadata.file, list("Matching_Observations_/_Year", "Matching Observations / Year from Search Results", "number", "none"))
          metadata.file <<- rbind(metadata.file, list("Years_of_Observation", "Years of Observations from Search Results", "number", "none"))
        } else {
          metadata.file <<- rbind(metadata.file, list("custom", "Observation Search Results", "string", "none"))
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

      if (colnames(attributes.file)[1] == 'Participant_Id') {
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
      setkey(event.file, Participant_Id)

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
    prtcpnt.file <<- prtcpnt.file[, !drop, with = FALSE]
    
    if (exists("event.file")) {
      if (!is.null(event.file) & nrow(event.file) > 1) {
        event.file <<- event.file[, !drop, with = FALSE]
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
  
  #ui stuffs
  output$title <- renderUI({
    singleVarDataFetcher()

    current <<- callModule(timeline, "timeline", singleVarData, longitudinal.file, metadata.file)

    groupInfo <<- callModule(customGroups, "group", groupLabel = groupLabel, metadata.file = metadata.file, useData = groupData, singleVarData = singleVarData, event.file = event.file, selected = selectedGroup, groupsType = reactive(input$groupsType), groupsTypeID = "input$groupsType", moduleName = "groupInfo")

    facetInfo <<- callModule(customGroups, "facet", groupLabel = facetLabel, metadata.file = metadata.file, useData = facetData, singleVarData = singleVarData, event.file = event.file, selected = selectedFacet, groupsType = reactive(input$facetType), groupsTypeID = "input$facetType", moduleName = "facetInfo")

    titlePanel("Data Summaries")
  })

    output$xaxis_var <- renderUI({
      if (is.null(current$var2)) {
        return()
      }

      if (is.null(properties)) {
        radioButtons(inputId = "xaxisVar",
                     label = "X-Axis:",
                     choices = list("My Date Variable" = "dateVar", "My Age Variable" = "ageVar"),
                     selected = "dateVar",
                     inline = TRUE,
                     width = '100%')
      } else {
        radioButtons(inputId = "xaxisVar",
                     label = "X-Axis:",
                     choices = list("My Date Variable" = "dateVar", "My Age Variable" = "ageVar"),
                     selected = properties$selected[properties$input == "input$xaxisVar"],
                     inline = TRUE,
                     width = '100%')
      }
    })
  
    output$groups_type <- renderUI({
      longitudinal <- current$var1
      if (!is.null(input$xaxisVar)) {
        if (input$xaxisVar == "ageVar") {
          longitudinal <- current$var2
        }
      }
      mySelected <- properties$selected[properties$input == "input$groupsType"]

      if (is.null(properties)) {
        if (!is.null(longitudinal)) {
          selectInput(inputId = "groupsType",
                      label = "Facet Line:",
                      choices = c("All possible" = "direct", "Make my own" = "makeGroups", "None" = "none"),
                      selected = "direct",
                      width = '100%')
        } else {
          selectInput(inputId = "groupsType",
                      label = "X-Axis:",
                      choices = c("All possible" = "direct", "Make my own" = "makeGroups", "None" = "none"),
                      selected = "direct",
                      width = '100%')
        }
      } else {
        if (!is.null(longitudinal)) {
          selectInput(inputId = "groupsType",
                      label = "Facet Line:",
                      choices = c("All possible" = "direct", "Make my own" = "makeGroups", "None" = "none"),
                      selected = mySelected,
                      width = '100%')
        } else {
          selectInput(inputId = "groupsType",
                      label = "X-Axis:",
                      choices = c("All possible" = "direct", "Make my own" = "makeGroups", "None" = "none"),
                      selected = mySelected,
                      width = '100%')
        }
      }
      
    })
    
    output$facet_type <- renderUI({
      mySelected <- properties$selected[properties$input == "input$facetType"]

      if (is.null(properties)) {
        selectInput(inputId = "facetType",
                    label = "Facet Plot:",
                    choices = c("All possible" = "direct", "Make my own" = "makeGroups", "None" = "none"),
                    selected = "direct",
                    width = '100%')
      } else {
        selectInput(inputId = "facetType",
                    label = "Facet Plot:",
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
    
    groupLabel <- reactive({
        if (is.null(input$groupsType)) {
          return()
        } else {
          groupsType <- input$groupsType
        }
        longitudinal <- current$var1
        if (!is.null(input$xaxisVar)) {
          if (input$xaxisVar == "ageVar") {
            longitudinal <- current$var2
          }
        }
      
        if (!is.null(longitudinal)) {
          if (groupsType == "direct") {
            label = "facets for"
          } else {
            label = "facet where:"
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
    
    groupData <- reactive({
      if (is.null(input$groupsType)) {
        return()
      } else {
        groupsType <- input$groupsType
      }
  
      #can't remember why we arent using events here....  
      if (groupsType == "direct") {
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
    
    selectedGroup <- reactive({
      if (is.null(input$groupsType)) {
        return()
      } else {
        groupsType <- input$groupsType
      }
      
      if (groupsType == "direct") {
        selected <- "EUPATH_0000054"
      } else {
        selected <- "EUPATH_0000054"
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
        selected <- "EUPATH_0000054"
      } else {
        selected <- ""
      }

      return(selected)
    }) 
 
    output$choose_yaxis <- renderUI({
      mySelected <- properties$selected[properties$input == "input$yaxis"]

      #this list should contain anything from events file
      if (event.file.exists) {
        useData <- event.file
      } else {
        useData <- singleVarData
      }
      outChoiceList <- getUIList(useData, metadata.file)
      
      if (is.null(properties)) {
        selectInput(inputId = "yaxis",
                    label = "Y-Axis:",
                    choices = outChoiceList,
                    selected = "EUPATH_0000338",
                    width = '100%')
      } else {
        selectInput(inputId = "yaxis",
                    label = "Y-Axis:",
                    choices = outChoiceList,
                    selected = mySelected,
                    width = '100%')
      }
    })
    
    output$yaxis_stp1 <- renderUI({
      if (is.null(input$yaxis)) {
        return()
      }
      myY <- input$yaxis
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

      if (dontUseProps) {      
        if (!myY %in% nums$source_id) {
          selectInput(inputId = "yaxis_stp1",
                      label = NULL,
                      choices = attrStp1List,
                      width = '100%') 
        }
      } else {
        if (!myY %in% nums$source_id) {
          selectInput(inputId = "yaxis_stp1",
                      label = NULL,
                      choices = attrStp1List,
                      selected = mySelected,
                      width = '100%')
        }
      }
      
    })
    
    output$yaxis_stp2 <- renderUI({
      if (is.null(input$yaxis)) {
        return()
      } else {
        myY <- input$yaxis
      }
      nums <- getNums(metadata.file)
      longitudinal <- current$var1
      if (!is.null(input$xaxisVar)) {
        if (input$xaxisVar == "ageVar") {
          longitudinal <- current$var2
        }
      }
      myYSelected <- properties$selected[properties$input == "input$yaxis"]
      mySelected <- properties$selected[properties$input == "input$yaxis_stp2"]      

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
          if (!is.null(longitudinal)) {
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
      } else {
        if (myY %in% nums$source_id) {
          if (!is.null(longitudinal)) {
            radioButtons(inputId = "yaxis_stp2",
                         label = "Display as:",
                         choices = list("Mean" = "mean", "Smoothed Conditional Mean" = "smooth"),
                         selected = mySelected,
                         width = '100%',
                         inline = TRUE)
          } else {
            #later think up better way. shouldnt show something with only one option. but this is a required input later on. will need if statements below instead.
            radioButtons(inputId = "yaxis_stp2",
                         label = "Display as:",
                         choices = list("Mean" = "mean"),
                         selected = mySelected,
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
                       selected = mySelected,
                       width = '100%',
                       inline = TRUE)
        }
      }
      
    })
    
    output$plot <- renderPlotly({
      if (is.null(input$yaxis_stp2)) {
        return()
      } else {
        plotType <- input$yaxis_stp2
      }
      longitudinal <- current$var1
      if (!is.null(input$xaxisVar)) {
        xaxisVar <- input$xaxisVar
        if (xaxisVar == "ageVar") {
          longitudinal <- current$var2
        }
      } 
      
      dates <- getDates(metadata.file)
        #get data from plotData here
        df <- plotData()
      
        if (is.null(df)) {
          message("plotData returned null!")
          return()
        } 
        
        names(df)[names(df) == 'GROUPS'] <- 'LINES'
        #temp placeholder for checking if data has time vars for x axis
        if (!is.null(longitudinal)) {
          #define axis labels here
          if (xaxisVar == "ageVar") {
            xlab = "Age"
          } else {
            xlab = "Time"
          }
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
          if (!longitudinal %in% dates$source_id) {
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
          
          #find num colors needed
          if (numColors > 2) { 
            myPlot <- myPlot + scale_color_manual(values = viridis(numColors))
          } else if (numColors == 2) {
            myPlot <- myPlot + scale_color_manual(values = viridis(numColors, begin = .25, end = .75))
          } else {
            myPlot <- myPlot + scale_color_manual(values = viridis(numColors, begin = .5))
          }

          if (longitudinal %in% dates$source_id) {
            myPlot <- myPlot + theme(axis.text.x = element_text(angle = 45, hjust = 1))
          }

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
          myPlot <- myPlot + labs(y = "", x = "")
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

          #find num colors needed
          if (numColors > 2) { 
            myPlot <- myPlot + scale_fill_manual(values = viridis(numColors))
          } else if (numColors == 2) {
            
            myPlot <- myPlot + scale_fill_manual(values = viridis(numColors, begin = .25, end = .75))
          } else {
            
            myPlot <- myPlot + scale_fill_manual(values = viridis(numColors, begin = .5))
          }

        }
        
        #add facet if available
        if (any(colnames(df) %in% "FACET")) {
          myPlot <- myPlot + facet_wrap(~ FACET, ncol = 1)
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
        
        myPlotly <- ggplotly(myPlot, tooltip = c("text", "x", "y"))
        #myPlotly <- ggplotly(myPlot)
        myPlotly <- plotly:::config(myPlotly, displaylogo = FALSE, collaborate = FALSE)
        myPlotly <- layout(myPlotly, margin = list(l = 70, r = 0, b = 150, t = 40),
                                     xaxis = x_list, 
                                     yaxis = y_list,
                                     legend = list(x = 100, y = .5))
        
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
        message(paste("length data:", length(data)))
        if (length(data) > 2) {
          data[, "Totals"] <- rowSums(data[, -1], na.rm=TRUE)
        }
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
    
      longitudinal <- current$var1
      if (!is.null(input$xaxisVar)) {
        if (input$xaxisVar == "ageVar") {
          longitudinal <- current$var2
        }
      }
      #temp placeholder for checking if data has time vars for x axis
      if (!is.null(longitudinal)) {
        names(data)[names(data) == 'Line'] <- 'X-Axis'
      } 

      datatable(data, 
                rownames = FALSE
      )
    })

    #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    #values grabbed through reactive functions for better control of reactive context
  
    #all the work will be done here in prepping data
    tableData <- debounce(reactive({
      
      #collecting inputs 
      longitudinal1 <- current$var1
      myTimeframe1 <- current$range1
      longitudinal2 <- current$var2
      myTimeframe2 <- current$range2
      longitudinal <- current$var1
      if (!is.null(input$xaxisVar)) {
        if (input$xaxisVar == "ageVar") {
          longitudinal <- current$var2
        }
      }
      groupsType <- input$groupsType
      facetType <- input$facetType
      myFacet <- facetInfo$group
      myY <- input$yaxis
      yaxis_stp2 <- input$yaxis_stp2
      myGroups <- groupInfo$group
      #grab optional inputs
      groups_stp1 <- groupInfo$group_stp1
      groups_stp3 <- groupInfo$group_stp3
      groups_stp4 <- groupInfo$group_stp4
      groups_stp2 <- groupInfo$group_stp2
      facet_stp1 <- facetInfo$group_stp1
      facet_stp3 <- facetInfo$group_stp3
      facet_stp4 <- facetInfo$group_stp4
      facet_stp2 <- facetInfo$group_stp2
      yaxis_stp1 <- input$yaxis_stp1
      message("have all inputs for plotData")
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
  
      strings <- subset(metadata.file, metadata.file$type == "string", "source_id")
      go <- TRUE
      
      if (is.null(groupsType)) {
        go <- FALSE
      } else {
        if (groupsType == "makeGroups") {
          if (is.null(groups_stp1)) {
            go <- FALSE
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
      
      #once last field is populated .. GO
      if (go) {
        message("GO!!")
        #may not need to do the splitting on pipes. grepl will still return true for it.
        #should have natozero before this for non-anthro events?? so an NA for diarrhea -> 0 ?? 

        #could maybe make this a function just to improve readability 
        #first thing is to save properties 
        longitudinalText <- paste0("current$var1\t", longitudinal1, "\n",
                                 "current$range1[1]\t", myTimeframe1[1], "\n",
                                 "current$range1[2]\t", myTimeframe1[2], "\n",
                                 "current$var2\t", longitudinal2, "\n",
                                 "current$range2[1]\t", myTimeframe2[1], "\n",
                                 "current$range2[2]\t", myTimeframe2[2], "\n",
                                 "input$xaxisVar\t", input$xaxisVar, "\n")


        if (length(facet_stp1) > 1) {
          if (length(groups_stp1) > 1) {
            text <- paste0("input\tselected\n",
                    longitudinalText,
                    "facetInfo$group\t", myFacet, "\n",
                    "facetInfo$group_stp1[1]\t", facet_stp1[1], "\n",
                    "facetInfo$group_stp1[2]\t", facet_stp1[2], "\n",
                    "facetInfo$group_stp2\t", facet_stp2, "\n",
                    "facetInfo$group_stp3\t", facet_stp3, "\n",
                    "facetInfo$group_stp4\t", facet_stp4, "\n",
                    "groupInfo$group\t", myGroups, "\n",
                    "groupInfo$group_stp1[1]\t", groups_stp1[1], "\n",
                    "groupInfo$group_stp1[2]\t", groups_stp1[2], "\n",
                    "groupInfo$group_stp2\t", groups_stp2, "\n",
                    "groupInfo$group_stp3\t", groups_stp3, "\n",
                    "groupInfo$group_stp4\t", groups_stp4, "\n",
                    "input$groupsType\t", groupsType, "\n",
                    "input$facetType\t", facetType, "\n",
                    "input$yaxis\t", myY, "\n",
                    "input$yaxis_stp1\t", yaxis_stp1, "\n",
                    "input$yaxis_stp2\t", yaxis_stp2
                   )
          } else {
            text <- paste0("input\tselected\n",
                    longitudinalText,
                    "facetInfo$group\t", myFacet, "\n",
                    "facetInfo$group_stp1[1]\t", facet_stp1[1], "\n",
                    "facetInfo$group_stp1[2]\t", facet_stp1[2], "\n",
                    "facetInfo$group_stp2\t", facet_stp2, "\n",
                    "facetInfo$group_stp3\t", facet_stp3, "\n",
                    "facetInfo$group_stp4\t", facet_stp4, "\n",
                    "groupInfo$group\t", myGroups, "\n",
                    "groupInfo$group_stp1\t", groups_stp1, "\n",
                    "groupInfo$group_stp2\t", groups_stp2, "\n",
                    "groupInfo$group_stp3\t", groups_stp3, "\n",
                    "groupInfo$group_stp4\t", groups_stp4, "\n",
                    "input$groupsType\t", groupsType, "\n",
                    "input$facetType\t", facetType, "\n",
                    "input$yaxis\t", myY, "\n",
                    "input$yaxis_stp1\t", yaxis_stp1, "\n",
                    "input$yaxis_stp2\t", yaxis_stp2
                   )
          }
        } else {
          if (length(groups_stp1) > 1) {
            text <- paste0("input\tselected\n",
                    longitudinalText,
                    "facetInfo$group\t", myFacet, "\n",
                    "facetInfo$group_stp1\t", facet_stp1, "\n",
                    "facetInfo$group_stp2\t", facet_stp2, "\n",
                    "facetInfo$group_stp3\t", facet_stp3, "\n",
                    "facetInfo$group_stp4\t", facet_stp4, "\n",
                    "groupInfo$group\t", myGroups, "\n",
                    "groupInfo$group_stp1[1]\t", groups_stp1[1], "\n",
                    "groupInfo$group_stp1[2]\t", groups_stp1[2], "\n",
                    "groupInfo$group_stp2\t", groups_stp2, "\n",
                    "groupInfo$group_stp3\t", groups_stp3, "\n",
                    "groupInfo$group_stp4\t", groups_stp4, "\n",
                    "input$groupsType\t", groupsType, "\n",
                    "input$facetType\t", facetType, "\n",
                    "input$yaxis\t", myY, "\n",
                    "input$yaxis_stp1\t", yaxis_stp1, "\n",
                    "input$yaxis_stp2\t", yaxis_stp2
                   )
          } else {
            text <- paste0("input\tselected\n",
                    longitudinalText,
                    "facetInfo$group\t", myFacet, "\n",
                    "facetInfo$group_stp1\t", facet_stp1, "\n",
                    "facetInfo$group_stp2\t", facet_stp2, "\n",
                    "facetInfo$group_stp3\t", facet_stp3, "\n",
                    "facetInfo$group_stp4\t", facet_stp4, "\n",
                    "groupInfo$group\t", myGroups, "\n",
                    "groupInfo$group_stp1\t", groups_stp1, "\n",
                    "groupInfo$group_stp2\t", groups_stp2, "\n",
                    "groupInfo$group_stp3\t", groups_stp3, "\n",
                    "groupInfo$group_stp4\t", groups_stp4, "\n",
                    "input$groupsType\t", groupsType, "\n",
                    "input$facetType\t", facetType, "\n",
                    "input$yaxis\t", myY, "\n",
                    "input$yaxis_stp1\t", yaxis_stp1, "\n",
                    "input$yaxis_stp2\t", yaxis_stp2
                   )
          }
        }

        PUT(propUrl, body = "")
        PUT(propUrl, body = text)

        message(paste("\t ", head(data)))
        plotData <- completeDT(data, myY)
        plotData <- getFinalDT(plotData, metadata.file, myY)
        
        if (!is.null(longitudinal)) {
          myCols <- c("Participant_Id", myY, longitudinal)
          tempData <- plotData[, myCols, with=FALSE] 
          colnames(tempData) <- c("Participant_Id", "YAXIS", "XAXIS")
        } else {
          myCols <- c("Participant_Id", myY)
          tempData <- plotData[, myCols, with=FALSE] 
          colnames(tempData) <- c("Participant_Id", "YAXIS")
        }
        
        if (groupsType == "direct") {
          message("groups is direct")
          myCols <- c("Participant_Id", myGroups)
          groupData <- plotData[, myCols, with=FALSE]
          groupData <- unique(groupData)
          colnames(groupData) <- c("Participant_Id", "GROUPS")
          tempData <- merge(tempData, groupData, by = "Participant_Id")
        } 
        
        if (facetType == "direct") {
          message("facet is direct")
          myCols <- c("Participant_Id", myFacet)
          facetData <- plotData[, myCols, with=FALSE]
          facetData <- unique(facetData)
          colnames(facetData) <- c("Participant_Id", "FACET")
          tempData <- merge(tempData, facetData, by = "Participant_Id")
        } 
        
        plotData <- tempData
        #need better way. too specific right now. just need to know if xaxis is time
        #consider what to do about cinning for actual dates. will that work??
        if (!is.null(longitudinal)) {
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
            label <- makeGroupLabel(myFacet, metadata.file, facet_stp1, facet_stp2, facet_stp3, facet_stp4, event.list = colnames(event.file))
            message(paste("label is:", label))
            message("have custom facet! now merge..")
            #add makeGroups data to df and return
            colnames(outData) <- c("Participant_Id", "FACET")
            #will need a var called label that changes based on what the facet steps are. the below only works for strings.
            #if (any(colnames(event.file) %in% myFacet)) {
             # naToZero(outData, "FACET")
            #}
            message(paste("levels facet:", levels(as.factor(outData$FACET))))
            outData <- transform(outData, "FACET" = ifelse(as.numeric(FACET) == 0, label[2], label[1]))
           # outData$FACET <- factor(outData$FACET, levels(c("Other", facet_stp2)))
            message(paste("levels facet:", levels(as.factor(outData$FACET))))
            plotData <- merge(plotData, outData, by = "Participant_Id", all = TRUE)
            message(paste("levels facet:", levels(as.factor(plotData$FACET))))
          }
        }
        #if groups col exists return
        if (any(colnames(plotData) %in% "GROUPS")) {
          if (myGroups %in% nums$source_id | myGroups %in% dates$source_id) {
            if (length(levels(as.factor(plotData$GROUPS))) >= 4) {
              message("need to bin dates")
              hold <- try(ggplot2::cut_number(plotData$GROUPS, 4))
              message(paste("test binning:", levels(as.factor(hold))))
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
            outData <- makeGroups(data, metadata.file, myGroups, groups_stp1, groups_stp2, groups_stp3, groups_stp4)
            label <- makeGroupLabel(myGroups, metadata.file, groups_stp1, groups_stp2, groups_stp3, groups_stp4, event.list = colnames(event.file))
            message(paste("label is:", label))
            if (any(colnames(event.file) %in% myGroups)) {
              naToZero(plotData, "GROUPS")
            }
            message("have custom groups! now merge..")
            #add makeGroups data to df and return
            outData <- transform(outData, "GROUPS" = ifelse(as.numeric(GROUPS) == 0, label[2], label[1]))
            plotData <- merge(plotData, outData, by = "Participant_Id", all = TRUE)
            print("NA in groups:", any(is.na(plotData$GROUPS)))
          } else {
            plotData <- cbind(plotData, "GROUPS" = "All Participants")
          }
        }
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
          myY <- input$yaxis
          message(paste("my y from plotData:", myY))
          if (is.null(input$yaxis_stp2)) {
            return()
          } else {
            yaxis_stp2 <- input$yaxis_stp2
          }
          yaxis_stp1 <- input$yaxis_stp1
          
          strings <- getStrings(metadata.file)
        
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
          #the following to get proportions of prtcpnts with matching observatio rather than proportion of matching observations.
          tempData <- aggregate(as.formula(paste0(aggStr1, " + Participant_Id")), plotData, sum)
          tempData <- transform(tempData, "YAXIS"=ifelse(YAXIS > 1, 1, 0))
          mergeData <- aggregate(as.formula(aggStr1), tempData, sum) 
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
