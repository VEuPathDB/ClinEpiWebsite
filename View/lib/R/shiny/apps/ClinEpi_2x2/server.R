## server.r

#the sliders only show transparent for the first two to appear, names not reused/ dont refer only to whats on screen at the time
#do we want to move naToZero before the merge and only apply to outdata? seems NA for prtcpnt and house info are real NAs

shinyServer(function(input, output, session) {
  
  event.file <- NULL
  prtcpnt.file <- NULL
  house.file <- NULL
  attributes.file <- NULL
  metadata.file <- NULL
  singleVarData <- NULL
  longitudinal.file <- NULL
  current <- NULL
  attrInfo <- NULL
  outInfo <- NULL
  attribute.file <- NULL
  propUrl <- NULL
  longitudinal1 <- NULL
  longitudinal2 <- NULL

  filesFetcher <- reactive({
  if (is.null(propUrl)) {
    propUrl <<- getPropertiesUrl(session)
  }
  message(paste("propUrl:", propUrl))

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
    prtcpnt.file <<- prtcpnt.file[, (drop):=NULL]
    
    if (exists("event.file") & !is.null(event.file) & nrow(event.file) > 1) {
      merge1 <- merge(event.file, prtcpnt.file, by = "Participant_Id")
    } else {
      merge1 <- prtcpnt.file
    }
    
    if (exists("house.file") & !is.null(house.file) & nrow(house.file) > 1) {
      #house.file <<- house.file[, (drop):=NULL]
      house.file <<- house.file[, -(drop), with = FALSE]
      singleVarData <<- merge(merge1, house.file, by = "Participant_Id")
    } else {
      singleVarData <<- merge1
    }
    
    if (any(colnames(singleVarData) %in% "EUPATH_0000644")) {
      setkey(singleVarData, EUPATH_0000644)
    }

    #remove unnecessary metadata info
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
    singleVarDataFetcher()

    current <<- callModule(timeline, "timeline", singleVarData, longitudinal.file, metadata.file)
  
    attrInfo <<- callModule(customGroups, "attr", groupLabel = reactive("Variable 1:"), useData = reactive(list(singleVarData)), metadata.file = metadata.file, singleVarData = singleVarData, event.file = event.file, selected = reactive("EUPATH_0000338"), moduleName = "attrInfo")
 
    outInfo <<- callModule(customGroups, "out", groupLabel = reactive("Variable 2:"), useData = reactive(list(singleVarData)), metadata.file = metadata.file, singleVarData = singleVarData, event.file = event.file, selected = reactive("EUPATH_0000054"), moduleName = "outInfo") 

    titlePanel("Contingency Tables")
  }) 
  
    output$plot <- renderPlotly({
      print("about to render plot")
        tableData <- plotData()
        if (is.null(tableData)) {
          message("plotData returned null!")
          return()
        }
        
        rows <- tableData$rownames
        #remove totals
        df <- tableData[ -nrow(tableData), ]
        df$Totals <- NULL
        df$rownames <- NULL
        #reorganize to make stacked bar
        cols <- colnames(df)
        df <- t(df)
        message("plot df after transform")
        df <- rbind(df, c(df[1,2], NA))
        df <- rbind(df, c(df[2,2], NA))
        df <- cbind(c(cols[1], cols[2], cols[1], cols[2]), df)
        colnames(df) <- c("Variable2" ,"Proportion", "Variable1")
        df <- data.table(df)
        df$Proportion = as.numeric(df$Proportion)
        df$Variable1 <- c(rows[1], rows[1], rows[2], rows[2])
        df$Variable2 <- factor(df$Variable2, levels = c(cols[1], cols[2]))
        df$Variable1 <- factor(df$Variable1, levels = c(rows[1], rows[2]))
        
        #define axis labels here
        xlab <- ""
        ylab <- "Proportion"
        
        #determine width of bars for outcome
        outPos <- tableData[1,3 , with = FALSE]
        outNeg <- tableData[2,3 , with = FALSE]
        total <- tableData$Totals[3]
        OPprop <- outPos / total
        ONprop <- outNeg / total
        width <- c( OPprop*1.9, ONprop*1.9, OPprop*1.9, ONprop*1.9)
        df$width <- width
   
        #plot here
        myPlot <- ggplot(data = df, aes(x = Variable2, y = Proportion, width = width, fill = Variable1))
        myPlot <- myPlot + theme_bw()
        myPlot <- myPlot + labs(y = ylab, x = xlab)
        
        myPlot <- myPlot + geom_bar(stat = "identity", position = "fill")
        #myPlot <- myPlot + scale_fill_manual(values = c("#32baba", "#e26c6c"))
        #myPlot <- myPlot + scale_fill_manual(values = plasma(2))
        myPlot <- myPlot + scale_fill_manual(values = viridis(2, begin = .25, end = .75))
        
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
 
        #myPlotly <- ggplotly(myPlot, tooltip = c("text", "x"))
        myPlotly <- ggplotly(myPlot)
        myPlotly <- plotly:::config(myPlotly, displaylogo = FALSE, collaborate = FALSE)
        myPlotly <- layout(myPlotly, margin = list(l = 70, r = 0, b = 30, t = 40), 
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

      data$rownames <- NULL
      datatable(data, 
                width = '100%',
                options = list(
                  sDom = '<"top"><"bottom">',
                  autoWidth = TRUE, 
                  columnDefs = list(list(width = '250px', className = 'dt-right', targets = c(1,2,3)))
        )
      )
    })
    
    output$statsTable <- DT::renderDataTable({
      tableData <- plotData()
      if (is.null(tableData)) {
        return()
      }
      
      #remove totals
      df <- tableData[ -nrow(tableData), ]
      df$Totals <- NULL
      df$rownames <- NULL
      #get pval
      p <- chisq.test(df)
      p <- p$p.value
      #get or
      a <- df[1,1 , with = FALSE]
      b <- df[2,1 , with = FALSE]
      c <- df[1,2, with = FALSE]
      d <- df[2,2 , with = FALSE]
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

      p <- round(p, digits=4)
      if (p != "NaN" & p < 0.0001) {
        p <- "<0.0001"
      }
      #make stats table
      odds.ratio <- c(OR, paste(ORlo, "-", ORhi))
      relative.risk <- c(RR, paste(RRlo, "-", RRhi))
      p.val <- c(p, "N/A")
      stats <- data.table("p-value" = p.val, "Odds Ratio" = odds.ratio, "Relative Risk" = relative.risk)
      rownames(stats) <- c("Statistics", "95% Confidence Interval")
      
      datatable(stats, 
                width = '100%',
                options = list(
                  sDom = '<"top"><"bottom">',
                  autoWidth = TRUE, 
                  columnDefs = list(list(width = '250px', className = 'dt-right', targets = c(1,2,3)))
                )
      )
    })
    
    #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    #values grabbed through reactive functions for better control of reactive context

    #all the work will be done here in prepping data
    plotData <- reactive({
      #test <- propText()    
  
      #collecting inputs 
      myTimeframe1 <- current$range1
      myTimeframe2 <- current$range2
      if (is.null(attrInfo$group)) {
        message("attr group is null")
        return()
      } else {
        message("setting myAttr")
        myAttr <- attrInfo$group
      }
      if (is.null(outInfo$group)) {
        print("out group is null")
        return()
      } else {
        print("setting myOut")
        myOut <- outInfo$group
      }
      print("have attr and out info")
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

        text <- paste0("input\tselected\n",
                       longitudinalText,
                       attrText,
                       outText
                      )

        PUT(propUrl, body = "")
        PUT(propUrl, body = text)   
 
        #get attr col
        attrData <- completeDT(data, myAttr)
        attrData <- getFinalDT(attrData, metadata.file, myAttr)
        myCols <- c("Participant_Id", myAttr)
        attrData <- attrData[, myCols, with=FALSE]
        
        numeric <- c("lessThan", "greaterThan", "equals")
        anthro <- c("percentDays", "delta", "direct")
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
        attrData <- makeGroups(attrData, metadata.file, myAttr, attr_stp1, attr_stp2, attr_stp3, attr_stp4)
        attrLabel <- makeGroupLabel(myAttr, metadata.file, attr_stp1, attr_stp2, attr_stp3, attr_stp4, colnames(event.file))
        colnames(attrData) <- c("Participant_Id", "Attribute")
       print(attrData)
        #get outcome data
        #may not need to do the splitting on pipes. grepl will still return true for it.
        outData <- completeDT(data, myOut)
        outData <- getFinalDT(outData, metadata.file, myOut)
        myCols <- c("Participant_Id", myOut)
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
        
        outData <- makeGroups(outData, metadata.file, myOut, out_stp1, out_stp2, out_stp3, out_stp4)
        outLabel <- makeGroupLabel(myOut, metadata.file, out_stp1, out_stp2, out_stp3, out_stp4, colnames(event.file))
        colnames(outData) <- c("Participant_Id", "Outcome")
        print(outData)
        #merge on participant id an1d keep all prtcpnts.
        data <- merge(attrData, outData, by = "Participant_Id", all = TRUE)
        #replace NA with 0, essentially assuming that no reporting is negative reporting
        naToZero(data)
        
        #format into 2x2
        data <- transform(data, "APOP" = ifelse( Attribute == 1 & Outcome == 1, 1, 0))
        data <- transform(data, "APOF" = ifelse( Attribute == 1 & Outcome == 0, 1, 0))
        data <- transform(data, "AFOP" = ifelse( Attribute == 0 & Outcome == 1, 1, 0))
        data <- transform(data, "AFOF" = ifelse( Attribute == 0 & Outcome == 0, 1, 0))
        
        APOP <- sum(data$APOP)
        APOF <- sum(data$APOF)
        AFOP <- sum(data$AFOP)
        AFOF <- sum(data$AFOF)
        
        APtotal <- APOP + APOF
        AFtotal <- AFOP + AFOF
        OPtotal <- APOP + AFOP
        OFtotal <- APOF + AFOF
        total <- APOP + APOF + AFOP + AFOF
        
        OP <- c(APOP, AFOP, OPtotal)
        OF <- c(APOF, AFOF, OFtotal)
        totals <- c(APtotal, AFtotal, total)
        
        #tableData <- data.table(outLabel[1] = OP, outLabel[2] = OF, "Totals" = totals)
        tableData <- data.table("Outcome+" = OP, "Outcome-" = OF, "Totals" = totals)
        colnames(tableData) <- c(outLabel[1], outLabel[2], "Totals")
        rownames(tableData) <- c(attrLabel[1], attrLabel[2], "Totals")
        tableData$rownames <- c(attrLabel[1], attrLabel[2], "Totals")

        if (all(sort(colnames(tableData)) == c("No", "rownames", "Totals", "Yes"))) {
          setcolorder(tableData, c("Yes", "No", "Totals", "rownames")) 
        }
        if (all(sort(rownames(tableData)) == c("No", "Totals", "Yes"))) {
          setroworder(tableData, c(2,1,3))
          rownames(tableData) <- c("Yes", "No", "Totals")
          tableData$rownames <- c("Yes", "No", "Totals")
        }
        message(colnames(tableData))
        message(rownames(tableData))
        message(tableData)
        tableData
      } 
      
    })

})
