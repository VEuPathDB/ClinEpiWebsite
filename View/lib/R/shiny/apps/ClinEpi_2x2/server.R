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
  longitudinal <- NULL
  current <- NULL
  attrInfo <- NULL
  outInfo <- NULL
  attribute.file <- NULL
  propUrl <- NULL

  filesFetcher <- reactive({
 
  #not sure if i need properties here, or just in the module
  #also not sure which vars should be scopes to the page, rather than the function
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

    singleVarData
  }
 
  output$title <- renderUI({
    singleVarDataFetcher()

    current <<- callModule(timeline, "timeline", singleVarData, longitudinal, metadata.file)
  
    attrInfo <<- callModule(customGroups, "attr", groupLabel = reactive("Variable 1:"), useData = reactive(list(singleVarData)), metadata.file = metadata.file, singleVarData = singleVarData, event.file = event.file, selected = reactive("EUPATH_0000704"), moduleName = "attrInfo")
 
    outInfo <<- callModule(customGroups, "out", groupLabel = reactive("Variable 2:"), useData = reactive(list(singleVarData)), metadata.file = metadata.file, singleVarData = singleVarData, event.file = event.file, selected = reactive("EUPATH_0000665"), moduleName = "outInfo") 
    print("done with modules")
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
        myPlotly <- layout(myPlotly, margin = list(l = 150, r = 20, b = 30, t = 20), 
                                     xaxis = x_list, 
                                     yaxis = y_list)
        
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
      myTimeframe <- current$timeframe
      longitudinal <- current$longitudinal
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
      #which cols can be used for this will have to change. too specific right now
      if (any(colnames(singleVarData) %in% longitudinal)) {
        if (!is.null(myTimeframe)) {
          data <- subsetDataFetcher(myTimeframe[1], myTimeframe[2], singleVarData, longitudinal)
        } else {
          print("exiting for timeline problem")
          return()
        }
      } else {
        data <- singleVarData
      }
  
      go <- TRUE
      
      if (is.null(outInfo$group_stp1) | is.null(attrInfo$group_stp1)) {
        print("out or attr stp1 is null")
        go <- FALSE
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
  
        #could maybe make this a function just to improve readability 
        #first thing is to save properties 
        if (length(attr_stp1) > 1) {
          if (length(out_stp1) > 1) {
            text <- paste0("input\tselected\n",
                    "current$longitudinal\t", longitudinal, "\n",
                    "current$timeframe[1]\t", myTimeframe[1], "\n",
                    "current$timeframe[2]\t", myTimeframe[2], "\n",
                    "attrInfo$group\t", myAttr, "\n",
                    "attrInfo$group_stp1[1]\t", attr_stp1[1], "\n",
                    "attrInfo$group_stp1[2]\t", attr_stp1[2], "\n",
                    "attrInfo$group_stp2\t", attr_stp2, "\n",
                    "attrInfo$group_stp3\t", attr_stp3, "\n",
                    "attrInfo$group_stp4\t", attr_stp4, "\n",
                    "outInfo$group\t", myOut, "\n",
                    "outInfo$group_stp1[1]\t", out_stp1[1], "\n",
                    "outInfo$group_stp1[2]\t", out_stp1[2], "\n",
                    "outInfo$group_stp2\t", out_stp2, "\n",
                    "outInfo$group_stp3\t", out_stp3, "\n",
                    "outInfo$group_stp4\t", out_stp4
                   )
          } else {
            text <- paste0("input\tselected\n",
                    "current$longitudinal\t", longitudinal, "\n",
                    "current$timeframe[1]\t", myTimeframe[1], "\n",
                    "current$timeframe[2]\t", myTimeframe[2], "\n",
                    "attrInfo$group\t", myAttr, "\n",
                    "attrInfo$group_stp1[1]\t", attr_stp1[1], "\n",
                    "attrInfo$group_stp1[2]\t", attr_stp1[2], "\n",
                    "attrInfo$group_stp2\t", attr_stp2, "\n",
                    "attrInfo$group_stp3\t", attr_stp3, "\n",
                    "attrInfo$group_stp4\t", attr_stp4, "\n",
                    "outInfo$group\t", myOut, "\n",
                    "outInfo$group_stp1\t", out_stp1, "\n",
                    "outInfo$group_stp2\t", out_stp2, "\n",
                    "outInfo$group_stp3\t", out_stp3, "\n",
                    "outInfo$group_stp4\t", out_stp4
                   )
          }
        } else {
          if (length(out_stp1) > 1) {
            text <- paste0("input\tselected\n",
                    "current$longitudinal\t", longitudinal, "\n",
                    "current$timeframe[1]\t", myTimeframe[1], "\n",
                    "current$timeframe[2]\t", myTimeframe[2], "\n",
                    "attrInfo$group\t", myAttr, "\n",
                    "attrInfo$group_stp1\t", attr_stp1, "\n",
                    "attrInfo$group_stp2\t", attr_stp2, "\n",
                    "attrInfo$group_stp3\t", attr_stp3, "\n",
                    "attrInfo$group_stp4\t", attr_stp4, "\n",
                    "outInfo$group\t", myOut, "\n",
                    "outInfo$group_stp1[1]\t", out_stp1[1], "\n",
                    "outInfo$group_stp1[2]\t", out_stp1[2], "\n",
                    "outInfo$group_stp2\t", out_stp2, "\n",
                    "outInfo$group_stp3\t", out_stp3, "\n",
                    "outInfo$group_stp4\t", out_stp4
                   )
          } else {
            text <- paste0("input\tselected\n",
                    "current$longitudinal\t", longitudinal, "\n",
                    "current$timeframe[1]\t", myTimeframe[1], "\n",
                    "current$timeframe[2]\t", myTimeframe[2], "\n",
                    "attrInfo$group\t", myAttr, "\n",
                    "attrInfo$group_stp1\t", attr_stp1, "\n",
                    "attrInfo$group_stp2\t", attr_stp2, "\n",
                    "attrInfo$group_stp3\t", attr_stp3, "\n",
                    "attrInfo$group_stp4\t", attr_stp4, "\n",
                    "outInfo$group\t", myOut, "\n",
                    "outInfo$group_stp1\t", out_stp1, "\n",
                    "outInfo$group_stp2\t", out_stp2, "\n",
                    "outInfo$group_stp3\t", out_stp3, "\n",
                    "outInfo$group_stp4\t", out_stp4
                   )
          }
        }

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
        attrLabel <- makeGroupLabel(myAttr, metadata.file, attr_stp1, attr_stp2, attr_stp3, attr_stp4)
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
        outLabel <- makeGroupLabel(myOut, metadata.file, out_stp1, out_stp2, out_stp3, out_stp4)
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
        print(tableData)
        tableData
      } 
      
    })

})
