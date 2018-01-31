### all clinepi specific functions b/c rely on data and/or metadata to be formatted consistently


longitudinalText <- function(myTimeframe1, myTimeframe2) {
  longitudinalText <- paste0("current$range1[1]\t", myTimeframe1[1], "\n",
                             "current$range1[2]\t", myTimeframe1[2], "\n",
                             "current$range2[1]\t", myTimeframe2[1], "\n",
                             "current$range2[2]\t", myTimeframe2[2], "\n")

  longitudinalText
}

groupText <- function(moduleName, myGroups, groups_stp1, groups_stp2, groups_stp3, groups_stp4) {
  if (length(groups_stp2) > 1) {
    groupsStp2Text <- ""
    for (i in seq(length(groups_stp2))) {
      groupsStp2Text <- paste0(groupsStp2Text,
      moduleName, "$group_stp2\t", groups_stp2[i], "\n")
    }
  } else {
    groupsStp2Text <- paste0(moduleName, "$group_stp2\t", groups_stp2, "\n")
  }

  if (length(groups_stp1) > 1) {
    #check if date or strings
    if (all(isDate(groups_stp1))) {
      groupsStp1Text <- paste0(moduleName, "$group_stp1[1]\t", groups_stp1[1], "\n",
                               moduleName, "$group_stp1[2]\t", groups_stp1[2], "\n")
    } else {
      groupsStp1Text <- ""
      for (i in seq(length(groups_stp1))) {
        groupsStp1Text <- paste0(groupsStp1Text,
        moduleName, "$group_stp1\t", groups_stp1[i], "\n")
      }
    }
  } else {
    groupsStp1Text <- paste0(moduleName, "$group_stp1\t", groups_stp1, "\n")
  }

    groupsText <- paste0(moduleName, "$group\t", myGroups, "\n",
                         groupsStp1Text,
                         groupsStp2Text,
                         moduleName, "$group_stp3\t", groups_stp3, "\n",
                         moduleName, "$group_stp4\t", groups_stp4, "\n")

  groupsText
}

subsetDataFetcher <- function(min, max, data, col){

  #this version maled specific.
  #eupath_0000743 is last date observed. so the below doesnt include prtcpnts who drop out before the max day set
  #this inner if statement temporary (hopefully) until we sort the root of the problem. events table has data up to ageday 745, but prtcpnt file says no observations past 732
  #if (any(colnames(data) %in% "EUPATH_0000743")) {
  #  grabMe <- length(levels(as.factor(data$EUPATH_0000743)))
  #  maxDay <- levels(as.factor(data$EUPATH_0000743))[grabMe]
  #  if (max > maxDay) {
  #    tempDF <- data[data$EUPATH_0000644 >= min & data$EUPATH_0000644 <= max]
  #  } else {
   #   tempDF <- data[data$EUPATH_0000644 >= min & data$EUPATH_0000644 <= max & data$EUPATH_0000743 >= max]
   # }
  #} else {
  #  tempDF <- data
  #}
 
  tempDF <- data[data[[col]] >= min & data[[col]] <= max] 
 
  tempDF
}
  
getNums <- function(metadata.file){
  #identify nums 
  nums <- subset(metadata.file, metadata.file$type == "number", "source_id") 
   
  nums
}
    
getStrings <- function(metadata.file){
  #identify strings 
  strings <- subset(metadata.file, metadata.file$type == "string", "source_id") 
      
  strings
}
    
getDates <- function(metadata.file){
  #identify dates 
  dates <- subset(metadata.file, metadata.file$type == "date", "source_id") 
      
  dates
}

#this just a list of clinepi source_ids we want to make sure never appear in ui    
getDropList <- function(){
  c("EUPATH_0000644", "BFO_0000015", "EUPATH_0000702", "OBI_0100051")
}

#makes named list containing source_ids and display names for ui
#can limit what gets returned by limiting what cols are in data  
#can also provide list of data frames to grab cols from i.e. data = list(df1, df2)
getUIList <- function(data, metadata.file, minLevels = 1, maxLevels = Inf, addNone = FALSE) {
  drop <- getDropList()

  colnames <- colnames(data)
  colnames <- setdiff(colnames, drop)

  #get display names from metadata
  choices <- subset(metadata.file, source_id %in% colnames)
  if (nrow(choices) == 0) {
    return()
  }
  choicesNumeric <- subset(choices, type %in% "number")
 # choicesDates <- subset(choices, type %in% "date")

  #limit by minLevels and maxLevels
  temp <- as.vector(choices$source_id)
  boolean <- sapply(data[ , (temp), with=FALSE ], function(x) {(length(levels(as.factor(x))) <= maxLevels & length(levels(as.factor(x))) >= minLevels)})
  choices <- choices[match(names(boolean), choices$source_id),]
  choices <- choices[as.vector(boolean),]

  #add numbers back in, sort alphabetically and convert df to named list
  choices <- rbind(choices, choicesNumeric)#, choicesDates)
  choices <- unique(choices)
  myorder <- sort(choices$property)
  choices <- choices[match(myorder, choices$property),]
  if (addNone) {
    choiceList <- as.vector(c("none", choices$source_id))
    names(choiceList) <- as.vector(c("None", choices$property))
  } else {
    choiceList <- as.vector(choices$source_id)
    names(choiceList) <- as.vector(choices$property)
  }
  mylist <- as.list(choiceList)

  mylist
}
   
#this ui will be different from above one(s)
#returns all values for a column, can probably just pass singleVarData as data param
getUIStp1List <- function(data, col){

  tempDF <- completeDT(data, col)
  data <- setDT(tempDF)[, lapply(.SD, function(x) unlist(tstrsplit(x, " | ", fixed=TRUE))), 
                      by = setdiff(names(tempDF), eval(col))][!is.na(eval(col))]

  levels <- levels(as.factor(data[[col]]))
}
    
#seperate pipe delimited fields into their own rows if necessary
getFinalDT <- function(data, metadata.file, col){
     
  strings <- getStrings(metadata.file)
   
  if (col %in% strings$source_id) {
    data <- setDT(data)[, lapply(.SD, function(x) unlist(tstrsplit(x, " | ", fixed=TRUE))), 
                          by = setdiff(names(data), eval(col))][!is.na(eval(col))]
  }
       
  data
      
}
    
#this takes inputs passed to it and creates subsets of data based on those inputs
makeGroups <- function(data, metadata.file, myGroups, groups_stp1, groups_stp2, groups_stp3, groups_stp4){
  #realistically should check for nulls before calling, but just in case
  if (is.null(groups_stp1)) {
    message("groups stp1 is null!! returning")
    return()
  }
  message("preparing data to make own groups")
  #get group data for make groups option
  groupData <- completeDT(data, myGroups)
  groupData <- getFinalDT(groupData, metadata.file, myGroups)
  myCols <- c("Participant_Id", myGroups)
  outData <- groupData[, myCols, with=FALSE]

  obs <- c("any", "all")

  #if anthro direct comparison do same as for number
  if (!any(c("POSIXct", "Date") %in% class(groups_stp1))) {
    if (groups_stp1 == "direct") {
      if (is.null(groups_stp3)) {
        return()
      }
      groups_stp1 = groups_stp2
      groups_stp2 = groups_stp3
    }
  }

   #this check needs to come after the other in order to avoid erroneously shifting values twice. think up better way later.
   #if dealing with an observation, set a flag to determine if any or all option, then move up the ui
  obsFlag = -1
  if (groups_stp1 %in% obs) {
    if (groups_stp1 == "any") {
      obsFlag <- "any"
    } else {
      obsFlag <- "all"
    }
    groups_stp1 = groups_stp2
    groups_stp2 = groups_stp3
  }

  #call either the any or the all function
  if (obsFlag != "all") {
    outData <- anyGroups(outData, metadata.file, myGroups, groups_stp1, groups_stp2, groups_stp3, groups_stp4)
  } else {
    outData <- allGroups(outData, metadata.file, myGroups, groups_stp1, groups_stp2, groups_stp3, groups_stp4)
  }
  
  outData
}

anyGroups <- function(outData, metadata.file, myGroups, groups_stp1, groups_stp2, groups_stp3, groups_stp4) {

  #this if statement will have to change. handle dates first
  if (any(c("POSIXct", "Date") %in% class(groups_stp1))) {
    outData <- transform(data, "GROUPS" = ifelse(data[[myGroups]] < groups_stp1[2] & data[[myGroups]] > groups_stp1[1], 1, 0))
    cols <- c("Participant_Id", "GROUPS")
    outData <- outData[, cols, with = FALSE]
    outData <- unique(outData)
    message("custom groups for date")
  #for numbers
  } else if (groups_stp1 == "lessThan") {
    if (is.null(groups_stp2)) {
      return()
    }
    outData <- aggregate(outData, by=list(outData$Participant_Id), FUN = function(x){ if (any(x < as.numeric(groups_stp2))) {1} else {0} })
  } else if (groups_stp1 == "greaterThan") {
    if (is.null(groups_stp2)) {
      return()
    }
    outData <- aggregate(outData, by=list(outData$Participant_Id), FUN = function(x){ if (any(x > as.numeric(groups_stp2))) {1} else {0} })
  } else if (groups_stp1 == "equals") {
    if (is.null(groups_stp2)) {
      return()
    }
    outData <- aggregate(outData, by=list(outData$Participant_Id), FUN = function(x){ if (any(x == as.numeric(groups_stp2))) {1} else {0} })
    #for change over time  
  } else if (groups_stp1 == "delta") {
    if(is.null(groups_stp3)) {
      return()
    }
    outData <- completeDT(data, myGroups)
    outData <- getFinalDT(outData, metadata.file, myGroups)
    myCols <- c("Participant_Id", "EUPATH_0000644", myGroups)
    outData <- outData[, myCols, with=FALSE]
    #should start an empty table here to add values in as i go through the loop
    tempTable <- NULL
    #do the below for each participant. think i need for loop. :( but will see if i can think up better way.
    prtcpnts <- levels(as.factor(outData$Participant_Id))
    for (i in prtcpnts) {
      currData <- subset(outData, outData$Participant_Id %in% i)
      
      startDay <- min(currData[, EUPATH_0000644])
      startVal <- currData[[myGroups]][currData$EUPATH_0000644 == startDay]
      endVal <- currData[[myGroups]][currData$EUPATH_0000644 == max(currData[, EUPATH_0000644])]
      diffVal <- startVal - endVal
      
      #if statement for direction of change
      if (startVal > endVal) {
        diffVal = diffVal * -1
      }
      
      if (groups_stp2 == "lessThan") {
        if (diffVal < groups_stp3) {
          row <- c(i, 1)
        } else {
          row <- c(i,0)
        }
      } else if (groups_stp2 == "greaterThan") {
        if (diffVal > groups_stp3) {
          row <- c(i,1)
        } else {
          row <- c(i,0)
        }
      } else {
        if (diffVal == groups_stp3) {
          row <- c(i,1)
        } else {
          row <- c(i,0)
        }
      }
       
      #add participant to growing data table for outcomes
      tempTable <- rbindlist(list(tempTable, as.list(row)))
    }
    #edit outdata so the merge with attr data works..
    outData <- tempTable
    colnames(outData) <- c("Participant_Id", "GROUPS")
  } else if (groups_stp1 == "percentDays") {
    if (is.null(groups_stp4)) {
      return()
    }
    tempTable <- NULL
    #may be able to do this option with aggregate. look into it TODO
    prtcpnts <- levels(as.factor(outData$Participant_Id))
    for (i in prtcpnts) {
      currData <- subset(outData, outData$Participant_Id %in% i)
      
      if (groups_stp3 == "lessThan") {
        currData <- transform(currData, "GROUPS" =  ifelse(currData[[myGroups]] < groups_stp4,1 ,0))
      } else if (groups_stp3 == "greaterThan") {
        currData <- transform(currData, "GROUPS" =  ifelse(currData[[myGroups]] > groups_stp4,1 ,0))
      } else {
        currData <- transform(currData, "GROUPS" =  ifelse(currData[[myGroups]] == groups_stp4,1 ,0))
      }
      colnames(currData) <- c("Participant_Id", "drop", "GROUPS")
      if ((sum(currData$Outcome)/length(currData$Outcome)*100) >= groups_stp2) {
        row <- c(i,1)
      } else {
        row <- c(i,0)
      }
       
      tempTable <- rbindlist(list(tempTable, as.list(row)))
    }
    outData <- tempTable
    colnames(outData) <- c("Participant_Id", "GROUPS")
  }  else {
    mergeData <- NULL
    #for strings
    for (i in seq(length(groups_stp1))) {
      tempData <- aggregate(outData, by=list(outData$Participant_Id), FUN = function(x){ if(groups_stp1[[i]] %in% x) {1} else {0} })
      colnames(tempData) <- c("Participant_Id", "drop", "GROUPS")
      tempData$drop <- NULL
      if (is.null(mergeData)) {
        mergeData <- tempData
      } else {
        colnames(mergeData) <- c("Participant_Id", "PrevGroup")
        mergeData <- merge(mergeData, tempData, by = "Participant_Id")
        mergeData <- transform(mergeData, "GROUPS" = ifelse(PrevGroup == 1 | GROUPS == 1, 1, 0))
        mergeData$PrevGroup <- NULL 
      }
    }
    outData <- mergeData
  }

  if (ncol(outData) > 2) {
    colnames(outData) <- c("Participant_Id", "drop", "GROUPS")
  }
  
  outData <- as.data.table(outData)
  #dtop dtop
  if (any(colnames(outData) %in% "drop")) {
    outData$drop <- NULL
  }
  
  outData
}

allGroups <- function(outData, metadata.file, myGroups, groups_stp1, groups_stp2, groups_stp3, groups_stp4) {

  #this if statement will have to change. handle dates first
  if (any(c("POSIXct", "Date") %in% class(groups_stp1))) {
    outData <- transform(data, "GROUPS" = ifelse(data[[myGroups]] < groups_stp1[2] & data[[myGroups]] > groups_stp1[1], 1, 0))
    cols <- c("Participant_Id", "GROUPS")
    outData <- outData[, cols, with = FALSE]
    tempData <- aggregate(GROUPS ~ Participant_Id, outData, FUN = function(x){length(unique(x))})
    colnames(tempData) <- c("Participant_Id", "numLevels")
    tempData <- merge(outData, tempData, by = "Participant_Id")
    outData <- transform(tempData, "GROUPS" = ifelse(numLevels == 1, GROUPS, 0))
    outData <- outData[, cols, with = FALSE]
    outData <- unique(outData)
    message("custom groups for date")
  #for numbers
  } else if (groups_stp1 == "lessThan") {
    if (is.null(groups_stp2)) {
      return()
    }
    outData <- aggregate(outData, by=list(outData$Participant_Id), FUN = function(x){ if (all(x < as.numeric(groups_stp2))) {1} else {0} })
  } else if (groups_stp1 == "greaterThan") {
    if (is.null(groups_stp2)) {
      return()
    }
    outData <- aggregate(outData, by=list(outData$Participant_Id), FUN = function(x){ if (all(x > as.numeric(groups_stp2))) {1} else {0} })
  } else if (groups_stp1 == "equals") {
    if (is.null(groups_stp2)) {
      return()
    }
    outData <- aggregate(outData, by=list(outData$Participant_Id), FUN = function(x){ if (all(x == as.numeric(groups_stp2))) {1} else {0} })
  }  else {
    #for strings
    aggStr <- paste0(myGroups, " ~ Participant_Id")
    outData <- aggregate(as.formula(aggStr), outData, FUN = function(x){ ifelse(length(levels(as.factor(x))) == length(groups_stp1), all(sort(levels(as.factor(x))) == sort(groups_stp1)), FALSE) })
    colnames(outData) <- c("Participant_Id", "GROUPS")
    outData <- transform(outData, "GROUPS" = ifelse(GROUPS == TRUE, 1, 0))
  }

  if (ncol(outData) > 2) {
    colnames(outData) <- c("Participant_Id", "drop", "GROUPS")
  }
  
  outData <- as.data.table(outData)
  #dtop dtop
  if (any(colnames(outData) %in% "drop")) {
    outData$drop <- NULL
  }
  
  outData
}


#this make appropriate labels for groups created in function above    
makeGroupLabel <- function(myGroups, metadata.file, groups_stp1, groups_stp2, groups_stp3, groups_stp4, event.list){
  numeric <- c("lessThan", "greaterThan", "equals")
  anthro <- c("percentDays", "delta", "direct")
  label <- vector()
  obs <- c("any", "all")   
 
  obsFlag = ""
  if (groups_stp1 %in% obs) {
    if (groups_stp1 == "any") {
      obsFlag <- "Any"
    } else {
      obsFlag <- "All"
    }
    groups_stp1 = groups_stp2
    groups_stp2 = groups_stp3
  } 
 
  displayName <- metadata.file$property[metadata.file$source_id == myGroups]
  if (groups_stp1 %in% numeric ){
    if (groups_stp1 == "greaterThan") {
      label[1] <- paste0(obsFlag, " ", displayName, " > ", groups_stp2)
      label[2] <- paste0(obsFlag, " ", displayName, " <= ", groups_stp2)
    } else if (groups_stp1 == "lessThan") {
      label[1] <- paste0(obsFlag, " ", displayName, " < ", groups_stp2)
      label[2] <- paste0(obsFlag, " ", displayName, " >= ", groups_stp2)
    } else {
      label[1] <- paste0(obsFlag, " ", displayName, " = ", groups_stp2)
      label[2] <- paste0(obsFlag, " ", displayName, " != ", groups_stp2)
    }
  } else if (groups_stp1 %in% anthro) {
    if (groups_stp1 == "direct") {
      if (groups_stp2 == "lessThan") {
        label[1] <- paste0(displayName, " < ", groups_stp3)
        label[2] <- paste0(displayName, " >= ", groups_stp3)
      } else if (groups_stp2 == "greaterThan") {
        label[1] <- paste0(displayName, " > ", groups_stp3)
        label[2] <- paste0(displayName, " <= ", groups_stp3)
      } else {
        label[1] <- paste0(displayName, " = ", groups_stp3)
        label[2] <- paste0(displayName, " != ", groups_stp3)
      }
    } else if (groups_stp1 == "delta") {
      if (groups_stp2 == "lessThan") {
        label[1] <- paste0("Change in ", displayName, " over time < ", groups_stp3)
        label[2] <- paste0("Change in ", displayName, " over time >= ", groups_stp3)
      } else if (groups_stp2 == "greaterThan") {
        label[1] <- paste0("Change in ", displayName, " over time > ", groups_stp3)
        label[2] <- paste0("Change in ", displayName, " over time <= ", groups_stp3)
      } else {
        label[1] <- paste0("Change in ", displayName, " over time = ", groups_stp3)
        label[2] <- paste0("Change in ", displayName, " over time != ", groups_stp3)
      }
    } else {
      if (groups_stp3 == "lessThan") {
        label[1] <- paste0(displayName, " < ", groups_stp4, " for more than ", groups_stp1, "% of days monitored")
        label[2] <- paste0(displayName, " >= ", groups_stp4, " for more than ", groups_stp1, "% of days monitored")
      } else if (groups_stp3 == "greaterThan") {
        label[1] <- paste0(displayName, " > ", groups_stp4, " for more than ", groups_stp1, "% of days monitored")
        label[2] <- paste0(displayName, " <= ", groups_stp4, " for more than ", groups_stp1, "% of days monitored")
      } else {
        label[1] <- paste0(displayName, " = ", groups_stp4, " for more than ", groups_stp1, "% of days monitored")
        label[2] <- paste0(displayName, " != ", groups_stp4, " for more than ", groups_stp1, "% of days monitored")
      }
    }
  } else {
    if (!any(c("POSIXct", "Date") %in% class(groups_stp1))) {
      if (length(groups_stp1) == 1) { 
        label[1] <- paste(obsFlag, groups_stp1)
        if (label[1] == paste0(obsFlag, " Yes")) {
          label[2] <- "No"
        } else if (label[1] == paste0(obsFlag, " No")) {
          label[2] <- "Yes"
        } else if (label[1] == paste0(obsFlag, " True")) {
          label[2] <- "False"
        } else if (label[1] == paste0(obsFlag, " False")) {
          label[2] <- "True"
        } else {
            label[2] <- paste0("Not ", label[1])
        }
      } else {
        label[1] <- paste(obsFlag, "Observations Matching Selected Variables")
        label[2] <- paste("Not", obsFlag, "Observations Matching Selected Variables")
      }
    } else {
      label[1] <- paste0(obsFlag, " within date range")
      label[2] <- paste0(obsFlag, " outside date range")
    }
  }
  label
}
