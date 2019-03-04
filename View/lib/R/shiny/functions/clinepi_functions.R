### all clinepi specific functions b/c rely on data and/or metadata to be formatted consistently


longitudinalText <- function(mySubset = NULL, myTimeframe1 = NULL, myTimeframe2 = NULL) {
  subsetText <- ""
  for (i in seq(length(mySubset))) {
    subsetText <- paste0(subsetText,
    "current$subset\t", mySubset[i], "\n")
  }

  longitudinalText <- paste0(subsetText,
                             "current$range1[1]\t", myTimeframe1[1], "\n",
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

subsetDataFetcher <- function(keep = NULL, min = NULL, max = NULL, myData = NULL, col = NULL){
  if (is.null(keep)) {
    colMax <- max(myData[[col]], na.rm = TRUE)
    colMin <- min(myData[[col]], na.rm = TRUE)
  
    if (colMax != max | colMin != min) {
      #test this. dont understand how it didnt need a comma
      tempDF <- myData[myData[[col]] >= min & myData[[col]] <= max,] 
    } else {
      return(myData)
    }
  } else {
    tempDF <- myData[myData[[col]] %in% keep,]
  } 
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
  c("EUPATH_0000644", "EUPATH_0000702", "OBI_0100051")
}

  
getUIList <- function(metadata.file, minLevels = 1, maxLevels = Inf, include=c("all"), timepoints.keep = NULL) {
  drop <- getDropList()

  choices <- metadata.file[!metadata.file$source_id %in% drop,]
  #temporary until i can figure how to allow plotting of dates. 
  #will probably need to manually bin them in any place we would allowl plotly to automatically bin nums
  #then remember in those cases to set the labels accurately
  choices <- subset(choices, !type %in% "date")  


  #here remove from choices anything where category not in include param, unless param is 'all'
  #could alternatively assume if include is NULL to use everything
  
  if (length(include) > 1) {
    if (all(include != "all")) {
      include <- c(include, "", "null")
      choices <- choices[choices$category %in% include,]
    } else {
      stop("parameter 'include' of getUIList can either be 'all' or a character vector of categories..")
    }
  } else {
    if (include != "all") {
      include <- c(include, "", "null")
      choices <- choices[choices$category %in% include,]
    }
  }

  if (!is.null(timepoints.keep)) {
    if ("timepoints" %in% colnames(choices)) {
      leaves <- subset(choices, sapply(choices$timepoints, FUN = function(x){any(timepoints.keep %in% x)}))
    }
  }

  if (nrow(choices) == 0) {
    return()
  }

  network <- choices
  networkOther <- network[!network$type == "string",]
  networkString <- network[network$type == "string",]
  networkString$stdisable <- is.na(networkString$number_distinct_values) | networkString$number_distinct_values > maxLevels | networkString$number_distinct_values < minLevels
  networkOther$stdisable <- is.na(networkOther$number_distinct_values)
  network <- rbind(networkString, networkOther)

  network$leaf <- !(network$property %in% network$parentlabel)
  if (exists("leaves")) {
    network$leaf <- network$property %in% leaves$property 
  }  

  #i hate loops
  while (any(network$stdisable == TRUE & network$leaf == TRUE)) {
    remove <- network$stdisable == TRUE & network$leaf == TRUE
    network <- network[!remove,]
    network$leaf <- !(network$property %in% network$parentlabel)
  }
  disabled <- network$property[network$stdisable == TRUE]
  
  myCols <- c("property", "parentlabel")#, "stdisable")
  network <- as.data.frame(network[, myCols, with=FALSE])
  rootName <- unique(network[!(network$parentlabel %in% network$property),])
 
  
  tree <- FromDataFrameNetwork(network)
  list <- as.list(tree)
  list$name <- NULL
  
  #grr recursion
  list <- setAttrDisabled(disabled, list)
  
  list
} 

setAttrDisabled <- function(disabledNames, list) {
  if (length(list) == 0) {
    return("")
  }  
  for (name in names(list)) {
    if (name %in% disabledNames) {
      attr(list[[name]], "stdisabled") <- TRUE
    }
    disabledList <- setAttrDisabled(disabledNames, list[[name]])
    list[[name]] <- disabledList
  }
  list
}

getUIStp1List <- function(metadata.file, col){
  uniqueVals <- metadata.file$distinct_values[metadata.file$source_id == col]
  uniqueVals <- unlist(strsplit(uniqueVals, split="|", fixed = TRUE))

  uniqueVals
}
    
#seperate pipe delimited fields into their own rows if necessary
getFinalDT <- function(data, metadata.file, col){
     
  strings <- getStrings(metadata.file)
  if (col %in% strings$source_id) {
	
    if (any(grepl("|", data[[col]], fixed=TRUE))) {
      data <- separate_rows(data, col, sep = "[|]+")
    }
  }
       
  data
      
}
    
#this takes inputs passed to it and creates subsets of data based on those inputs
makeGroups <- function(data, metadata.file, myGroups, groups_stp1, groups_stp2, groups_stp3, groups_stp4, aggKey = c("Participant_Id")){
  #realistically should check for nulls before calling, but just in case
  if (is.null(myGroups)) {
    return()
  }
  if (length(myGroups) == 0) {
    return()
  }
  if (is.null(groups_stp1)) {
    message("groups stp1 is null!! returning")
    return()
  }
  #get group data for make groups option
  groupData <- completeDT(data, myGroups)
  groupData <- getFinalDT(groupData, metadata.file, myGroups)
  #myCols <- c("Participant_Id", myGroups)
  myCols <- c(aggKey, myGroups)
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

  #call either the any/ever or the all/always function
  if (obsFlag != "all") {
    #aggKey only necessary for obsToggle and will always run this through any
    outData <- anyGroups(outData, metadata.file, myGroups, groups_stp1, groups_stp2, groups_stp3, groups_stp4, aggKey)
  } else {
    outData <- allGroups(outData, metadata.file, myGroups, groups_stp1, groups_stp2, groups_stp3, groups_stp4)
  }
  
  outData
}

anyGroups <- function(outData, metadata.file, myGroups, groups_stp1, groups_stp2, groups_stp3, groups_stp4, aggKey) {
  aggStr <- paste(myGroups, "~", paste(aggKey, collapse=" + "))
  #this if statement will have to change. handle dates first
  if (any(c("POSIXct", "Date") %in% class(groups_stp1))) {
    outData <- transform(data, "GROUPS" = ifelse(data[[myGroups]] < groups_stp1[2] & data[[myGroups]] > groups_stp1[1], 1, 0))
    #cols <- c("Participant_Id", "GROUPS")
    cols <- c(aggKey, "GROUPS")
    outData <- outData[, cols, with = FALSE]
    outData <- unique(outData)
  #for numbers
  } else if (groups_stp1 == "lessThan") {
    if (is.null(groups_stp2)) {
      return()
    }
    if (!is.numeric(groups_stp2)) { 
      return()
    }
    outData <- aggregate(as.formula(aggStr), outData, FUN = function(x){ if (any(x < as.numeric(groups_stp2))) {1} else {0} })
  } else if (groups_stp1 == "greaterThan") {
    if (is.null(groups_stp2)) {
      return()
    }
    if (!is.numeric(groups_stp2)) {
      return()
    }
    outData <- aggregate(as.formula(aggStr), outData, FUN = function(x){ if (any(x > as.numeric(groups_stp2))) {1} else {0} })
  } else if (groups_stp1 == "equals") {
    if (is.null(groups_stp2)) {
      return()
    }
    if (!is.numeric(groups_stp2)) {
      return()
    }
    outData <- aggregate(as.formula(aggStr), outData, FUN = function(x){ if (any(x == as.numeric(groups_stp2))) {1} else {0} })
    #for change over time, maled  
  } else if (groups_stp1 == "delta") {
    if(is.null(groups_stp3)) {
      return()
    }
    outData <- completeDT(data, myGroups)
    outData <- getFinalDT(outData, metadata.file, myGroups)
    myCols <- c("Participant_Id", "EUPATH_0000644", myGroups)
    outData <- outData[, myCols, with=FALSE]
    
    tempTable <- NULL
   
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
    #also maled, but wonder if this could be generalized
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
      tempData <- aggregate(as.formula(aggStr), outData, FUN = function(x){ if(groups_stp1[[i]] %in% x) {1} else {0} })
      colnames(tempData) <- c(aggKey, "GROUPS")
      tempData$drop <- NULL
      if (is.null(mergeData)) {
        mergeData <- tempData
      } else {
        colnames(mergeData) <- c(aggKey, "PrevGroup")
        mergeData <- merge(mergeData, tempData, by = aggKey)
        mergeData <- transform(mergeData, "GROUPS" = ifelse(PrevGroup == 1 | GROUPS == 1, 1, 0))
        mergeData$PrevGroup <- NULL 
      }
    }
    outData <- mergeData
  }

  if (ncol(outData) > 2) {
    colnames(outData) <- c(aggKey, "GROUPS")
  }
  
  outData <- as.data.table(outData)
  
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
makeGroupLabel <- function(myGroups, metadata.file, groups_stp1, groups_stp2, groups_stp3, groups_stp4, event.list, useGroup = FALSE){
  numeric <- c("lessThan", "greaterThan", "equals")
  anthro <- c("percentDays", "delta", "direct")
  label <- vector()
  obs <- c("any", "all")   
 
  obsFlag = ""
  if (!is.null(groups_stp1)) {
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
    if (!is.null(groups_stp1)) {
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
            for (i in groups_stp1) {
              if (length(label) == 0) {
                label[1] <- paste(obsFlag, "Observations Matching:", i)
                label[2] <- paste("Not", obsFlag, "Observations Matching:", i)
              } else {
                label[1] <- paste0(label[1], ", ", i)
                label[2] <- paste0(label[2], ", ", i)
              }
            }
          }
        } else {
          label[1] <- paste0(obsFlag, " observations between ", groups_stp1[1], " and ", groups_stp1[2])
          label[2] <- paste0("Not ", obsFlag, " observations between ", groups_stp1[1], " and ", groups_stp1[2])
        }
      }   
 
        if (useGroup) {
          prefix <- metadata.file$property[metadata.file$source_id == myGroups]
          label[1] <- paste0(prefix, ": ", label[1])
          label[2] <- paste0(prefix, ": ", label[2])
        }
      } else {
        label <- metadata.file$property[metadata.file$source_id == myGroups]
      }
    } else {
      label <- metadata.file$property[metadata.file$source_id == myGroups]
    }

  label
}