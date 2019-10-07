getNamedQueryResult <- function(con, queryName, tblPrefix, sourceId, timeSourceId = "none") {
  if (is.null(queryName)) { return() }
  if (is.null(tblPrefix)) { return() }

  if (!is.null(sourceId)) {
    if (sourceId == timeSourceId) { timeSourceId <- "none" }
  }

  message(Sys.time(), "functions/clinepi_functions.R:  Running query: ", queryName, " with sourceId: ", sourceId, " and timeSourceId: ", timeSourceId, " for dataset: ", tblPrefix)

  if (queryName == "Participant") {
    if (timeSourceId == "none") {
      query <- paste0("select pa.name as Participant_Id",
                            ", pa.", sourceId,
                     " from apidbtuning.", tblPrefix, "Participants pa",
                     " where pa.", sourceId, " is not null")
    } else {
      query <- paste0("select pa.name as participant_id",
                           ", p.string_value as ", sourceId,
                           ", o.string_value as ", timeSourceId,
                     " from apidbtuning.", tblPrefix, "ParticipantMD p",
                         ", apidbtuning.", tblPrefix, "ObservationMD o",
                         ", apidbtuning.", tblPrefix, "Participants pa",
                     " where p.ontology_term_name = '", sourceId, "'",
                     " and o.ontology_term_name = '", timeSourceId, "'",
                     " and p.participant_id = o.participant_id",
                     " and p.participant_id = pa.pan_id")
    }
  } else if (queryName == "Household") {
    if (timeSourceId == "none") {
      query <- paste0("select pa.name as Participant_Id",
                            ", h.string_value as ", sourceId,
                     " from apidbtuning.", tblPrefix, "HouseholdMD h",
                         ", apidbtuning.", tblPrefix, "HousePartIO hp",
                         ", apidbtuning.", tblPrefix, "Participants pa",
                     " where h.ontology_term_name = '", sourceId, "'",
                     " and hp.household_id = h.household_id",
                     " and hp.participant_id = pa.pan_id")
    } else{
      query <- paste0("with termtime as (",
                       " select h1.household_id",
                             ", h1.string_value as ", sourceId,
                             ", h2.string_value as ", timeSourceId,
                       " from apidbTuning.", tblPrefix, "HouseholdMD h1",
                           ", apidbTuning.", tblPrefix, "HouseholdMD h2",
                       " where h1.ontology_term_name = '", sourceId, "'",
                       " and h2.ontology_term_name = '", timeSourceId, "'",
                       " and h1.household_id = h2.household_id",
                       " and exists (select ontology_term_name",
                                   " from apidbTuning.", tblPrefix, "HouseholdMD",
                                   " where ontology_term_name = '", sourceId, "'",
                                   " and household_id = household_observation_id)",
                       " UNION ALL",
                       " select h1.household_id",
                             ", h1.string_value as ", sourceId,
                             ", h2.string_value as ", timeSourceId,
                       " from apidbTuning.", tblPrefix, "HouseholdMD h1",
                           ", apidbTuning.", tblPrefix, "HouseholdMD h2",
                       " where h1.ontology_term_name = '", sourceId, "'",
                       " and h2.ontology_term_name = '", timeSourceId, "'",
                       " and h1.household_observation_id = h2.household_observation_id",
                       " and not exists (select ontology_term_name",
                                       " from apidbTuning.", tblPrefix, "HouseholdMD",
                                       " where ontology_term_name = '", sourceId, "'",
                                       " and household_id = household_observation_id)",
                     ")",
                    " select p.name as participant_id",
                          ", termtime.", sourceId,
                          ", termtime.", timeSourceId,
                    " from termtime",
                        ", apidbTuning.", tblPrefix, "HousePartIO hp",
                        ", apidbTuning.", tblPrefix, "Participants p",
                    " where termtime.household_id = hp.household_id",
                    " and hp.participant_id = p.pan_id")
    }
  } else if (queryName == "Observation") {
    if (timeSourceId == "none") {
      query <- paste0("select pa.name as participant_id",
                           ", string_value as ", sourceId,
                     " from apidbtuning.", tblPrefix, "ObservationMD o",
                         ", apidbtuning.", tblPrefix, "Participants pa",
                     " where o.ontology_term_name = '", sourceId, "'",
                     " and o.participant_id = pa.pan_id")
    } else {
      query <- paste0("with termtime as (",
                       " select o1.observation_id",
                             ", o1.string_value as ", sourceId,
                             ", o2.string_value as ", timeSourceId,
                       " from apidbTuning.", tblPrefix, "ObservationMD o1",
                           ", apidbTuning.", tblPrefix, "ObservationMD o2",
                       " where o1.ontology_term_name = '", sourceId, "'",
                       " and o2.ontology_term_name = '", timeSourceId, "'",
                       " and o1.observation_id = o2.observation_id",
                       " and exists (select ontology_term_name",
                                   " from apidbTuning.", tblPrefix, "ObservationMD",
                                   " where ontology_term_name = '", timeSourceId, "'",
                                   " and observation_id = sub_observation_id)",
                       " UNION ALL",
                       " select o1.observation_id",
                             ", o1.string_value as ", sourceId,
                             ", o2.string_value as ", timeSourceId,
                       " from apidbTuning.", tblPrefix, "ObservationMD o1",
                           ", apidbTuning.", tblPrefix, "ObservationMD o2",
                       " where o1.ontology_term_name = '", sourceId, "'",
                       " and o2.ontology_term_name = '", timeSourceId, "'",
                       " and o1.sub_observation_id = o2.sub_observation_id",
                       " and not exists (select ontology_term_name",
                                       " from apidbTuning.", tblPrefix, "ObservationMD",
                                       " where ontology_term_name = '", timeSourceId, "'",
                                       " and observation_id = sub_observation_id)",
                     ")",
                    " select p.name as participant_id",
                          ", termtime.", sourceId,
                          ", termtime.", timeSourceId,
                    " from termtime",
                        ", apidbTuning.", tblPrefix, "PartObsIO po",
                        ", apidbTuning.", tblPrefix, "Participants p",
                    " where termtime.observation_id = po.observation_id",
                    " and po.participant_id = p.pan_id")
    }
  } else if (queryName == "ObservationNames") {
    if (timeSourceId == "none") {
      query <- paste0("select pa.name as participant_id",
                           ", oa.name as observation_id",
                     " from apidbtuning.", tblPrefix, "Observations oa",
                         ", apidbtuning.", tblPrefix, "Participants pa",
                         ", apidbtuning.", tblPrefix, "PartObsIO po",
                     " where oa.pan_id = po.observation_id",
                     " and po.participant_id = pa.pan_id")
    } else{
      query <- paste0("select pa.name as participant_id",
                           ", oa.name as observation_id",
                           ", oa.", timeSourceId,
                     " from apidbtuning.", tblPrefix, "Observations oa",
                         ", apidbtuning.", tblPrefix, "Participants pa",
                         ", apidbtuning.", tblPrefix, "PartObsIO po",
                     " where oa.pan_id = po.observation_id",
                     " and po.participant_id = pa.pan_id")
    } 
  } else if (queryName == "Sample") {
    if (timeSourceId == "none") {
      query <- paste0("select pa.name as Participant_Id, sa.", sourceId,
                     " from apidbtuning.", tblPrefix, "Participants pa",
                         ", apidbtuning.", tblPrefix, "Samples sa ",
                         ", apidbtuning.", tblPrefix, "PartObsIO io",
                         ", apidbtuning.", tblPrefix, "ObsSampleIO io2",
                     " where pa.pan_id = io.participant_id",
                     " and io.observation_id = io2.observation_id",
                     " and io2.sample_id = sa.pan_id",
                     " and sa.", sourceId, " is not null")
    } else{
      query <- paste0("select pa.name as Participant_Id",
                           ", sa.", sourceId,
                           ", oa.", timeSourceId,
                     " from apidbtuning.", tblPrefix, "Participants pa",
                         ", apidbtuning.", tblPrefix, "Observations oa",
                         ", apidbtuning.", tblPrefix, "Samples sa ",
                         ", apidbtuning.", tblPrefix, "PartObsIO io",
                         ", apidbtuning.", tblPrefix, "ObsSampleIO io2",
                     " where pa.pan_id = io.participant_id",
                     " and io.observation_id = oa.pan_id",
                     " and io.observation_id = io2.observation_id",
                     " and io2.sample_id = sa.pan_id",
                     " and sa.", sourceId, " is not null")
    }
  } else {
    warning(Sys.time(), "Query name not recognized: ", queryName)
  }

  dt <- as.data.table(dbGetQuery(con, query))
  
  dt
}

#TODO revisit this name later
queryTermData <- function(con, myVar, attributes.file, datasetDigest, metadata.file, longitudinal1, longitudinal2, lon2Data, lon1Data, hlongitudinal1, hlongitudinal2, hlon2Data, hlon1Data) {

  if (is.null(myVar)) { return() }
  if (length(myVar) == 0) { return() } 
  if (is.na(myVar)) { return() }
  if (myVar == "none") { return() } 

  category <- metadata.file$CATEGORY[metadata.file$SOURCE_ID == myVar]
  if (is.null(category)) { return() }
  if (length(category) == 0) { return() }
  if (is.na(category)) { return() }

  if (!is.null(longitudinal2)) {
    if (myVar == longitudinal2) { return(lon2Data) }
  }
  if (!is.null(hlongitudinal2)) {
    if (myVar == hlongitudinal2) { return(hlon2Data) }
  }

  if (myVar != "custom") {
    if (!is.null(longitudinal1)) {
      if (category != "Household") {
        data <- getNamedQueryResult(con, category, datasetDigest, myVar, longitudinal1)
        if (is.null(data)) { return() }
        data <- data[, PARTICIPANT_ID:=as.character(PARTICIPANT_ID)]
        data <- setDTColType(longitudinal1, metadata.file, data)
	if (category != "Participant") {
          if (!is.null(lon2Data)) {
            data <- merge(data, lon2Data, by = c("PARTICIPANT_ID", longitudinal1), all = TRUE)
          } else {
            data <- merge(data, lon1Data, by = c("PARTICIPANT_ID", longitudinal1), all = TRUE)
          }
        } else {
	  data[[longitudinal1]] <- NULL
	  data <- unique(data)
	  if (!is.null(lon2Data)) {
            data <- merge(data, lon2Data, by = c("PARTICIPANT_ID"), all = TRUE)
          } else {
            data <- merge(data, lon1Data, by = c("PARTICIPANT_ID"), all = TRUE)
          }
        }
      } else {
        if (!is.null(hlongitudinal1)) {
          data <- getNamedQueryResult(con, category, datasetDigest, myVar, hlongitudinal1)
          if (is.null(data)) { return() }
          data <- data[, PARTICIPANT_ID:=as.character(PARTICIPANT_ID)]
          data <- setDTColType(hlongitudinal1, metadata.file, data)
          if (!is.null(hlon2Data)) {
            data <- merge(data, hlon2Data, by = c("PARTICIPANT_ID", hlongitudinal1))
            names(data)[names(data) == hlongitudinal2] <- longitudinal2
            names(data)[names(data) == hlongitudinal1] <- longitudinal1
          } else {
            data <- merge(data, hlon1Data, by = c("PARTICIPANT_ID", hlongitudinal1))
            names(data)[names(data) == hlongitudinal1] <- longitudinal1
          }
          names(data)[names(data) == hlongitudinal1] <- longitudinal1
	  if (myVar == hlongitudinal1) { myVar <- longitudinal1 }

        # assuming if we dont have a household longitudinal source id that households data is static
	# if household table has a column for date/age include it in longitudinal.tab even if its identical to the obs source id
        } else {
          data <- getNamedQueryResult(con, category, datasetDigest, myVar)
          if (is.null(data)) { return() }
          data <- data[, PARTICIPANT_ID:=as.character(PARTICIPANT_ID)]
          if (!is.null(lon2Data)) {
             data <- merge(data, lon2Data, by = c("PARTICIPANT_ID"), all = TRUE)
          } else {
            data <- merge(data, lon1Data, by = c("PARTICIPANT_ID"), all = TRUE)
          }
        }
      }
    } else {
      data <- getNamedQueryResult(con, category, datasetDigest, myVar)
      if (is.null(data)) { return() }
    }
  } else {
    message(Sys.time(), " Using strategy results, no query necessary")
    if (category == "Participant") {
      if (!is.null(longitudinal1)) {
        if (!is.null(lon2Data)) {
          data <- merge(lon2Data, attributes.file, by = "PARTICIPANT_ID", all = TRUE) 
        } else {
          data <- merge(lon1Data, attributes.file, by = "PARTICIPANT_ID", all = TRUE) 
        }
      } else {
        data <- getNamedQueryResult(con, category, datasetDigest, "NAME")
        if (is.null(data)) { return() }
        data <- data[, PARTICIPANT_ID:=as.character(PARTICIPANT_ID)]
        data <- merge(data, attributes.file, by = "PARTICIPANT_ID", all = TRUE) 
      }
      naToNotSelected(data, col = "custom")
    } else if (category == "Observation") {
      data <- getNamedQueryResult(con, "ObservationNames", datasetDigest, NULL, longitudinal1)
      if (is.null(data)) { return() }
      data <- merge(data, attributes.file, by = "OBSERVATION_ID", all=TRUE)
      if (!is.null(longitudinal1)) {
        data <- setDTColType(longitudinal1, metadata.file, data)
        if (!is.null(lon2Data)) {
          data <- merge(data, lon2Data, by = c("PARTICIPANT_ID", longitudinal1), all = TRUE)
        } else {
          data <- merge(data, lon1Data, by = c("PARTICIPANT_ID", longitudinal1), all = TRUE)
        }
      }
      naToNotSelected(data, col = "custom")
    }
  }

  data <- data[, PARTICIPANT_ID:=as.character(PARTICIPANT_ID)]
  data <- setDTColType(myVar, metadata.file, data)
  if (!is.null(longitudinal1)) {
    if (longitudinal1 %in% colnames(data)) {
      data <- setDTColType(longitudinal1, metadata.file, data)
    }
  }
  if (!is.null(longitudinal2)) {
    if (longitudinal2 %in% colnames(data)) {
      data <- setDTColType(longitudinal2, metadata.file, data)
    }
  }
 
  message(Sys.time(), " Returning query data")
  data
}

setDTColType <- function(myVar, metadata.file, data) {
  colType <- unique(metadata.file$TYPE[metadata.file$SOURCE_ID == myVar])
  if (colType == "string") {
    data <- data[, (myVar):=as.character(get(myVar))]
  } else if (colType == "number") {
    data <- data[, (myVar):=as.numeric(get(myVar))]
  } else {
    data <- suppressWarnings(separate(data, myVar, c(myVar, "drop"), "[[:blank:]]"))
    data$drop <- NULL
    data <- data[, (myVar):=as.Date(get(myVar), format = "%Y-%m-%d")]
  }

  data
} 

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
  nums <- subset(metadata.file, metadata.file$TYPE == "number", "SOURCE_ID") 
   
  nums
}
    
getStrings <- function(metadata.file){
  #identify strings 
  strings <- subset(metadata.file, metadata.file$TYPE == "string", "SOURCE_ID") 
      
  strings
}
    
getDates <- function(metadata.file){
  #identify dates 
  dates <- subset(metadata.file, metadata.file$TYPE == "date", "SOURCE_ID") 
      
  dates
}

#this just a list of clinepi source_ids we want to make sure never appear in ui    
getDropList <- function(){
  c("EUPATH_0000644", "EUPATH_0000702", "OBI_0100051")
}

  
getUIList <- function(metadata.file, minLevels = 1, maxLevels = Inf, include = NULL, timepoints.keep = NULL) {

  if (is.null(include)) { return() }
 
  drop <- getDropList()
  choices <- metadata.file[!metadata.file$SOURCE_ID %in% drop,]
  #temporary until i can figure how to allow plotting of dates. 
  #will probably need to manually bin them in any place we would allowl plotly to automatically bin nums
  #then remember in those cases to set the labels accurately
  choices <- subset(choices, !TYPE %in% "date")  

  #here remove from choices anything where category not in include param, unless param is 'all'
  if (length(include) > 1) {
    if (all(include != "all")) {
      include <- c(include, "", "null")
      choices <- choices[choices$CATEGORY %in% include,]
    } else {
      stop("parameter 'include' of getUIList can either be 'all' or a character vector of categories..")
    }
  } else {
    if (include != "all") {
      include <- c(include, "", "null")
      choices <- choices[choices$CATEGORY %in% include,]
    }
  }

  #if (!is.null(timepoints.keep)) {
  #  if ("timepoints" %in% colnames(choices)) {
  #    leaves <- subset(choices, sapply(choices$timepoints, FUN = function(x){any(timepoints.keep %in% x)}))
  #  }
  #}

  if (nrow(choices) == 0) {
    return()
  }

  network <- choices
  networkOther <- network[!network$TYPE == "string",]
  networkString <- network[network$TYPE == "string",]
  networkString$stdisable <- is.na(networkString$NUMBER_DISTINCT_VALUES) | as.numeric(networkString$NUMBER_DISTINCT_VALUES) > maxLevels | as.numeric(networkString$NUMBER_DISTINCT_VALUES) < minLevels | networkString$NUMBER_DISTINCT_VALUES == "null"
  networkOther$stdisable <- is.na(networkOther$NUMBER_DISTINCT_VALUES) | networkOther$NUMBER_DISTINCT_VALUES == "null"
  network <- rbind(networkString, networkOther)

  network$leaf <- !(network$PROPERTY %in% network$PARENTLABEL)
  if (exists("leaves")) {
    network$leaf <- network$PROPERTY %in% leaves$PROPERTY 
  }  

  #i hate loops
  while (any(network$stdisable == TRUE & network$leaf == TRUE)) {
    remove <- network$stdisable == TRUE & network$leaf == TRUE
    network <- network[!remove,]
    network$leaf <- !(network$PROPERTY %in% network$PARENTLABEL)
  }
  disabled <- network$PROPERTY[network$stdisable == TRUE]
  
  myCols <- c("PROPERTY", "PARENTLABEL")#, "stdisable")
  network <- as.data.frame(network[, myCols, with=FALSE])
  rootName <- unique(network[!(network$PARENTLABEL %in% network$PROPERTY),])
 
  tree <- FromDataFrameNetwork(network)
  list <- as.list(tree)
  list$name <- NULL
  
  #grrr recursion
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
  uniqueVals <- metadata.file$DISTINCT_VALUES[metadata.file$SOURCE_ID == col]
  uniqueVals <- unlist(strsplit(uniqueVals, split="|", fixed = TRUE))

  uniqueVals
}
    
#seperate pipe delimited fields into their own rows if necessary
getFinalDT <- function(data, metadata.file, col){
     
  strings <- getStrings(metadata.file)
  if (col %in% strings$SOURCE_ID) {
	
    if (any(grepl("|", data[[col]], fixed=TRUE))) {
      data <- separate_rows(data, col, sep = "[|]+")
    }
  }
       
  data
      
}
    
#this takes inputs passed to it and creates subsets of data based on those inputs
makeGroups <- function(data, metadata.file, myGroups, groups_stp1, groups_stp2, groups_stp3, groups_stp4, aggKey = c("PARTICIPANT_ID")){
  #realistically should check for nulls before calling, but just in case
  if (is.null(myGroups)) {
    return()
  }
  if (length(myGroups) == 0) {
    return()
  }
  if (is.null(groups_stp1)) {
    return()
  }
  #get group data for make groups option
  groupData <- completeDT(data, myGroups)
  groupData <- getFinalDT(groupData, metadata.file, myGroups)
  #myCols <- c("PARTICIPANT_ID", myGroups)
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

myAnyStringGroups <- function(groups_stp1 = NULL, outData = NULL, aggStr = NULL, aggKey = NULL) {
  if (is.null(groups_stp1)) { return() }
  if (is.null(outData)) { return() }
  if (is.null(aggStr)) { return() }
  if (is.null(aggKey)) { return() }
  
  tempData <- aggregate(as.formula(aggStr), outData, FUN = function(x){ if(groups_stp1 %in% x) {1} else {0} })
  colnames(tempData) <- c(aggKey, "GROUPS")
  tempData$drop <- NULL
  
  tempData
}

anyGroups <- function(outData, metadata.file, myGroups, groups_stp1, groups_stp2, groups_stp3, groups_stp4, aggKey) {
  aggStr <- paste(myGroups, "~", paste(aggKey, collapse=" + "))
  if (any(c("POSIXct", "Date") %in% class(groups_stp1))) {
    outData <- transform(data, "GROUPS" = ifelse(data[[myGroups]] < groups_stp1[2] & data[[myGroups]] > groups_stp1[1], 1, 0))
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
    outData <- unique(outData[,list(GROUPS = (get(myGroups) < as.numeric(groups_stp2))), by = aggKey])
    outData$GROUPS[is.na(outData$GROUPS)] = 0
  } else if (groups_stp1 == "greaterThan") {
    if (is.null(groups_stp2)) {
      return()
    }
    if (!is.numeric(groups_stp2)) {
      return()
    }
    outData <- unique(outData[,list(GROUPS = (get(myGroups) > as.numeric(groups_stp2))), by = aggKey])
    outData$GROUPS[is.na(outData$GROUPS)] = 0
  } else if (groups_stp1 == "equals") {
    if (is.null(groups_stp2)) {
      return()
    }
    if (!is.numeric(groups_stp2)) {
      return()
    }
    outData <- unique(outData[,list(GROUPS = (get(myGroups) == as.numeric(groups_stp2))), by = aggKey])
    outData$GROUPS[is.na(outData$GROUPS)] = 0
  }  else {
    mergeData <- NULL
    #for strings
    
    for (i in seq(length(groups_stp1))) {
      tempData <- outData[,list(GROUPS = (any(get(myGroups) == groups_stp1[[i]]))), by = aggKey]
      tempData$GROUPS[is.na(tempData$GROUPS)] <- 0
      tempData <- unique(tempData)
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

  colnames(outData) <- c(aggKey, "GROUPS")
  outData <- as.data.table(outData)
  
  if ("drop" %in% colnames(outData)) {
    outData$drop <- NULL
  }
  
  outData
}

allGroups <- function(outData, metadata.file, myGroups, groups_stp1, groups_stp2, groups_stp3, groups_stp4) {

  #this if statement will have to change. handle dates first
  if (any(c("POSIXct", "Date") %in% class(groups_stp1))) {
    outData <- transform(data, "GROUPS" = ifelse(data[[myGroups]] < groups_stp1[2] & data[[myGroups]] > groups_stp1[1], 1, 0))
    cols <- c("PARTICIPANT_ID", "GROUPS")
    outData <- outData[, cols, with = FALSE]
    tempData <- aggregate(GROUPS ~ PARTICIPANT_ID, outData, FUN = function(x){length(unique(x))})
    colnames(tempData) <- c("PARTICIPANT_ID", "numLevels")
    tempData <- merge(outData, tempData, by = "PARTICIPANT_ID")
    outData <- transform(tempData, "GROUPS" = ifelse(numLevels == 1, GROUPS, 0))
    outData <- outData[, cols, with = FALSE]
    outData <- unique(outData)
  #for numbers
  } else if (groups_stp1 == "lessThan") {
    if (is.null(groups_stp2)) {
      return()
    }
    #outData <- aggregate(outData, by=list(outData$PARTICIPANT_ID), FUN = function(x){ if (all(x < as.numeric(groups_stp2))) {1} else {0} })
    outData <- unique(outData[,list(GROUPS = (get(myGroups) < as.numeric(groups_stp2))), by = 'PARTICIPANT_ID'])
    outData$GROUPS[is.na(outData$GROUPS)] = 0
  } else if (groups_stp1 == "greaterThan") {
    if (is.null(groups_stp2)) {
      return()
    }
    #outData <- aggregate(outData, by=list(outData$PARTICIPANT_ID), FUN = function(x){ if (all(x > as.numeric(groups_stp2))) {1} else {0} })
    outData <- unique(outData[,list(GROUPS = (get(myGroups) > as.numeric(groups_stp2))), by = 'PARTICIPANT_ID'])
    outData$GROUPS[is.na(outData$GROUPS)] = 0
  } else if (groups_stp1 == "equals") {
    if (is.null(groups_stp2)) {
      return()
    }
    #outData <- aggregate(outData, by=list(outData$PARTICIPANT_ID), FUN = function(x){ if (all(x == as.numeric(groups_stp2))) {1} else {0} })
    outData <- unique(outData[,list(GROUPS = (get(myGroups) == as.numeric(groups_stp2))), by = 'PARTICIPANT_ID'])
    outData$GROUPS[is.na(outData$GROUPS)] = 0
  }  else {
    #for strings
    aggStr <- paste0(myGroups, " ~ PARTICIPANT_ID")
    #outData <- aggregate(as.formula(aggStr), outData, FUN = function(x){ ifelse(length(levels(as.factor(x))) == length(groups_stp1), all(sort(levels(as.factor(x))) == sort(groups_stp1)), FALSE) })
    outData <- unique(outData[,list(GROUPS = all(sort(unique(get(myGroups))) == sort(groups_stp1))), by = 'PARTICIPANT_ID'])
    outData$GROUPS[is.na(outData$GROUPS)] = 0
    colnames(outData) <- c("PARTICIPANT_ID", "GROUPS")
    outData <- transform(outData, "GROUPS" = ifelse(GROUPS == TRUE, 1, 0))
  }

  if (ncol(outData) > 2) {
    colnames(outData) <- c("PARTICIPANT_ID", "drop", "GROUPS")
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
    
    displayName <- metadata.file$PROPERTY[metadata.file$SOURCE_ID == myGroups]
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
          prefix <- metadata.file$PROPERTY[metadata.file$SOURCE_ID == myGroups]
          label[1] <- paste0(prefix, ": ", label[1])
          label[2] <- paste0(prefix, ": ", label[2])
        }
      } else {
        label <- metadata.file$PROPERTY[metadata.file$SOURCE_ID == myGroups]
      }
    } else {
      label <- metadata.file$PROPERTY[metadata.file$SOURCE_ID == myGroups]
    }

  label
}
