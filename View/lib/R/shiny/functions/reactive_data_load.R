reactiveDataFetcher = reactive({

   if (is.null(datasetName)) {
     custom.props <- try(fread(
        getWdkDatasetFile('customProps.txt', session, FALSE, dataStorageDir)))
     datasetName <<- colnames(custom.props)

     if (datasetName %in% names(datasetList)) {
       studyData <<- datasetList[[datasetName]]
       metadata.file <<- metadataList[[datasetName]]
     } else {
       model.prop <- fread(getWdkDatasetFile('model.prop', session, FALSE, dataStorageDir),
                           sep="=", header=FALSE, fill=TRUE)
       mirror.dir <- paste0(model.prop$V2[model.prop$V1 == "WEBSERVICEMIRROR"], "ClinEpiDB")
       num <- paste0("build-", model.prop$V2[model.prop$V1 == 'buildNumber'])
       mirror.dir <- paste0(mirror.dir, "/", num, "/", datasetName, "/shiny/")
       studyData <<- fread(paste0(mirror.dir, "shiny_masterDataTable.txt"), na.strings = c('NA', 'null', 'N/A', 'na', '')) 
       datasetList[[datasetName]] <- studyData
       metadata.file <<- fread(paste0(mirror.dir, "ontologyMetadata.tab"))
       metadataList[[datasetName]] <- metadata.file
       assign("datasetList", datasetList, .GlobalEnv)
       assign("metadataList", metadataList, .GlobalEnv)
     }

    if (grepl("GEMS", datasetName)) {
      obs <- studyData[!is.na(studyData$BFO_0000015),]
      obs <- obs[,which(unlist(lapply(obs, function(x)!all(is.na(x))))),with=F]
      houseObs <- studyData[is.na(studyData$BFO_0000015),]
      houseObs$BFO_0000015 <- houseObs$EUPATH_0015467
      houseObs <- houseObs[,which(unlist(lapply(houseObs, function(x)!all(is.na(x))))),with=F]
      myCols <- colnames(obs)[colnames(obs) %in% colnames(houseObs) & !colnames(obs) %in% c("Participant_Id", "BFO_0000015")]
      houseObs <- houseObs[, !myCols, with=FALSE]
      myStaticCols <- c("Participant_Id", "OBI_0001627")
      static <- houseObs[, myStaticCols, with=FALSE]
      static <- unique(static[!is.na(static$OBI_0001627),])
      houseObs <- houseObs[, !c("OBI_0001627"), with=FALSE]
      houseObs <- merge(static, houseObs, by = "Participant_Id")
      studyData <<- merge(obs, houseObs, by = c("Participant_Id", "BFO_0000015"))
      metadata.file$distinct_values[metadata.file$source_id == 'BFO_0000015'] <<- "60 day follow-up|Enrollment"
      temp <- sapply(metadata.file$source_id[metadata.file$source_id %in% colnames(studyData)], FUN = function(x){unique(studyData$BFO_0000015[!is.na(studyData[[x]])])})
      temp <- data.table(timepoints = temp, source_id = names(temp))
      metadata.file <<- merge(metadata.file, temp, by = "source_id", all=TRUE)
    }

    if (grepl("India", datasetName) & !(grepl("fever", datasetName))) {
      obs <- studyData[!is.na(studyData$EUPATH_0000091),]
      obs <- obs[,which(unlist(lapply(obs, function(x)!all(is.na(x))))),with=F]
      houseObs <- studyData[is.na(studyData$EUPATH_0000091),]
      houseObs <- houseObs[,which(unlist(lapply(houseObs, function(x)!all(is.na(x))))),with=F]
      myCols <- colnames(obs)[colnames(obs) %in% colnames(houseObs) & !colnames(obs) %in% c("Participant_Id", "BFO_0000015", "EUPATH_0000091")]
      houseObs <- houseObs[, !myCols, with=FALSE]
      myCols <- c("EUPATH_0021070", "EUPATH_0021071", "EUPATH_0021072", "EUPATH_0021073")	
      visit1 <- houseObs[, c("Participant_Id", myCols), with=FALSE]
      visit1 <- unique(visit1[!is.na(visit1$EUPATH_0021070),])
      enrollment <- houseObs[, !myCols, with=FALSE]
      enrollment <- unique(enrollment[!is.na(enrollment$EUPATH_0021095),])
      houseObs <- merge(visit1, enrollment, by = "Participant_Id")
      studyData <<- merge(obs, houseObs, by = "Participant_Id")
      metadata.file <<- metadata.file[metadata.file$source_id %in% colnames(studyData) | metadata.file$property %in% metadata.file$parentlabel,]
    }



    if (grepl("PRISM", datasetName)) {
      obs <- studyData[is.na(studyData$EUPATH_0000054),]
      houseObs <- studyData[!is.na(studyData$EUPATH_0000054),]
      obs <- obs[,which(unlist(lapply(obs, function(x)!all(is.na(x))))),with=F]
      houseObs <- houseObs[,which(unlist(lapply(houseObs, function(x)!all(is.na(x))))),with=F]
      myCols <- colnames(houseObs)[colnames(houseObs) %in% colnames(obs) & !colnames(houseObs) %in% c("Participant_Id")]
      houseObs <- houseObs[, !myCols, with=FALSE]
      obs <- unique(obs)
      metadata.file <<- metadata.file[metadata.file$category != "Entomological measurements",]
      keep <- c("Participant_Id", colnames(houseObs)[colnames(houseObs) %in% metadata.file$source_id])
      houseObs <- houseObs[, keep, with=FALSE]
      houseObs <- unique(houseObs)
      studyData <<- merge(obs, houseObs, by = "Participant_Id")    
    }

     longitudinal.file <<- fread("../../functions/longitudinal.tab")
     longitudinal.file <<- longitudinal.file[longitudinal.file$dataset_name == datasetName]
     longitudinal.file <<- setDT(longitudinal.file)[, lapply(.SD, function(x) unlist(tstrsplit(x, "|", fixed=TRUE))),
                         by = setdiff(names(longitudinal.file), "columns")][!is.na(longitudinal.file$columns)]

         nums <- getNums(metadata.file)$source_id
    strings <- getStrings(metadata.file)$source_id
    dates <- getDates(metadata.file)$source_id
    if (!nrow(longitudinal.file) == 0) {
      if (all(longitudinal.file$columns %in% dates) | all(longitudinal.file$columns %in% nums) | all(longitudinal.file$columns %in% strings)) {
        numTimelines <- 1
	if (!any(longitudinal.file$columns %in% strings)) {
          contLongitudinal <<- TRUE
        }
      } else {
        numTimelines <- 2
        contLongitudinal <<- TRUE
      }
      if (numTimelines == 1) {
        longitudinal1 <<- longitudinal.file$columns
        longitudinal2 <<- NULL
      } else {
        longitudinal1 <<- subset(longitudinal.file, longitudinal.file$columns %in% dates)$columns
        longitudinal2 <<- subset(longitudinal.file, longitudinal.file$columns %in% nums)$columns
      }
    } 
   }

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
    
      if (grepl("Error", attribute_temp[1])){
        stop("Error: Attributes file missing or unreadable!")
      } else {
        attributes.file <<- attribute_temp
        names(attributes.file) <<-  gsub(" ", "_", gsub("\\[|\\]", "", names(attributes.file)))
        project.id <<- attributes.file$project_id[1]
        attributes.file <<- cbind(attributes.file, custom = "Selected")

        if ('Participant_Id' %in% colnames(attributes.file)) {
          studyData <<- merge(studyData, attributes.file, by = "Participant_Id", all = TRUE)
          naToZero(studyData, col = "custom")
          studyData$custom[studyData$custom == 0] <<- "Not Selected"
        }
        if ('Observation_Id' %in% colnames(attributes.file)) {
          studyData <<- merge(studyData, attributes.file, by = "Observation_Id", all = TRUE)
          naToZero(studyData, col = "custom")
          studyData$custom[studyData$custom == 0] <<- "Not Selected"
        }

	if (!"timepoints" %in% colnames(metadata.file)) {
	  metadata.file$timepoints <<- NA
	}
        if ('Participant_Id' %in% colnames(attributes.file)) {
          attributes.file <- attributes.file[, Participant_Id:=as.character(Participant_Id)]
          isParticipant <<- TRUE
          metadata.file <<- rbind(metadata.file, list("custom", "Selected Participants", "string", "Participants", "Participant", NA, NA, NA, NA, NA, 2, "Selected|Not Selected", NA))
          metadata.file <<- rbind(metadata.file, list("dontcare", "Participants", "string", "Search Results", "Participant", NA, NA, NA, NA, NA, NA, NA, NA))
          metadata.file <<- rbind(metadata.file, list("dontcare2", "Dynamic Attributes", "string", "Search Results", "Participant", NA, NA, NA, NA, NA, NA, NA, NA))
          metadata.file <<- rbind(metadata.file, list("dontcare3", "Search Results", "string", "", "Participant", NA, NA, NA, NA, NA, NA, NA, NA))
          #TODO figure out what we want to do here for the dynamic ones. for now NA
          metadata.file <<- rbind(metadata.file, list("Avg_Female_Anopheles", "Avg Female Anopheles", "number", "Dynamic Attributes", "Participant", NA, NA, NA, NA, NA, NA, NA, NA))
          metadata.file <<- rbind(metadata.file, list("Matching_Observations_/_Year", "Matching Observations / Year", "number", "Dynamic Attributes", "Participant", NA, NA, NA, NA, NA, NA, NA, NA))
          metadata.file <<- rbind(metadata.file, list("Years_of_Observation", "Years of Observations", "number", "Dynamic Attributes", "Participant", NA, NA, NA, NA, NA, NA, NA, NA))
          } else {
          attributes.file = attributes.file[, Observation_Id:=as.character(Observation_Id)]
          isParticipant <<- FALSE
          metadata.file <<- rbind(metadata.file, list("custom", "Selected Observations", "string", "Observations", "Observation", NA, NA, NA, NA, NA, 2, "Selected|Not Selected", NA))
          metadata.file <<- rbind(metadata.file, list("dontcare", "Observations", "string", "Search Results", "Observation", NA, NA, NA, NA, NA, NA, NA, NA))
          metadata.file <<- rbind(metadata.file, list("dontcare2", "Search Results", "string", "", "Observation", NA, NA, NA, NA, NA, NA, NA, NA))
        }
      }
    }



  })
