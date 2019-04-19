reactiveDataFetcher = reactive({

   if (is.null(datasetName)) {
     custom.props <- try(fread(
        getWdkDatasetFile('customProps.txt', session, FALSE, dataStorageDir)))
     datasetName <<- colnames(custom.props)
     datasetDigest <<- paste0("D", substr(digest(datasetName, algo = "sha1", serialize=FALSE), 1, 10))
  
       model.prop <- fread(getWdkDatasetFile('model.prop', session, FALSE, dataStorageDir),
                           sep="=", header=FALSE, fill=TRUE)
       serviceUrl <<- paste0(model.prop$V2[model.prop$V1 == "LOCALHOST"], "/a/service/shiny")

       metadata.file <<- fread(getWdkDatasetFile("ontologyMetadata.tab", session, FALSE, dataStorageDir))
       metadata.file <<- metadata.file[metadata.file$CATEGORY != "Entomological measurements",]

     longitudinal.file <<- fread("../../functions/longitudinal.tab")
     longitudinal.file <<- longitudinal.file[longitudinal.file$dataset_name == datasetName]
     longitudinal.file <<- setDT(longitudinal.file)[, lapply(.SD, function(x) unlist(tstrsplit(x, "|", fixed=TRUE))), by = setdiff(names(longitudinal.file), c("columns", "house_columns"))][!is.na(longitudinal.file$columns)]

         nums <- getNums(metadata.file)$SOURCE_ID
    strings <- getStrings(metadata.file)$SOURCE_ID
    dates <- getDates(metadata.file)$SOURCE_ID
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
        message("lon data: ", paste0(serviceUrl, "/", metadata.file$CATEGORY[metadata.file$SOURCE_ID == longitudinal1], "/", datasetDigest, "/", longitudinal1))
        lon1Data <<- unique(as.data.table(stream_in(url(paste0(serviceUrl, "/", metadata.file$CATEGORY[metadata.file$SOURCE_ID == longitudinal1], "/", datasetDigest, "/", longitudinal1)), pagesize=1000)))
        lon1Data <<- lon1Data[, PARTICIPANT_ID:=as.character(PARTICIPANT_ID)]
        longitudinal2 <<- NULL
        hlongitudinal1 <<- ifelse(is.na(longitudinal.file$house_columns), NULL, longitudinal.file$house_columns)
        
        message("hlon data: ", paste0(serviceUrl, "/", metadata.file$CATEGORY[metadata.file$SOURCE_ID == hlongitudinal1], "/", datasetDigest, "/", hlongitudinal1))
        hlon1Data <<- unique(as.data.table(stream_in(url(paste0(serviceUrl, "/", metadata.file$CATEGORY[metadata.file$SOURCE_ID == hlongitudinal1], "/", datasetDigest, "/", hlongitudinal1)), pagesize=1000)))
        hlon1Data <<- hlon1Data[, PARTICIPANT_ID:=as.character(PARTICIPANT_ID)]
        hlongitudinal2 <<- NULL
      } else {
        longitudinal1 <<- subset(longitudinal.file, longitudinal.file$columns %in% dates)$columns
        
        longitudinal2 <<- subset(longitudinal.file, longitudinal.file$columns %in% nums)$columns
        message("lon data: ", paste0(serviceUrl, "/", metadata.file$CATEGORY[metadata.file$SOURCE_ID == longitudinal2], "/", datasetDigest, "/", longitudinal2, "?timeSourceId=", longitudinal1))
        lon2Data <<- unique(as.data.table(stream_in(url(paste0(serviceUrl, "/", metadata.file$CATEGORY[metadata.file$SOURCE_ID == longitudinal2], "/", datasetDigest, "/", longitudinal2, "?timeSourceId=", longitudinal1)), pagesize=1000)))
        lon2Data <<- lon2Data[, PARTICIPANT_ID:=as.character(PARTICIPANT_ID)]
        hlongitudinal1 <<- subset(longitudinal.file, longitudinal.file$house_columns %in% dates)$house_columns
        if (length(hlongitudinal1) > 0) {
          hlongitudinal2 <<- subset(longitudinal.file, longitudinal.file$house_columns %in% nums)$house_columns
          message("hlon data: ", paste0(serviceUrl, "/", metadata.file$CATEGORY[metadata.file$SOURCE_ID == hlongitudinal2], "/", datasetDigest, "/", hlongitudinal2, "?timeSourceId=", hlongitudinal1))
          hlon2Data <<- unique(as.data.table(stream_in(url(paste0(serviceUrl, "/", metadata.file$CATEGORY[metadata.file$SOURCE_ID == hlongitudinal2], "/", datasetDigest, "/", hlongitudinal2, "?timeSourceId=", hlongitudinal1)), pagesize=1000)))
          hlon2Data <<- hlon2Data[, PARTICIPANT_ID:=as.character(PARTICIPANT_ID)]
        } else {
          hlongitudinal1 <<- NULL
          hlongitudinal2 <<- NULL
        }
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
    
      if (grepl("Error", attribute_temp[[1]])){
        stop("Error: Attributes file missing or unreadable!")
      } else {
        attributes.file <<- attribute_temp
        names(attributes.file) <<-  gsub(" ", "_", gsub("\\[|\\]", "", names(attributes.file)))
        project.id <<- attributes.file$project_id[1]
        names(attributes.file)[names(attributes.file) == "Participant_Id"] <<- "PARTICIPANT_ID"
        names(attributes.file)[names(attributes.file) == "Observation_Id"] <<- "OBSERVATION_ID"
        attributes.file <<- cbind(attributes.file, custom = "Selected")

	if (!"timepoints" %in% colnames(metadata.file)) {
	  metadata.file$timepoints <<- NA
	}

        if ('PARTICIPANT_ID' %in% colnames(attributes.file)) {
          attributes.file <<- attributes.file[, PARTICIPANT_ID:=as.character(PARTICIPANT_ID)]
          isParticipant <<- TRUE
          metadata.file <<- rbind(metadata.file, list("custom", "Selected Participants", "string", "Participants", "Participant", "null", "null", "null", "null", "null", 2, "Selected|Not Selected", "null"))
          metadata.file <<- rbind(metadata.file, list("dontcare", "Participants", "string", "Search Results", "Participant", "null", "null", "null", "null", "null", "null", "null", "null"))
          metadata.file <<- rbind(metadata.file, list("dontcare2", "Dynamic Attributes", "string", "Search Results", "Participant", "null", "null", "null", "null", "null", "null", "null", "null"))
          metadata.file <<- rbind(metadata.file, list("dontcare3", "Search Results", "string", "null", "Participant", "null", "null", "null", "null", "null", "null", "null", "null"))
          #TODO figure out what we want to do here for the dy"null"mic ones. for now "null"
          metadata.file <<- rbind(metadata.file, list("Avg_Female_Anopheles", "Avg Female Anopheles", "number", "Dynamic Attributes", "Participant", "null", "null", "null", "null", "null", "null", "null", "null"))
          metadata.file <<- rbind(metadata.file, list("Matching_Observations_/_Year", "Matching Observations / Year", "number", "Dynamic Attributes", "Participant", "null", "null", "null", "null", "null", "null", "null", "null"))
          metadata.file <<- rbind(metadata.file, list("Years_of_Observation", "Years of Observations", "number", "Dynamic Attributes", "Participant", "null", "null", "null", "null", "null", "null", "null", "null"))
          } else {
          attributes.file = attributes.file[, OBSERVATION_ID:=as.character(OBSERVATION_ID)]
          isParticipant <<- FALSE
          metadata.file <<- rbind(metadata.file, list("custom", "Selected Observations", "string", "Observations", "Observation", "null", "null", "null", "null", "null", 2, "Selected|Not Selected", "null"))
          metadata.file <<- rbind(metadata.file, list("dontcare", "Observations", "string", "Search Results", "Observation", "null", "null", "null", "null", "null", "null", "null", "null"))
          metadata.file <<- rbind(metadata.file, list("dontcare2", "Search Results", "string", "null", "Observation", "null", "null", "null", "null", "null", "null", "null", "null"))
        }
      }
    }



  })
