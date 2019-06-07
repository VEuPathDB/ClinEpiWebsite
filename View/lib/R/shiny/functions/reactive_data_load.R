reactiveDataFetcher = reactive({

   if (is.null(dbCon)) {
     model.prop <- fread(getWdkDatasetFile('model.prop', session, FALSE, dataStorageDir), sep="=", header=FALSE, fill=TRUE)
     gusHome <- model.prop$V2[model.prop$V1 == "gusHome"]
     project_id <- model.prop$V2[model.prop$V1 == "PROJECT_ID"]
     modelConfig <- paste0(gusHome, "/config/", project_id, "/model-config.xml")
     appDbInfo <- appDbFromConfigXml(modelConfig)
     dbCon <<- dbConnect(dbDrv, appDbInfo$login, appDbInfo$password, unlist(strsplit(appDbInfo$connectionUrl, '@', fixed = TRUE))[2])
   }

   if (is.null(datasetName)) {
     custom.props <- try(fread(
        getWdkDatasetFile('customProps.txt', session, FALSE, dataStorageDir)))
     datasetName <<- colnames(custom.props)
     datasetDigest <<- paste0("D", substr(digest(datasetName, algo = "sha1", serialize=FALSE), 1, 10))
     #model.prop <- fread(getWdkDatasetFile('model.prop', session, FALSE, dataStorageDir),
     #                      sep="=", header=FALSE, fill=TRUE)
     #  serviceUrl <<- paste0(model.prop$V2[model.prop$V1 == "LOCALHOST"], "/a/service/shiny") 
 
       metadata.file <<- fread(getWdkDatasetFile("ontologyMetadata.tab", session, FALSE, dataStorageDir))
       metadata.file <<- metadata.file[metadata.file$CATEGORY != "Entomological measurements",]
       metadata.file <<- metadata.file[order(metadata.file$PROPERTY),]

     longitudinal.file <<- fread("../../functions/longitudinal.tab", blank.lines.skip = TRUE)
     names(longitudinal.file) <<- c("dataset_name", "columns", "house_columns")
     longitudinal.file <<- longitudinal.file[longitudinal.file$dataset_name == datasetName]
     longitudinal.file$columns[longitudinal.file$columns == "NA"] <<- NA
     longitudinal.file$house_columns[longitudinal.file$house_columns == "NA"] <<- NA
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
        message("lon data: ", metadata.file$CATEGORY[metadata.file$SOURCE_ID == longitudinal1], ", ", datasetDigest, ", ", longitudinal1)
	if (!datasetName %in% names(lon1DataList)) {
          lon1Data <<- getNamedQueryResult(dbCon, metadata.file$CATEGORY[metadata.file$SOURCE_ID == longitudinal1], datasetDigest, longitudinal1)
          #lon1Data <<- unique(as.data.table(stream_in(url(paste0(serviceUrl, "/", metadata.file$CATEGORY[metadata.file$SOURCE_ID == longitudinal1], "/", datasetDigest, "/", longitudinal1)), pagesize=10000)))
          lon1Data <<- lon1Data[, PARTICIPANT_ID:=as.character(PARTICIPANT_ID)]
          lon1Data <<- setDTColType(longitudinal1, metadata.file, lon1Data)
	  lon1DataList[[datasetName]] <<- lon1Data
	} else {
	  lon1Data <<- lon1DataList[[datasetName]]
        }
        longitudinal2 <<- NULL
        hlongitudinal1 <<- NULL 
        if(!is.na(longitudinal.file$house_columns)) {
	  hlongitudinal1 <<- longitudinal.file$house_columns
	}
        if (!is.null(hlongitudinal1)) { 
          message("hlon data: ", metadata.file$CATEGORY[metadata.file$SOURCE_ID == hlongitudinal1], ", ", datasetDigest, ", ", hlongitudinal1)
  	  if (!datasetName %in% names(hlon1DataList)) {
            hlon1Data <<- getNamedQueryResult(dbCon, metadata.file$CATEGORY[metadata.file$SOURCE_ID == hlongitudinal1], datasetDigest, hlongitudinal1)
            #hlon1Data <<- unique(as.data.table(stream_in(url(paste0(serviceUrl, "/", metadata.file$CATEGORY[metadata.file$SOURCE_ID == hlongitudinal1], "/", datasetDigest, "/", hlongitudinal1)), pagesize=10000)))
            hlon1Data <<- hlon1Data[, PARTICIPANT_ID:=as.character(PARTICIPANT_ID)]
            hlon1Data <<- setDTColType(hlongitudinal1, metadata.file, hlon1Data)
  	    hlon1DataList[[datasetName]] <<- hlon1Data
            } else {
  	    hlon1Data <<- hlon1DataList[[datasetName]]
  	  }
        }
        hlongitudinal2 <<- NULL
      } else {
        longitudinal1 <<- subset(longitudinal.file, longitudinal.file$columns %in% dates)$columns
        
        longitudinal2 <<- subset(longitudinal.file, longitudinal.file$columns %in% nums)$columns
        message("lon data: ", metadata.file$CATEGORY[metadata.file$SOURCE_ID == longitudinal2], ", ", datasetDigest, ", ", longitudinal2, ", ", longitudinal1)
	if (!datasetName %in% names(lon2DataList)) {
          lon2Data <<- getNamedQueryResult(dbCon, metadata.file$CATEGORY[metadata.file$SOURCE_ID == longitudinal2], datasetDigest, longitudinal2, longitudinal1)
          #lon2Data <<- unique(as.data.table(stream_in(url(paste0(serviceUrl, "/", metadata.file$CATEGORY[metadata.file$SOURCE_ID == longitudinal2], "/", datasetDigest, "/", longitudinal2, "?timeSourceId=", longitudinal1)), pagesize=10000)))
          lon2Data <<- lon2Data[, PARTICIPANT_ID:=as.character(PARTICIPANT_ID)]
          lon2Data <<- setDTColType(longitudinal1, metadata.file, lon2Data)
          lon2Data <<- setDTColType(longitudinal2, metadata.file, lon2Data)
	  lon2DataList[[datasetName]] <<- lon2Data
	} else {
	  lon2Data <<- lon2DataList[[datasetName]]
        }
        hlongitudinal1 <<- subset(longitudinal.file, longitudinal.file$house_columns %in% dates)$house_columns
        if (length(hlongitudinal1) > 0) {
          hlongitudinal2 <<- subset(longitudinal.file, longitudinal.file$house_columns %in% nums)$house_columns
          message("hlon data: ", metadata.file$CATEGORY[metadata.file$SOURCE_ID == hlongitudinal2], ", ", datasetDigest, ", ", hlongitudinal2, ", ", hlongitudinal1)
          if (!datasetName %in% names(hlon2DataList)) {
            hlon2Data <<- getNamedQueryResult(dbCon, metadata.file$CATEGORY[metadata.file$SOURCE_ID == hlongitudinal2], datasetDigest, hlongitudinal2, hlongitudinal1)
            #hlon2Data <<- unique(as.data.table(stream_in(url(paste0(serviceUrl, "/", metadata.file$CATEGORY[metadata.file$SOURCE_ID == hlongitudinal2], "/", datasetDigest, "/", hlongitudinal2, "?timeSourceId=", hlongitudinal1)), pagesize=10000)))
            hlon2Data <<- hlon2Data[, PARTICIPANT_ID:=as.character(PARTICIPANT_ID)]
            hlon2Data <<- setDTColType(hlongitudinal1, metadata.file, hlon2Data)
            hlon2Data <<- setDTColType(hlongitudinal2, metadata.file, hlon2Data)
	    hlon2DataList[[datasetName]] <<- hlon2DataList
	  } else {
	    hlon2Data <<- hlon2DataList[[datasetName]]
	  }
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
      if (length(properties) > 0) {
        if (grepl("Error", properties)) {
          properties <<- NULL
        }
      } else {
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
