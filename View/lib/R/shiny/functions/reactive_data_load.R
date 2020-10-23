reactiveDataFetcher = reactive({

   if (is.null(model.prop)) {
     model.prop <<- fread(getWdkDatasetFile('model.prop', session, FALSE, dataStorageDir), sep="=", header=FALSE, fill=TRUE)
   }
   dbCon <<- manageOracleConnection(dbDrv, dbCon, model.prop)

   if (is.null(datasetName)) {
     custom.props <- try(fread(
        getWdkDatasetFile('customProps.txt', session, FALSE, dataStorageDir)))
     datasetName <<- colnames(custom.props)
     datasetDigest <<- paste0("D", substr(digest(datasetName, algo = "sha1", serialize=FALSE), 1, 10))
 
       metadata.file <<- fread(getWdkDatasetFile("ontologyMetadata.tab", session, FALSE, dataStorageDir))
       metadata.file <<- metadata.file[metadata.file$CATEGORY != "Entomological measurements",]
       metadata.file <<- metadata.file[metadata.file$CATEGORY != "Entomology",]
       metadata.file <<- metadata.file[order(metadata.file$PROPERTY),]

     longitudinal.file <<- suppressWarnings(fread("../../functions/longitudinal.tab", blank.lines.skip = TRUE))
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
        if (!datasetName %in% names(lon1DataList)) {
          message("running query for lon1Data")
          lon1Data <<- getNamedQueryResult(dbCon, metadata.file$CATEGORY[metadata.file$SOURCE_ID == longitudinal1], datasetDigest, longitudinal1)
          lon1Data <<- lon1Data[, PARTICIPANT_ID:=as.character(PARTICIPANT_ID)]
          lon1Data <<- setDTColType(longitudinal1, metadata.file, lon1Data)
          lon1DataList[[datasetName]] <<- lon1Data
        } else {
	  message("already have lon1Data")
          lon1Data <<- lon1DataList[[datasetName]]
        }
        longitudinal2 <<- NULL

        hlongitudinal1 <<- NULL
        if(!is.na(longitudinal.file$house_columns)) {
	  hlongitudinal1 <<- longitudinal.file$house_columns
	}
        if (!is.null(hlongitudinal1)) { 
          if (!datasetName %in% names(hlon1DataList)) {
            hlon1Data <<- getNamedQueryResult(dbCon, metadata.file$CATEGORY[metadata.file$SOURCE_ID == hlongitudinal1], datasetDigest, hlongitudinal1)
            hlon1Data <<- hlon1Data[, PARTICIPANT_ID:=as.character(PARTICIPANT_ID)]
            hlon1Data <<- setDTColType(hlongitudinal1, metadata.file, hlon1Data)
            hlon1DataList[[datasetName]] <<- hlon1Data
          } else {
            hlon1Data <<- hlon1DataList[[datasetName]]
          }
        }
        hlongitudinal2 <<- NULL

        clongitudinal1 <<- NULL 
        if(!is.na(longitudinal.file$community_columns)) {
          clongitudinal1 <<- longitudinal.file$community_columns
        }
        if (!is.null(clongitudinal1)) { 
          if (!datasetName %in% names(clon1DataList)) {
            clon1Data <<- getNamedQueryResult(dbCon, metadata.file$CATEGORY[metadata.file$SOURCE_ID == clongitudinal1], datasetDigest, clongitudinal1)
            clon1Data <<- clon1Data[, PARTICIPANT_ID:=as.character(PARTICIPANT_ID)]
            clon1Data <<- setDTColType(clongitudinal1, metadata.file, clon1Data)
            clon1DataList[[datasetName]] <<- clon1Data
          } else {
            clon1Data <<- clon1DataList[[datasetName]]
          }
        }
        clongitudinal2 <<- NULL

      } else {
        longitudinal1 <<- subset(longitudinal.file, longitudinal.file$columns %in% dates)$columns
        
        longitudinal2 <<- subset(longitudinal.file, longitudinal.file$columns %in% nums)$columns
        if (!datasetName %in% names(lon2DataList)) {
          lon2Data <<- getNamedQueryResult(dbCon, metadata.file$CATEGORY[metadata.file$SOURCE_ID == longitudinal2], datasetDigest, longitudinal2, longitudinal1)
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
          if (!datasetName %in% names(hlon2DataList)) {
            hlon2Data <<- getNamedQueryResult(dbCon, metadata.file$CATEGORY[metadata.file$SOURCE_ID == hlongitudinal2], datasetDigest, hlongitudinal2, hlongitudinal1)
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

        clongitudinal1 <<- subset(longitudinal.file, longitudinal.file$house_columns %in% dates)$house_columns
        if (length(clongitudinal1) > 0) {
          clongitudinal2 <<- subset(longitudinal.file, longitudinal.file$house_columns %in% nums)$house_columns
          if (!datasetName %in% names(clon2DataList)) {
            clon2Data <<- getNamedQueryResult(dbCon, metadata.file$CATEGORY[metadata.file$SOURCE_ID == clongitudinal2], datasetDigest, clongitudinal2, clongitudinal1)
            clon2Data <<- clon2Data[, PARTICIPANT_ID:=as.character(PARTICIPANT_ID)]
            clon2Data <<- setDTColType(clongitudinal1, metadata.file, clon2Data)
            clon2Data <<- setDTColType(clongitudinal2, metadata.file, clon2Data)
            clon2DataList[[datasetName]] <<- clon2DataList
          } else {
            clon2Data <<- clon2DataList[[datasetName]]
          }
        } else { 
           clongitudinal1 <<- NULL
           clongitudinal2 <<- NULL
        }
      }
    } 
   }

   if (is.null(propUrl)) {
      propUrl <<- getPropertiesUrl(session)
      message("\n", Sys.time(), " functions/reactive_data_load.R: propUrl: ", propUrl)
      properties <<- suppressWarnings(try(fread(propUrl)))
      if (length(properties) > 0) {
        properties <<- unique(properties)
        message(Sys.time(), " functions/reactive_data_load.R: reading properties...")
        if (grepl("Error", properties)) {
	  message(Sys.time(), " functions/reactive_data_load.R: Error! properties will not be used")
          properties <<- NULL
        } else {message(Sys.time(), " functions/reactive_data_load.R: properties read:\n", properties)}
      } else {
        message(Sys.time(), " functions/reactive_data_load.R: no properties! new analysis")
        properties <<- NULL
      } 
    }

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
          metadata.file <<- rbind(metadata.file, list("custom", "Search results (selected participants)", "string", "null", "Participant", "null", "null", "null", "null", "null", 2, "Selected|Not Selected", "null"))
          } else {
          attributes.file = attributes.file[, OBSERVATION_ID:=as.character(OBSERVATION_ID)]
          isParticipant <<- FALSE
          metadata.file <<- rbind(metadata.file, list("custom", "Search results (selected observations)", "string", "null", "Observation", "null", "null", "null", "null", "null", 2, "Selected|Not Selected", "null"))
        }
      }
    }



  })
