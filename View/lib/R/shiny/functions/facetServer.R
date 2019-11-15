validateAndDebounceFacet <- debounce(reactive({
  if (is.null(input$facetType)) {
    return()
  }
  
  facetType <- input$facetType
  message("\n", Sys.time(), " functions/facetServer.R: validateAndDebounceFacet: facetType is: -", facetType, "-")
  test2 <- input$`facet-group`
  test2 <- input$`facet-group_stp1`
  test2 <- input$`facet-group_stp2`
  test2 <- input$`facet-group_stp3`
  test2 <- input$`facet-group_stp4`
  myFacet <- facetInfo()$group
  facet_stp1 <- facetInfo()$group_stp1
  facet_stp3 <- facetInfo()$group_stp3
  facet_stp4 <- facetInfo()$group_stp4
  facet_stp2 <- facetInfo()$group_stp2
  
  if (facetType == "makeGroups") {
    if (is.null(facet_stp1)) {
      return()
    } else {
      if (facet_stp1 == 'any' | facet_stp1 == 'all') {
        if (is.null(facet_stp2)) {
          return()
        } else {
          if (facet_stp2 %in% c("lessThan", "greaterThan", "equals")) {
            if (is.null(facet_stp3)) {
              return()
            }
          }
        }
      }
    }
  }else if (facetType == "direct") {
    if (is.null(myFacet)) {
      if (is.null(properties)) {
        myFacet = selectedFacet()
      } else {
        myFacet = properties$selected[properties$input == "facetInfo$group"]
        message(Sys.time(), " functions/facetServer.R: validateAndDebounceFacet: facetType direct, myFacet (read from properties) is: -", myFacet, "-")
      }
    }
  } else {
    myFacet <- "none"
  }


#else if (facetType == "direct") {
    #if (is.null(myFacet)) { 
      #if (is.null(selectedFacet())) {
	#return() 
      #} else {
 	#myFacet <- selectedFacet()
      #}
    #}
  #} else {
    #myFacet <- "none"
  #}

  message(Sys.time(), " functions/facetServer.R: validateAndDebounceFacet: finished validation, validated facet inputs:")
  message("input$facetType: ", facetType)
  message("myFacet: ", myFacet)
  message("facet_stp1: ", facet_stp1)
  message("facet_stp2: ", facet_stp2)
  message("facet_stp3: ", facet_stp3)
  message("facet_stp4: ", facet_stp4)  
  list(facetType = facetType, 
       myFacet = myFacet, 
       facet_stp1 = facet_stp1, 
       facet_stp2 = facet_stp2,
       facet_stp3 = facet_stp3,
       facet_stp4 = facet_stp4)
}), 1000)

facet1Query <- reactive({
  if (is.null(validateAndDebounceFacet())) {
    return()
  }
  myInputs <- validateAndDebounceFacet()
  myFacet <- myInputs$myFacet
  message("\n", Sys.time(), " functions/facetServer.R: facet1Query: myFacet is: -", myFacet, "-")
  if (myFacet != "none") {
    message(Sys.time(), " (if not none) Initiating query for facet data")
  } else {
    return()
  }

  dbCon <<- manageOracleConnection(dbDrv, dbCon, model.prop)
  data <- queryTermData(dbCon, myFacet, attributes.file, datasetDigest, metadata.file, longitudinal1, longitudinal2, lon2Data, lon1Data, hlongitudinal1, hlongitudinal2, hlon2Data, hlon1Data)
  if (is.null(data)) { message(Sys.time(), " functions/facetServer.R: facet1Query: query returned null")
                       return() }
  message(Sys.time(), " functions/facetServer.R: facet1Query: returning data")
  data
})

facet1 <- reactive({
  if (is.null(validateAndDebounceFacet()) | is.null(validateAndDebounceTimeline())) {
    return()
  }
  myInputs <- c(validateAndDebounceFacet(), validateAndDebounceTimeline())
  message(Sys.time(), " functions/facetServer.R: facet1 started") 
  facetType <- myInputs$facetType
  myFacet <- myInputs$myFacet
  facet_stp1 <- myInputs$facet_stp1
  facet_stp3 <- myInputs$facet_stp3
  facet_stp2 <- myInputs$facet_stp2
  facet_stp4 <- myInputs$facet_stp4
  mySubset <- myInputs$mySubset
  myTimeframe1 <- myInputs$myTimeframe1
  myTimeframe2 <- myInputs$myTimeframe2
  
  nums <- getNums(metadata.file)
  dates <- getDates(metadata.file)

  data <- facet1Query()
  if (is.null(data)) { return() }
  data <- timelineData(mySubset, myTimeframe1, myTimeframe2, data, longitudinal1, longitudinal2)


  myFacet<- myFacet
  names<-c(colnames(data))
  num<-grep(myFacet, colnames(data))

  if(sum(is.na(data[ ,num, with=FALSE])) == nrow(data)){
      message("the selected facet1 variable has no data, please select another one ")
      }




  if (facetType == "direct") {
        outData <- data
        if (myFacet %in% nums$SOURCE_ID | myFacet %in% dates$SOURCE_ID) {
          outData[[myFacet]] <- rcut_number(outData[[myFacet]],4)
        }
        displayLabel <- metadata.file$PROPERTY[metadata.file$SOURCE_ID == myFacet]
        outData[[myFacet]] <- paste0(displayLabel, ": ", outData[[myFacet]])
        myCols <- c(aggKey(), myFacet)
        outData <- unique(outData[, myCols, with=FALSE])
      } else if (facetType == "makeGroups") {
        aggKey <- aggKey()
        outData <- makeGroups(data, metadata.file, myFacet, facet_stp1, facet_stp2, facet_stp3, facet_stp4, aggKey)
	      if (is.null(outData)) {
	        return()
	      }
        observations <- metadata.file$SOURCE_ID[metadata.file$CATEGORY == "Observation"]
        label <- makeGroupLabel(myFacet, metadata.file, facet_stp1, facet_stp2, facet_stp3, facet_stp4, event.list = observations)
        #add makeGroups data to df and return
        colnames(outData) <- c(aggKey, "FACET")
        outData <- transform(outData, "FACET" = ifelse(as.numeric(FACET) == 0, label[2], label[1]))
        outData <- unique(outData)
      }

  #facetText <<- groupText("facetInfo", myFacet, facet_stp1, facet_stp2, facet_stp3, facet_stp4)
  unique(outData)
})

validateAndDebounceFacet2 <- debounce(reactive({
  if (is.null(input$facet2Type)) {
    return()
  }
  
  facet2Type <- input$facet2Type
  message("\n", Sys.time(), " functions/facetServer.R: validatendDebounceFacet2:: facet2Type is: -", facet2Type, "-")
  test <- input$`facet2-group`
  test <- input$`facet2-group_stp1`
  test <- input$`facet2-group_stp2`
  test <- input$`facet2-group_stp3`
  test <- input$`facet2-group_stp4`
  myFacet2 <- facet2Info()$group
  facet2_stp1 <- facet2Info()$group_stp1
  facet2_stp3 <- facet2Info()$group_stp3
  facet2_stp2 <- facet2Info()$group_stp2
  facet2_stp4 <- facet2Info()$group_stp4
  
  if (facet2Type == "makeGroups") {
    if (is.null(facet2_stp1)) {
      return()
    } else {
      if (facet2_stp1 == 'any' | facet2_stp1 == 'all') {
        if (is.null(facet2_stp2)) {
          return()
        } else {
          if (facet2_stp2 %in% c("lessThan", "greaterThan", "equals")) {
            if (is.null(facet2_stp3)) { return() }
          }
        }
      }
    }
  }else if (facet2Type == "direct") {
    if (is.null(myFacet2)) {
      if (is.null(properties)) {
        myFacet2 = selectedFacet2()
      } else {
        myFacet2 = properties$selected[properties$input == "facet2Info$group"]
        message(Sys.time(), " functions/facetServer.R: validateAndDebounceFacet2: facet2Type direct, myFacet2 (read from properties) is: -", myFacet2, "-")
      }
    }
  } else {
    myFacet2 <- "none"
  }


################################################### teste for facet2 ######################
  #} else if (facet2Type == "direct") {
   # if (is.null(myFacet2)) { 
    #  if (is.null(selectedFacet2())) {
     #   return() 
     # } else {
      #  myFacet2 <- selectedFacet2()
      #}
    #}
 # } else {
  #  myFacet2 <- "none"
  #}

  message(Sys.time(), " functions/facetServer.R: validateAndDebounceFacet2: finished validation, validated facet2 inputs:")
  message("input$facet2Type: ", facet2Type)
  message("myFacet2: ", myFacet2)
  message("facet2_stp1: ", facet2_stp1)      
  message("facet2_stp2: ", facet2_stp2)
  message("facet2_stp3: ", facet2_stp3)
  message("facet2_stp4: ", facet2_stp4)
  list(facet2Type = facet2Type, 
       myFacet2 = myFacet2, 
       facet2_stp1 = facet2_stp1, 
       facet2_stp2 = facet2_stp2,
       facet2_stp3 = facet2_stp3,
       facet2_stp4 = facet2_stp4)
}), 1000)

facet2Query <- reactive({
  if (is.null(validateAndDebounceFacet2())) {
    return()
  }
  myInputs <- validateAndDebounceFacet2()
  myFacet2 <- myInputs$myFacet2
  message("\n", Sys.time(), " functions/facetServer.R: facet2Query: myFacet2 is: -", myFacet2, "-")

  if (myFacet2 != "none") {
    message(Sys.time(), "(if not none) Initiating query for facet2 data")
  } else {
    return()
  }

  dbCon <<- manageOracleConnection(dbDrv, dbCon, model.prop)
  data <- queryTermData(dbCon, myFacet2, attributes.file, datasetDigest, metadata.file, longitudinal1, longitudinal2, lon2Data, lon1Data, hlongitudinal1, hlongitudinal2, hlon2Data, hlon1Data)
  if (is.null(data)) { message(Sys.time(), " functions/facetServer.R: facet2Query: query returned null")
                       return() }
  message(Sys.time(), " functions/facetServer.R: facet2Query: returning data")
  data
})

facet2 <- reactive({
  if (is.null(validateAndDebounceFacet2()) | is.null(validateAndDebounceFacet2())) {
    return()
  }
  myInputs <- c(validateAndDebounceFacet2(), validateAndDebounceTimeline())
  message(Sys.time(), " functions/facetServer.R: facet2 started") 
  facet2Type <- myInputs$facet2Type
  myFacet2 <- myInputs$myFacet2
  facet2_stp1 <- myInputs$facet2_stp1
  facet2_stp3 <- myInputs$facet2_stp3
  facet2_stp2 <- myInputs$facet2_stp2
  facet2_stp4 <- myInputs$facet2_stp4
  mySubset <- myInputs$mySubset
  myTimeframe1 <- myInputs$myTimeframe1
  myTimeframe2 <- myInputs$myTimeframe2

  nums <- getNums(metadata.file)
  dates <- getDates(metadata.file)

  data <- facet2Query()
  if (is.null(data)) { return() }
  data <- timelineData(mySubset, myTimeframe1, myTimeframe2, data, longitudinal1, longitudinal2)


  myFacet2<- myFacet2
  names<-c(colnames(data))
  num<-grep(myFacet2, colnames(data))

  if(sum(is.na(data[ ,num, with=FALSE])) == nrow(data)){
      message("the selected facet2 variable has no data, please select another one ")
      }




  if (facet2Type == "direct") {
        outData <- data
        if (myFacet2 %in% nums$SOURCE_ID | myFacet2 %in% dates$SOURCE_ID) {
          outData[[myFacet2]] <- rcut_number(outData[[myFacet2]],4)
        }
        displayLabel <- metadata.file$PROPERTY[metadata.file$SOURCE_ID == myFacet2]
        outData[[myFacet2]] <- paste0(displayLabel, ": ", outData[[myFacet2]])
        myCols <- c(aggKey(), myFacet2)
        outData <- unique(outData[, myCols, with=FALSE])
      } else if (facet2Type == "makeGroups") {
          aggKey <- aggKey()
          outData <- makeGroups(data, metadata.file, myFacet2, facet2_stp1, facet2_stp2, facet2_stp3, facet2_stp4, aggKey)
	  if (is.null(outData)) { return() }
          observations <- metadata.file$SOURCE_ID[metadata.file$CATEGORY == "Observation"]
          label <- makeGroupLabel(myFacet2, metadata.file, facet2_stp1, facet2_stp2, facet2_stp3, facet2_stp4, event.list = observations)
          #add makeGroups data to df and return
          colnames(outData) <- c(aggKey, "FACET2")
          outData <- transform(outData, "FACET2" = ifelse(as.numeric(FACET2) == 0, label[2], label[1]))
          outData <- unique(outData)
      }

  #facet2Text <<- groupText("facet2Info", myFacet2, facet2_stp1, facet2_stp2, facet2_stp3, facet2_stp4)
  unique(outData)
})

