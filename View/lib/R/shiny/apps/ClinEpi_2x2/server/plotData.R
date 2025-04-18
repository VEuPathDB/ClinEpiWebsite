source("../../functions/timelineServer.R", local = TRUE)

source("../../functions/facetServer.R", local = TRUE)

validateAndDebounceAttr <- debounce(reactive({
  test2 <- input$`attr-group`
  test2 <- input$`attr-group_stp1`
  test2 <- input$`attr-group_stp2`
  test2 <- input$`attr-group_stp3`
  test2 <- input$`attr-group_stp4`
  myAttr <- attrInfo()$group
  attr_stp1 <- attrInfo()$group_stp1
  attr_stp2 <- attrInfo()$group_stp2
  attr_stp3 <- attrInfo()$group_stp3
  attr_stp4 <- attrInfo()$group_stp4

  
  if (is.null(attr_stp1)) {
    return()
  } else {
    if (attr_stp1 == 'any' | attr_stp1 == 'all') {
      if (is.null(attr_stp2)) {
        return()
      } else {
        if (attr_stp2 %in% c("lessThan", "greaterThan", "equals")) {
          if (is.null(attr_stp3)) {
            return()
          }
        }
      }
    }
  }


  message(Sys.time(), " validated attr inputs:")
  message("myAttr: ", myAttr)
  message("attr_stp1: ", attr_stp1)      
  message("attr_stp2: ", attr_stp2)
  message("attr_stp3: ", attr_stp3)
  message("attr_stp4: ", attr_stp4)  
  list(myAttr = myAttr, 
       attr_stp1 = attr_stp1, 
       attr_stp2 = attr_stp2,
       attr_stp3 = attr_stp3,
       attr_stp4 = attr_stp4)
}), 1000)

attrQuery <- reactive({
  if (is.null(validateAndDebounceAttr())) { return() }
  myInputs <- validateAndDebounceAttr()
  myAttr <- myInputs$myAttr

  message(Sys.time(), " Initiating query for attribute data")
  dbCon <<- manageOracleConnection(dbDrv, dbCon, model.prop)
  data <- queryTermData(dbCon, myAttr, attributes.file, datasetDigest, metadata.file, longitudinal1, longitudinal2, lon2Data, lon1Data, hlongitudinal1, hlongitudinal2, hlon2Data, hlon1Data, clongitudinal1, clongitudinal2, clon2Data, clon1Data)
  if (is.null(data)) { return() }

  data
})

attr <- reactive({
  if (is.null(validateAndDebounceAttr()) | is.null(validateAndDebounceTimeline())) { return() }
  myInputs <- c(validateAndDebounceAttr(), validateAndDebounceTimeline())
  myAttr <- myInputs$myAttr
  attr_stp1 <- myInputs$attr_stp1
  attr_stp2 <- myInputs$attr_stp2
  attr_stp3 <- myInputs$attr_stp3
  attr_stp4 <- myInputs$attr_stp4
  mySubset <- myInputs$mySubset
  myTimeframe1 <- myInputs$myTimeframe1
  myTimeframe2 <- myInputs$myTimeframe2

  if (!is.null(hlongitudinal1)) {
    if (myAttr == hlongitudinal1) {
      myAttr <- longitudinal1
    }
  }
 
  data <- attrQuery()
  if (is.null(data)) { return() }
  data <- timelineData(mySubset, myTimeframe1, myTimeframe2, data, longitudinal1, longitudinal2, lon1Data, lon2Data)

  if (all(is.na(data[, myAttr, with=FALSE]))) {
    showNotification(paste0("The Independent/Exposure variable has no data for the timepoint(s) selected, please select another."), duration = NULL, type = "warning")
    return()
  }

  #get attr col
  aggKey <- aggKey()
  myCols <- c(aggKey, myAttr)
  attrData <- data[, myCols, with=FALSE]
	attrData <- completeDT(attrData, myAttr)
  attrData <- getFinalDT(attrData, metadata.file, myAttr)

  attrData <- makeGroups(attrData, metadata.file, myAttr, attr_stp1, attr_stp2, attr_stp3, attr_stp4, aggKey)
	if (is.null(attrData)) { return() }
  observations <- metadata.file$SOURCE_ID[metadata.file$CATEGORY == "Observation"]
  colnames(attrData) <- c(aggKey, "Attribute")
        
  #attrText <<- groupText("attrInfo", myAttr, attr_stp1, attr_stp2, attr_stp3, attr_stp4)
  unique(attrData)
})

validateAndDebounceOut <- debounce(reactive({
  test2 <- input$`out-group`
  test2 <- input$`out-group_stp1`
  test2 <- input$`out-group_stp2`
  test2 <- input$`out-group_stp3`
  test2 <- input$`out-group_stp4`
  myOut <- outInfo()$group
  out_stp1 <- outInfo()$group_stp1
  out_stp3 <- outInfo()$group_stp3
  out_stp4 <- outInfo()$group_stp4
  out_stp2 <- outInfo()$group_stp2
  
  if (is.null(out_stp1)) {
    return()
  } else {
    if (out_stp1 == 'any' | out_stp1 == 'all') {
      if (is.null(out_stp2)) {
        return()
      } else {
        if (out_stp2 %in% c("lessThan", "greaterThan", "equals")) {
          if (is.null(out_stp3)) {
            return()
          }
        }
      }
    }
  }
  
  message(Sys.time(), " validated outcome inputs:")
  message("myOut: ", myOut)
  message("out_stp1: ", out_stp1)      
  message("out_stp2: ", out_stp2)
  message("out_stp3: ", out_stp3)
  message("out_stp4: ", out_stp4)

  if (!outInit$done) {outInit$val <<- isolate(outInit$val) + 1}
  list(myOut = myOut, 
       out_stp1 = out_stp1, 
       out_stp2 = out_stp2,
       out_stp3 = out_stp3,
       out_stp4 = out_stp4)
}), 1000)

outQuery <- reactive({
  if (is.null(validateAndDebounceOut())) { return() }
  myInputs <- validateAndDebounceOut()
  myOut <- myInputs$myOut

  message(Sys.time(), " Initiating query for outcome data")
  dbCon <<- manageOracleConnection(dbDrv, dbCon, model.prop)
  data <- queryTermData(dbCon, myOut, attributes.file, datasetDigest, metadata.file, longitudinal1, longitudinal2, lon2Data, lon1Data, hlongitudinal1, hlongitudinal2, hlon2Data, hlon1Data, clongitudinal1, clongitudinal2, clon2Data, clon1Data)
  if (is.null(data)) { return() }

  data
})

out <- reactive({
  if (is.null(validateAndDebounceOut()) | is.null(validateAndDebounceTimeline())) { return() }
  myInputs <- c(validateAndDebounceOut(), validateAndDebounceTimeline())
  myOut <- myInputs$myOut
  out_stp1 <- myInputs$out_stp1
  out_stp2 <- myInputs$out_stp2
  out_stp3 <- myInputs$out_stp3
  out_stp4 <- myInputs$out_stp4

  mySubset <- myInputs$mySubset
  myTimeframe1 <- myInputs$myTimeframe1
  myTimeframe2 <- myInputs$myTimeframe2
  
  if (!is.null(hlongitudinal1)) {
    if (myOut == hlongitudinal1) {
      myOut <- longitudinal1
    }
  }

  data <- outQuery()
  if (is.null(data)) { return() }
  data <- timelineData(mySubset, myTimeframe1, myTimeframe2, data, longitudinal1, longitudinal2, lon1Data, lon2Data)
 
  if (all(is.na(data[, myOut, with=FALSE]))) {
    showNotification(paste0("the Dependent/Outcome variable has no data for the timepoint(s) selected, please select another."), duration = NULL, type = "error")
    return()
  }
 
  #get out col
  aggKey <- aggKey()
  myCols <- c(aggKey, myOut)
  outData <- data[, myCols, with=FALSE]
  outData <- completeDT(outData, myOut)
  outData <- getFinalDT(outData, metadata.file, myOut)
  
  outData <- makeGroups(outData, metadata.file, myOut, out_stp1, out_stp2, out_stp3, out_stp4, aggKey)
  if (is.null(outData)) { return() }
  observations <- metadata.file$SOURCE_ID[metadata.file$CATEGORY == "Observation"]
  colnames(outData) <- c(aggKey, "Outcome")
  
  #outText <<- groupText("outInfo", myOut, out_stp1, out_stp2, out_stp3, out_stp4)
  unique(outData)
})


plotData <- reactive({
  if (is.null(validateAndDebounceAttr()) | is.null(validateAndDebounceOut()) | is.null(validateAndDebounceFacet())| is.null(validateAndDebounceTimeline()) | is.null(validateAndDebounceFacet2())) { return() }
  myInputs <- c(validateAndDebounceAttr(), validateAndDebounceOut(), validateAndDebounceFacet(), validateAndDebounceFacet2(), validateAndDebounceTimeline())
  
  myAttr <- myInputs$myAttr
  attr_stp1 <- myInputs$attr_stp1
  attr_stp2 <- myInputs$attr_stp2
  attr_stp3 <- myInputs$attr_stp3
  attr_stp4 <- myInputs$attr_stp4

  myOut <- myInputs$myOut
  out_stp1 <- myInputs$out_stp1
  out_stp2 <- myInputs$out_stp2
  out_stp3 <- myInputs$out_stp3
  out_stp4 <- myInputs$out_stp4

  mySubset <- myInputs$mySubset
  myTimeframe1 <- myInputs$myTimeframe1
  myTimeframe2 <- myInputs$myTimeframe2

  facetType <- myInputs$facetType
  myFacet <- myInputs$myFacet
  facet_stp1 <- myInputs$facet_stp1
  facet_stp3 <- myInputs$facet_stp3
  facet_stp2 <- myInputs$facet_stp2
  facet_stp4 <- myInputs$facet_stp4

  facet2Type <- myInputs$facet2Type
  myFacet2 <- myInputs$myFacet2
  facet2_stp1 <- myInputs$facet2_stp1
  facet2_stp2 <- myInputs$facet2_stp2
  facet2_stp3 <- myInputs$facet2_stp3
  facet2_stp4 <- myInputs$facet2_stp4


  longitudinalText <- longitudinalText(mySubset, myTimeframe1, myTimeframe2)
  facetText <- groupText("facetInfo", myFacet, facet_stp1, facet_stp2, facet_stp3, facet_stp4)
  facet2Text <- groupText("facet2Info", myFacet2, facet2_stp1, facet2_stp2, facet2_stp3, facet2_stp4)
  attrText <- groupText("attrInfo", myAttr, attr_stp1, attr_stp2, attr_stp3, attr_stp4)
  outText <- groupText("outInfo", myOut, out_stp1, out_stp2, out_stp3, out_stp4)
   

        text <- paste0("input\tselected\n",
                       longitudinalText,
                       attrText,
                       outText,
                       facetText,
                       facet2Text,
                       "input$facetType\t", facetType, "\n",
                       "input$facet2Type\t", facet2Type
                       #"input$individualPlot_stp1\t", input$individualPlot_stp1, "\n",
                       #"input$individualPlot_stp2\t", input$individualPlot_stp2 
                      )

         message("\n", Sys.time(), " ClinEpi_2x2/server/plotData.R: plotData: writing properties in propUrl:", propUrl, "\n", text)
        PUT(propUrl, body = "")
        PUT(propUrl, body = text)



#message("What are the saved parameterssssssss: ", text)



        aggKey <- aggKey()
        attrData <- attr()
	if (is.null(attrData)) { return() }
        outData <- out()
	if (is.null(outData)) { return() }
        facetData <- facet1()
        facet2Data <- facet2()

        data <- merge(attrData, outData, by = aggKey, all = TRUE)
        if (!is.null(facetData)) {
          data <- merge(data, facetData, by = aggKey, all.x = TRUE)
        }
        if (!is.null(facet2Data) && myFacet != myFacet2) {
          data <- merge(data, facet2Data, by = aggKey, all.x = TRUE)
        }

        #naToZero(data)
        #format into 2x2
        data <- transform(data, "APOP" = ifelse( Attribute == 1 & Outcome == 1, 1, 0))
        data <- transform(data, "APOF" = ifelse( Attribute == 1 & Outcome == 0, 1, 0))
        data <- transform(data, "AFOP" = ifelse( Attribute == 0 & Outcome == 1, 1, 0))
        data <- transform(data, "AFOF" = ifelse( Attribute == 0 & Outcome == 0, 1, 0))

        #TODO use aggregate here so we can sum based on facets
        if (facetType == "makeGroups") {
          facetCol = "FACET"
	  if (!facetCol %in% colnames(data)) {return()}
        } else if (myFacet == "none") {
          facetCol = c()
        }  else {
          facetCol = myFacet
        }
        if (facet2Type == "makeGroups") {
          facet2Col = "FACET2"
	  if (!facet2Col %in% colnames(data)) {return()}
        } else if (myFacet2 == "none") {
          facet2Col = c()
        } else {
          facet2Col = myFacet2
        }
        facets <- c(facetCol, facet2Col)
        if (length(facets) == 0) {
          APOP <- sum(data$APOP, na.rm = TRUE)
          APOF <- sum(data$APOF, na.rm = TRUE)
          AFOP <- sum(data$AFOP, na.rm = TRUE)
          AFOF <- sum(data$AFOF, na.rm = TRUE)

          Proportion <- c(APOP,APOF,AFOP,AFOF)
          Variable1 <- c("Attribute+", "Attribute+", "Attribute-", "Attribute-")
          Variable2 <- c("Outcome+", "Outcome-", "Outcome+", "Outcome-")
          returnData <- data.table(Proportion, Variable1, Variable2)
        } else {
          returnData <- aggregate(as.formula(paste("APOP ~ ", paste(facets, collapse = "+"))), data, sum)
          APOF <- aggregate(as.formula(paste("APOF ~ ", paste(facets, collapse = "+"))), data, sum)
          returnData <- merge(returnData, APOF, by = facets)
          AFOP <- aggregate(as.formula(paste("AFOP ~ ", paste(facets, collapse = "+"))), data, sum)
          returnData <- merge(returnData, AFOP, by = facets)
          AFOF <- aggregate(as.formula(paste("AFOF ~ ", paste(facets, collapse = "+"))), data, sum)
          returnData <- merge(returnData, AFOF, by = facets)

          returnData <- gather(returnData, key, Proportion, -facets)
          returnData <- transform(returnData, "Variable1" = ifelse( key == "APOP" | key == "APOF", "Attribute+", "Attribute-"))
          returnData <- transform(returnData, "Variable2" = ifelse( key == "AFOP" | key == "APOP", "Outcome+", "Outcome-"))
        }

       #add labels

        attrLabel <- makeGroupLabel(myAttr, metadata.file, attr_stp1, attr_stp2, attr_stp3, attr_stp4, observations)
        outLabel <- makeGroupLabel(myOut, metadata.file, out_stp1, out_stp2, out_stp3, out_stp4, observations)

        returnData <- transform(returnData, "Exposure" = ifelse( Variable1 == "Attribute+", attrLabel[1], attrLabel[2]))
        returnData <- transform(returnData, "Outcome" = ifelse( Variable2 == "Outcome+", outLabel[1], outLabel[2]))

        returnData
      

    })

