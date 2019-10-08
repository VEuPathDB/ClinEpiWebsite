source("../../functions/timelineServer.R", local = TRUE)

source("../../functions/facetServer.R", local = TRUE)

validateAndDebounceAxes <- debounce(reactive({
  #hack to force reactivity. idk maybe its a shiny bug, but reactlog cant find the module inputs for this specific case without referencing directly

  test2 <- input$`group-group`

  myX <- xaxisInfo()$group
  group_stp1 <- xaxisInfo()$group_stp1
  group_stp2 <- xaxisInfo()$group_stp2
  group_stp3 <- xaxisInfo()$group_stp3
  group_stp4 <- xaxisInfo()$group_stp4


##### Keep Variable Label and Source_ID consistent ######################
#########################################################################

if (is.null(myX)) {
    if (is.null(properties)) {
       if (is.null(selectedGroup())) {
          return()
       }else{
       myX <- selectedGroup()
       }
     }else {
           myX = properties$selected[properties$input == "xaxisInfo()$group"]
           }
   }


  message(Sys.time(), " validated xaxis inputs:")
  message("myX: ", myX)
  list(myX = myX,
       groups_stp1 = group_stp1, 
       groups_stp2 = group_stp2,
       groups_stp3 = group_stp3,
       groups_stp4 = group_stp4 
)

  message("\n", Sys.time(), " ClinEpi_EventDist/server/plotData.R: validateAndDebounceAxes, validated xaxis inputs: myX: -", myX, "-")
  list(myX = myX)
}), 1000)


xQuery <- reactive({
  if (is.null(validateAndDebounceAxes())) {
    return()
  }
  myInputs <- validateAndDebounceAxes()
  myX <- myInputs$myX

  message("\n", Sys.time(), " ClinEpi_EventDist/server/plotData.R: xQuery:  Initiating query for xaxis data")
  dbCon <<- manageOracleConnection(dbDrv, dbCon, model.prop)
  data <- queryTermData(dbCon, myX, attributes.file, datasetDigest, metadata.file, longitudinal1, longitudinal2, lon2Data, lon1Data, hlongitudinal1, hlongitudinal2, hlon2Data, hlon1Data)
  if (is.null(data)) { return() }

  strings <- getStrings(metadata.file)
  myCols <- c(aggKey(), myX)
  outData <- data[, myCols, with = FALSE]

  #if (myX %in% strings$SOURCE_ID) {
  #  if (any(grepl("|", outData[[myX]], fixed=TRUE))) {
  #    outData <- separate_rows(outData, myX, sep = "[|]+")
  #  }
  #}
  
  unique(outData)
})

xAxis <- reactive({
  if (is.null(xQuery())) {
    return()
  }
  if (is.null(validateAndDebounceTimeline())) {
    return()
  }

  message("\n", Sys.time(), " ClinEpi_EventDist/server/plotData.R:: xAxis started") 
  myInputs <- validateAndDebounceTimeline()
  #myInputs <- c(validateAndDebounceAxes(), validateAndDebounceTimeline())
  #myX <- myInputs$myX

  mySubset <- myInputs$mySubset
  myTimeframe1 <- myInputs$myTimeframe1
  myTimeframe2 <- myInputs$myTimeframe2

  data <- xQuery()  
  data <- timelineData(mySubset, myTimeframe1, myTimeframe2, data, longitudinal1, longitudinal2)
 
  unique(data)
})


plotData <- reactive({
      if (is.null(xAxis())) {
        return()
      }

   
   ########## copy all things and paste here to resolve propUrl
   ############################################################
   
     myInputs <- c(validateAndDebounceAxes(), validateAndDebounceTimeline())

     mySubset <- myInputs$mySubset
     myTimeframe1 <- myInputs$myTimeframe1
     myTimeframe2 <- myInputs$myTimeframe2

     myFacet <- facetInfo()$group
     facet_stp1 <- facetInfo()$group_stp1
     facet_stp2 <- facetInfo()$group_stp2
     facet_stp3 <- facetInfo()$group_stp3
     facet_stp4 <- facetInfo()$group_stp4
     
     myFacet2 <- facet2Info()$group
     facet2_stp1 <- facet2Info()$group_stp1
     facet2_stp3 <- facet2Info()$group_stp3
     facet2_stp2 <- facet2Info()$group_stp2
     facet2_stp4 <- facet2Info()$group_stp4

     myX <- myInputs$myX
     groups_stp1 <- myInputs$groups_stp1
     groups_stp3 <- myInputs$groups_stp3
     groups_stp2 <- myInputs$groups_stp2
     groups_stp4 <- myInputs$groups_stp4
     #facet2Type <- myInputs$facet2Type


      groupsText <- groupText("xaxisInfo", myX, groups_stp1, groups_stp2, groups_stp3, groups_stp4)
      longitudinalText <- longitudinalText(mySubset, myTimeframe1, myTimeframe2)
      facetText <- groupText("facetInfo", myFacet, facet_stp1, facet_stp2, facet_stp3, facet_stp4)
      facet2Text <- groupText("facet2Info", myFacet2, facet2_stp1, facet2_stp2, facet2_stp3, facet2_stp4)


  
      #first thing is to save properties 
      text <- paste0("input\tselected\n",
                     longitudinalText,
                     facetText,
                     facet2Text,
		     groupsText,
                     "xaxisInfo()$group\t", validateAndDebounceAxes()$myX, "\n",
                     "input$facetType\t", input$facetType, "\n",
                     "input$facet2Type\t", input$facet2Type, "\n",
                     "input$xaxis\t", input$xaxis
                     #"input$individualPlot_stp1\t", input$individualPlot_stp1, "\n",
                     #"input$individualPlot_stp2\t", input$individualPlot_stp2 
                    )

#message("What are the saved parameterssssssss: ", text)

      message("\n", Sys.time(), " ClinEpi_EventDist/server/plotData.R: plotData: writing properties in propUrl:", propUrl, "\n", text)
      PUT(propUrl, body = "")
      PUT(propUrl, body = text)

      xData <- xAxis()
      facetData <- facet1()
      facet2Data <- facet2()

      if (!is.null(facetData)) {
        data <- merge(xData, facetData, by = aggKey())
      } else {
        data <- xData
      }
      if (!is.null(facet2Data)) {
        data <- merge(data, facet2Data, by = aggKey())
      } else {
      }
      
      unique(data)
    })
