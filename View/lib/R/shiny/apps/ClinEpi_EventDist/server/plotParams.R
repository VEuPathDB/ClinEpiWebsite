  observeEvent(reactiveVal(0), {
    current <<- callModule(timeline, "timeline", longitudinal.file, metadata.file)
  }, once = TRUE)

  observeEvent(input$xaxis, {
    xaxisInfo <<- callModule(customGroups, "group", groupLabel = reactive(NULL), metadata.file = metadata.file, include = groupData, selected = selectedGroup, groupsType = reactive(input$xaxis), groupsTypeID = "input$xaxis", moduleName = "xaxisInfo", prtcpntView = reactive(prtcpntView$val), timepoints = reactive(current$subset))
  }, once = TRUE)

  observeEvent(input$facetType, {
    facetInfo <<- callModule(customGroups, "facet", groupLabel = reactive(NULL), metadata.file = metadata.file, include = facetData, selected = selectedFacet, groupsType = reactive(input$facetType), groupsTypeID = "input$facetType", moduleName = "facetInfo", prtcpntView = reactive(prtcpntView$val), timepoints = reactive(current$subset))
  }, once = TRUE)

  observeEvent(input$facet2Type, {
    facet2Info <<- callModule(customGroups, "facet2", groupLabel = reactive(NULL), metadata.file = metadata.file, include = facet2Data, selected = selectedFacet2, groupsType = reactive(input$facet2Type), groupsTypeID = "input$facet2Type", moduleName = "facet2Info", prtcpntView = reactive(prtcpntView$val), timepoints = reactive(current$subset))
  }, once = TRUE)

#  output$prtcpntViewSwitch <- renderUI({
#    if (isParticipant != TRUE) {
#      tagList(
#        box(width = 6, status = "primary", title = "Unit of Analysis",
#            radioButtons(inputId = "prtcpntViewSwitch",
#                         label = NULL,
#                         choiceNames = c("Participant View", "Observation View"),
#                         choiceValues = c(TRUE, FALSE),
#                         selected = "FALSE",
#                         inline = TRUE)
#        )
#      )
#    }
#  })
#
  #in case input does not get rendered (i.e. is null) set prtcpntView$val TRUE globally and override with input
#  observeEvent(input$prtcpntViewSwitch, {
#    if (input$prtcpntViewSwitch == "TRUE" | input$prtcpntViewSwitch == TRUE) {
  observeEvent(isParticipant, {
    if (isParticipant) {
      prtcpntView$val <- TRUE
    } else {
      prtcpntView$val <- FALSE
    }
  })

   output$choose_xaxis <- renderUI({

        mySelected <- properties$selected[properties$input == "input$xaxis"]

        if (is.null(properties)) {
          selectInput(inputId = "xaxis",
                      label = NULL,
                      choices = c("Select a variable" = "direct"),
                      selected = "direct",
                      width = '100%')
        } else {
          selectInput(inputId = "xaxis",
                      label = NULL,
                      choices = c("Select a variable" = "direct"),
                      selected = mySelected,
                      width = '100%')
        }
    })

    typeChoices <- reactive({

      label <- "Stratify by a non-longitudinal variable"

      if (!isParticipant & !prtcpntView$val) {
        label <- "Stratify by a variable"
      }

      myChoices <- c(label = "direct", "Stratify by a variable you will dichotomize" = "makeGroups", "Do not stratify" = "none")
      names(myChoices)[names(myChoices) == "label"] <- label

      myChoices
    })

    output$facet_type <- renderUI({
      mySelected <- properties$selected[properties$input == "input$facetType"]

      if (is.null(properties)) {
        if (isParticipant) {
          selectInput(inputId = "facetType",
                      label = NULL,
                      choices = typeChoices(),
                      selected = "direct",
                      width = '100%')
        } else {
          selectInput(inputId = "facetType",
                      label = NULL,
                      choices = typeChoices(),
                      selected = "makeGroups",
                      width = '100%')
        }
      } else {
        selectInput(inputId = "facetType",
                    label = NULL,
                    choices = typeChoices(),
                    selected = mySelected,
                    width = '100%')
      }
    })

    output$facet2_type <- renderUI({
      mySelected <- properties$selected[properties$input == "input$facet2Type"]

      if (is.null(properties)) {
        selectInput(inputId = "facet2Type",
                    label = NULL,
                    choices = typeChoices(),
                    selected = "none",
                    width = '100%')
      } else {
        selectInput(inputId = "facet2Type",
                    label = NULL,
                    choices = typeChoices(),
                    selected = mySelected,
                    width = '100%')
      }
    })

    groupLabel <- reactive({
      if (is.null(input$xaxis)) {
        return()
      } else {
        groupsType <- input$xaxis
      }

      label = "bins for"

      return(label)
    })

    groupData <- reactive({
      if (is.null(input$xaxis)) {
        return()
      } else {
        groupsType <- input$xaxis
      }
      
      #TODO figure why this is in a reactive. dont remember....
      return(c("all"))
    })

    selectedGroup <- reactive({
      if (is.null(input$xaxis)) {
        return()
      } else {
        groupsType <- input$xaxis
      }

      if ("EUPATH_0000338" %in% metadata.file$SOURCE_ID) {
        selected <- "EUPATH_0000338"
      } else {
        include <- groupData()
        if (include != "all") {
          temp <- metadata.file[metadata.file$CATEGORY %in% include]
        } else {
          temp <- metadata.file
        }
        myCols <- metadata.file$SOURCE_ID[!is.na(metadata.file$NUMBER_DISTINCT_VALUES)]
        temp <- temp[temp$SOURCE_ID %in% myCols]
        parents <- temp$PARENTLABEL
        leaves <- temp[!temp$PROPERTY %in% parents]
        leaves <- leaves[order(leaves$PROPERTY),]
        leaves <- leaves$SOURCE_ID
        #remove dates
        dates <- getDates(metadata.file)$SOURCE_ID
        leaves <- leaves[!leaves %in% dates]
        selected <- leaves[1]
      }

      return(selected)
    })

    selectedFacet <- reactive({
      if (is.null(input$facetType)) {
        return()
      } else {
        facetType <- input$facetType
      }

      if (facetType == "direct") {
        #selected <- "custom"
        selected <- "custom"
      } else if (facetType == "makeGroups") {
        if (isParticipant) {
          if ("EUPATH_0000054" %in% metadata.file$SOURCE_ID[!is.na(metadata.file$NUMBER_DISTINCT_VALUES)]) {
            selected <- "EUPATH_0000054"
          } else {
            include <- facetData()
            if (include != "all") {
              temp <- metadata.file[metadata.file$CATEGORY %in% include]
            } else {
              temp <- metadata.file
            }
            myCols <- metadata.file$SOURCE_ID[!is.na(metadata.file$NUMBER_DISTINCT_VALUES)]
            temp <- temp[temp$SOURCE_ID %in% myCols]
            parents <- temp$PARENTLABEL
            leaves <- temp[!temp$PROPERTY %in% parents]
            leaves <- leaves[order(leaves$PROPERTY),]
            leaves <- leaves$SOURCE_ID
            #remove dates
            dates <- getDates(metadata.file)$SOURCE_ID
            leaves <- leaves[!leaves %in% dates]
            selected <- leaves[1]
          }
        } else {
          selected <- "custom"
        }
      } else {
        selected <- ""
      }

      return(selected)
    })

    facetLabel <- reactive({
      if (is.null(input$facetType)) {
        return()
      } else {
        facetType <- input$facetType
      }

      label = ""
      if (facetType == "direct") {
        label <- "strata for"
      } else if (facetType != "none") {
        label <- "strata where:"
      }

      return(label)
    })

    facetData <- reactive({
      if (is.null(input$facetType)) {
        return()
      } else {
        facetType <- input$facetType
      }
    
      include <- c("all")
      if (isParticipant | prtcpntView$val) {
        if (facetType == "direct") {
          if (is.null(hlongitudinal1)) {
            include <- c("Participant", "Household")
          } else {
            include <- c("Participant")
          }
        }
      }    
 
      return(include)
    })

    selectedFacet2 <- reactive({
      if (is.null(input$facet2Type)) {
        return()
      } else {
        facet2Type <- input$facet2Type
      }

      if (facet2Type == "direct") {
        selected <- "custom"
      } else if (facet2Type == "makeGroups") {
        if (isParticipant) {
          if ("EUPATH_0000054" %in% metadata.file$SOURCE_ID[!is.na(metadata.file$NUMBER_DISTINCT_VALUES)]) {
            selected <- "EUPATH_0000054"
          } else {
            include <- facet2Data()
            if (include != "all") {
              temp <- metadata.file[metadata.file$CATEGORY %in% include]
            } else {
              temp <- metadata.file
            }
            myCols <- metadata.file$SOURCE_ID[!is.na(metadata.file$NUMBER_DISTINCT_VALUES)]
            temp <- temp[temp$SOURCE_ID %in% myCols]
            parents <- temp$PARENTLABEL
            leaves <- temp[!temp$PROPERTY %in% parents]
            leaves <- leaves[order(leaves$PROPERTY),]
            leaves <- leaves$SOURCE_ID
            #remove dates
            dates <- getDates(metadata.file)$SOURCE_ID
            leaves <- leaves[!leaves %in% dates]
            selected <- leaves[1]
          }
        } else {
          selected <- "custom"
        }
      } else {
        selected <- ""
      }

      return(selected)
    })

    facet2Label <- reactive({
      if (is.null(input$facet2Type)) {
        return()
      } else {
        facet2Type <- input$facet2Type
      }

      label = ""
      if (facet2Type == "direct") {
        label <- "strata for"
      } else if (facet2Type != "none") {
        label <- "strata where:"
      }

      return(label)
    })

    facet2Data <- reactive({
      if (is.null(input$facet2Type)) {
        return()
      } else {
        facet2Type <- input$facet2Type
      }

      include <- c("all")
      if (isParticipant | prtcpntView$val) {
        if (facet2Type == "direct") {
          if (is.null(hlongitudinal1)) {
            include <- c("Participant", "Household")
          } else {
            include <- c("Participant")
          }
        }
      }

      return(include)
    })

    #tried to wrap these three into one observer and it broke.. look again later
    observeEvent(xaxisInfo()$group, {
      #execute javascript to virtually click outside the dropdown
      js$virtualBodyClick();
    })

    observeEvent(facetInfo()$group, {
      #execute javascript to virtually click outside the dropdown
      js$virtualBodyClick();
    })

    observeEvent(facet2Info()$group, {
      #execute javascript to virtually click outside the dropdown
      js$virtualBodyClick();
    })
