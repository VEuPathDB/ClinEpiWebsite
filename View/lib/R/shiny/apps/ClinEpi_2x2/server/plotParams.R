  observeEvent(reactiveVal(0), {
    current <<- callModule(timeline, "timeline", longitudinal.file, metadata.file)
  }, once = TRUE)

  observeEvent(reactiveVal(0), {
    attrInfo <<- callModule(customGroups, "attr", groupLabel = reactive(NULL), metadata.file = metadata.file, include = reactive(c("all")), selected = selectedAttr, moduleName = "attrInfo", prtcpntView = reactive(prtcpntView$val), timepoints = reactive(current$subset))
  }, once = TRUE)

  observeEvent(reactiveVal(0), {
    outInfo <<- callModule(customGroups, "out", groupLabel = reactive(NULL), include = reactive(c("all")), metadata.file = metadata.file, selected = reactive("custom"), moduleName = "outInfo", prtcpntView = reactive(prtcpntView$val), timepoints = reactive(current$subset))
  }, once = TRUE)

  observeEvent(input$facetType, {
    facetInfo <<- callModule(customGroups, "facet", groupLabel = reactive(NULL), metadata.file = metadata.file, include = facetData, selected = selectedFacet, groupsType = reactive(input$facetType), groupsTypeID = "input$facetType", moduleName = "facetInfo", prtcpntView = reactive(prtcpntView$val), timepoints = reactive(current$subset))
  }, once = TRUE)

  observeEvent(input$facet2Type, {
     facet2Info <<- callModule(customGroups, "facet2", groupLabel = reactive(NULL), metadata.file = metadata.file, include = facet2Data, selected = selectedFacet2, groupsType = reactive(input$facet2Type), groupsTypeID = "input$facet2Type", moduleName = "facet2Info", prtcpntView = reactive(prtcpntView$val), timepoints = reactive(current$subset))
  }, once = TRUE)

  output$prtcpntViewSwitch <- renderUI({
    if (isParticipant != TRUE) {
      tagList(
        box(width = 6, status = "primary", title = "Unit of Analysis",
          radioButtons(inputId = "prtcpntViewSwitch",
                      label = NULL,
                      choiceNames = c("Participant View", "Observation View"),
                      choiceValues = c(TRUE, FALSE),
                      selected = "FALSE",
                      inline = TRUE)
        )
      )
    }
  })

  observeEvent(input$prtcpntViewSwitch, {
    if (input$prtcpntViewSwitch == "TRUE" | input$prtcpntViewSwitch == TRUE) {
      prtcpntView$val <- TRUE
    } else {
      prtcpntView$val <- FALSE
    }
  })

  selectedAttr <- reactive({
    if ("EUPATH_0000338" %in% metadata.file$SOURCE_ID) {
      selected <- "EUPATH_0000338"
    } else {
        parents <- metadata.file$PARENTLABEL
        leaves <- metadata.file[!metadata.file$PROPERTY %in% parents]
        leaves <- leaves[order(leaves$PROPERTY),]
        leaves <- leaves$SOURCE_ID
        #remove dates
        dates <- getDates(metadata.file)$SOURCE_ID
        leaves <- leaves[!leaves %in% dates]
        selected <- leaves[1]
    }
    return(selected)
  })

    #tried to wrap these three into one observer and it broke.. look again later
    observeEvent(attrInfo()$group, {
      #execute javascript to virtually click outside the dropdown
      print("clicking!!!!!!!!!!!")
      js$virtualBodyClick();
    })

    observeEvent(outInfo()$group, {
      #execute javascript to virtually click outside the dropdown
      print("clicking!!!!!!!!!!!")
      js$virtualBodyClick();
    })

    output$facet_type <- renderUI({
      mySelected <- properties$selected[properties$input == "input$facetType"]

      if (is.null(properties)) {
        if (isParticipant) {
          selectInput(inputId = "facetType",
                      label = NULL,
                      choices = c("Select an existing non-longitudinal variable to stratify on" = "direct", "Select a variable to transform into binary categories" = "makeGroups", "Do not stratify" = "none"),
                      selected = "direct",
                      width = '100%')
        } else {
          selectInput(inputId = "facetType",
                      label = NULL,
                      choices = c("Select an existing non-longitudinal variable to stratify on" = "direct", "Select a variable to transform into binary categories" = "makeGroups", "Do not stratify" = "none"),
                      selected = "makeGroups",
                      width = '100%')
        }
      } else {
        selectInput(inputId = "facetType",
                    label = NULL,
                    choices = c("Select an existing non-longitudinal variable to stratify on" = "direct", "Select a variable to transform into binary categories" = "makeGroups", "Do not stratify" = "none"),
                    selected = mySelected,
                    width = '100%')
      }
    })

    output$facet2_type <- renderUI({
      mySelected <- properties$selected[properties$input == "input$facet2Type"]

      if (is.null(properties)) {
        selectInput(inputId = "facet2Type",
                    label = NULL,
                    choices = c("Select an existing non-longitudinal variable to stratify on" = "direct", "Select a variable to transform into binary categories" = "makeGroups", "Do not stratify" = "none"),
                    selected = "none",
                    width = '100%')
      } else {
        selectInput(inputId = "facet2Type",
                    label = NULL,
                    choices = c("Select an existing non-longitudinal variable to stratify on" = "direct", "Select a variable to transform into binary categories" = "makeGroups", "Do not stratify" = "none"),
                    selected = mySelected,
                    width = '100%')
      }
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
          if ("EUPATH_0000054" %in% metadata.file$SOURCE_ID) {
            selected <- "EUPATH_0000054"
          } else {
            include <- facetData()
            if (include != "all") {
              temp <- metadata.file[metadata.file$CATEGORY %in% include]
            } else {
              temp <- metadata.file
            }
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
      if (isParticipant) {
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
        #selected <- "custom"
        selected <- "custom"
      } else if (facet2Type == "makeGroups") {
        if (isParticipant) {
          if ("EUPATH_0000054" %in% metadata.file$SOURCE_ID) {
            selected <- "EUPATH_0000054"
          } else {
            include <- facet2Data()
            if (include != "all") {
              temp <- metadata.file[metadata.file$CATEGORY %in% include]
            } else {
              temp <- metadata.file
            }
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
      if (isParticipant) {
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

    observeEvent(facetInfo()$group, {
      #execute javascript to virtually click outside the dropdown
      print("clicking!!!!!!!!!!!")
      js$virtualBodyClick();
    })

    observeEvent(facet2Info()$group, {
      #execute javascript to virtually click outside the dropdown
      print("clicking!!!!!!!!!!!")
      js$virtualBodyClick();
    })
