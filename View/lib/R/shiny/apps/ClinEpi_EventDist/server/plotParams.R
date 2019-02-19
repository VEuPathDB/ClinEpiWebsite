  timelineInit <- reactive({
    current <<- callModule(timeline, "timeline", longitudinal.file, metadata.file)
  })

  xaxisInit <- reactive({
    xaxisInfo <<- callModule(customGroups, "group", groupLabel = groupLabel, metadata.file = metadata.file, include = groupData, selected = selectedGroup, groupsType = reactive(input$xaxis), groupsTypeID = "input$xaxis", moduleName = "xaxisInfo", prtcpntView = reactive(prtcpntView$val))
      if (is.null(properties)) {
        getMyX$val <- selectedGroup()
      } else {
        getMyX$val <- properties$selected[properties$input == "xaxisInfo$group"]
      }
  })

  facetInit <- reactive({
    facetInfo <<- callModule(customGroups, "facet", groupLabel = facetLabel, metadata.file = metadata.file, include = facetData, selected = selectedFacet, groupsType = reactive(input$facetType), groupsTypeID = "input$facetType", moduleName = "facetInfo", prtcpntView = reactive(prtcpntView$val))
      if (is.null(properties)) {
        getMyFacet$val <- selectedFacet()
      } else {
        getMyFacet$val <- properties$selected[properties$input == "facetInfo$group"]
      }
  })

  facet2Init <- reactive({
    facet2Info <<- callModule(customGroups, "facet2", groupLabel = facet2Label, metadata.file = metadata.file, include = facet2Data, selected = selectedFacet2, groupsType = reactive(input$facet2Type), groupsTypeID = "input$facet2Type", moduleName = "facet2Info", prtcpntView = reactive(prtcpntView$val))
      if (is.null(properties)) {
        getMyFacet2$val <- selectedFacet2()
      } else {
        getMyFacet2$val <- properties$selected[properties$input == "facet2Info$group"]
      }
  })

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

  #in case input does not get rendered (i.e. is null) set prtcpntView$val TRUE globally and override with input
  observeEvent(input$prtcpntViewSwitch, {
    if (input$prtcpntViewSwitch == "TRUE" | input$prtcpntViewSwitch == TRUE) {
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
                      choices = c("All possible" = "direct"),
                      selected = "direct",
                      width = '100%')
        } else {
          selectInput(inputId = "xaxis",
                      label = NULL,
                      choices = c("All possible" = "direct"),
                      selected = mySelected,
                      width = '100%')
        }
    })

    output$facet_type <- renderUI({
      mySelected <- properties$selected[properties$input == "input$facetType"]

      if (is.null(properties)) {
        if (isParticipant) {
          selectInput(inputId = "facetType",
                      label = NULL,
                      choices = c("All possible" = "direct", "Make my own" = "makeGroups", "None" = "none"),
                      selected = "direct",
                      width = '100%')
        } else {
          selectInput(inputId = "facetType",
                      label = NULL,
                      choices = c("All possible" = "direct", "Make my own" = "makeGroups", "None" = "none"),
                      selected = "makeGroups",
                      width = '100%')
        }
      } else {
        selectInput(inputId = "facetType",
                    label = NULL,
                    choices = c("All possible" = "direct", "Make my own" = "makeGroups", "None" = "none"),
                    selected = mySelected,
                    width = '100%')
      }
    })

    output$facet2_type <- renderUI({
      mySelected <- properties$selected[properties$input == "input$facet2Type"]

      if (is.null(properties)) {
        selectInput(inputId = "facet2Type",
                    label = NULL,
                    choices = c("All possible" = "direct", "Make my own" = "makeGroups", "None" = "none"),
                    selected = "none",
                    width = '100%')
      } else {
        selectInput(inputId = "facet2Type",
                    label = NULL,
                    choices = c("All possible" = "direct", "Make my own" = "makeGroups", "None" = "none"),
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

      if ("EUPATH_0000338" %in% metadata.file$source_id) {
        selected <- "EUPATH_0000338"
      } else {
        include <- groupData()
        if (include != "all") {
          temp <- metadata.file[metadata.file$category %in% include]
        } else {
          temp <- metadata.file
        }
        myCols <- metadata.file$source_id[!is.na(metadata.file$number_distinct_values)]
        temp <- temp[temp$source_id %in% myCols]
        parents <- temp$parentlabel
        leaves <- temp[!temp$property %in% parents]
        leaves <- leaves[order(leaves$property),]
        leaves <- leaves$source_id
        #remove dates
        dates <- getDates(metadata.file)$source_id
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
          if ("EUPATH_0000054" %in% metadata.file$source_id[!is.na(metadata.file$number_distinct_values)]) {
            selected <- "EUPATH_0000054"
          } else {
            include <- facetData()
            if (include != "all") {
              temp <- metadata.file[metadata.file$category %in% include]
            } else {
              temp <- metadata.file
            }
            myCols <- metadata.file$source_id[!is.na(metadata.file$number_distinct_values)]
            temp <- temp[temp$source_id %in% myCols]
            parents <- temp$parentlabel
            leaves <- temp[!temp$property %in% parents]
            leaves <- leaves[order(leaves$property),]
            leaves <- leaves$source_id
            #remove dates
            dates <- getDates(metadata.file)$source_id
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

      if (facetType == "direct") {
        dates <- getDates(metadata.file)$source_id
        #ptmp <- prtcpnt.file[, !dates, with = FALSE]
        if ("Household" %in% metadata.file$category) {
          #htmp <- house.file[, !dates, with = FALSE]
          include <- c("Participant", "Household")
        } else {
          include <- c("Participant")
        }
      } else {
        include <- c("all")
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
          if ("EUPATH_0000054" %in% metadata.file$source_id[!is.na(metadata.file$number_distinct_values)]) {
            selected <- "EUPATH_0000054"
          } else {
            include <- facet2Data()
            if (include != "all") {
              temp <- metadata.file[metadata.file$category %in% include]
            } else {
              temp <- metadata.file
            }
            myCols <- metadata.file$source_id[!is.na(metadata.file$number_distinct_values)]
            temp <- temp[temp$source_id %in% myCols]
            parents <- temp$parentlabel
            leaves <- temp[!temp$property %in% parents]
            leaves <- leaves[order(leaves$property),]
            leaves <- leaves$source_id
            #remove dates
            dates <- getDates(metadata.file)$source_id
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

      if (facet2Type == "direct") {
        dates <- getDates(metadata.file)$source_id
        #ptmp <- prtcpnt.file[, !dates, with = FALSE]
        if ("Household" %in% metadata.file$category) {
          #htmp <- house.file[, !dates, with = FALSE]
          include <- c("Participant", "Household")
        } else {
          include <- c("Participant")
        }
      } else {
        include <- c("all")
      }

      return(include)
    })

    observeEvent(xaxisInfo$group, {
      if (length(get_selected(xaxisInfo$group, format="names")) != 0) {

        mySelected <- get_selected(xaxisInfo$group, format="names")[[1]]
        myProp <- mySelected[1]
        myParent <- unlist(attributes(mySelected))[length(unlist(attributes(mySelected)))]
        if (length(myParent) != 0) {
          nextX <- metadata.file$source_id[metadata.file$property == myProp & metadata.file$parentlabel == myParent]
        } else {
          nextX <- metadata.file$source_id[metadata.file$property == myProp & (metadata.file$parentlabel == "null" | metadata.file$parentlabel == "" | is.null(metadata.file$parentlabel))]
        }
        nextX <- unique(nextX)

        if (length(nextX) != 1) {
          message("Warning: non-unique source_ids returned ", nextX)
        }

        getMyX$val <- nextX
      }
    })

    observeEvent(facetInfo$group, {
      if (length(get_selected(facetInfo$group, format="names")) != 0) {

        mySelected <- get_selected(facetInfo$group, format="names")[[1]]
        myProp <- mySelected[1]
        myParent <- unlist(attributes(mySelected))[length(unlist(attributes(mySelected)))]
        if (length(myParent) != 0) {
          nextFacet <- metadata.file$source_id[metadata.file$property == myProp & metadata.file$parentlabel == myParent]
        } else {
          nextFacet <- metadata.file$source_id[metadata.file$property == myProp & (metadata.file$parentlabel == "null" | metadata.file$parentlabel == "" | is.null(metadata.file$parentlabel))]
        }
        nextFacet <- unique(nextFacet)
        if (length(nextFacet) != 1) {
          message("Warning: non-unique source_ids returned ", nextFacet)
        }

        getMyFacet$val <- nextFacet
      }
    })

    observeEvent(facet2Info$group, {
      if (length(get_selected(facet2Info$group, format="names")) != 0) {

        mySelected <- get_selected(facet2Info$group, format="names")[[1]]
        myProp <- mySelected[1]
        myParent <- unlist(attributes(mySelected))[length(unlist(attributes(mySelected)))]
        if (length(myParent) != 0) {
          nextFacet <- metadata.file$source_id[metadata.file$property == myProp & metadata.file$parentlabel == myParent]
        } else {
          nextFacet <- metadata.file$source_id[metadata.file$property == myProp & (metadata.file$parentlabel == "null" | metadata.file$parentlabel == "" | is.null(metadata.file$parentlabel))]
        }
        nextFacet <- unique(nextFacet)

        if (length(nextFacet) != 1) {
          message("Warning: non-unique source_ids returned ", nextFacet)
        }

        getMyFacet2$val <- nextFacet
      }
    })

    #tried to wrap these three into one observer and it broke.. look again later
    observeEvent(getMyX$val, {
      #execute javascript to virtually click outside the dropdown
      print("clicking!!!!!!!!!!!")
      js$virtualBodyClick();
    })

    observeEvent(getMyFacet$val, {
      #execute javascript to virtually click outside the dropdown
      print("clicking!!!!!!!!!!!")
      js$virtualBodyClick();
    })

    observeEvent(getMyFacet2$val, {
      #execute javascript to virtually click outside the dropdown
      print("clicking!!!!!!!!!!!")
      js$virtualBodyClick();
    })
