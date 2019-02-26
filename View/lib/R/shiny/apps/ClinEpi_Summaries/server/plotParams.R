  timelineInit <- reactive({
    current <<- callModule(timeline, "timeline", longitudinal.file, metadata.file)
  })

  groupInit <- reactive({
    groupInfo <<- callModule(customGroups, "group", groupLabel = groupLabel, metadata.file = metadata.file, include = groupData, selected = selectedGroup, groupsType = reactive(input$groupsType), groupsTypeID = "input$groupsType", moduleName = "groupInfo", prtcpntView = reactive(prtcpntView$val), timepoints = reactive(current$subset))
      if (is.null(properties)) {
        getMyGroups$val <- selectedGroup()
      } else {
        getMyGroups$val <- properties$selected[properties$input == "groupInfo$group"]
      }
  })

  facetInit <- reactive({
    facetInfo <<- callModule(customGroups, "facet", groupLabel = facetLabel, metadata.file = metadata.file, include = facetData, selected = selectedFacet, groupsType = reactive(input$facetType), groupsTypeID = "input$facetType", moduleName = "facetInfo", prtcpntView = reactive(prtcpntView$val), timepoints = reactive(current$subset))
      if (is.null(properties)) {
        getMyFacet$val <- "custom"
      } else {
        getMyFacet$val <- properties$selected[properties$input == "facetInfo$group"]
      }
  })

  facet2Init <- reactive({
    facet2Info <<- callModule(customGroups, "facet2", groupLabel = facet2Label, metadata.file = metadata.file, include = facet2Data, selected = selectedFacet2, groupsType = reactive(input$facet2Type), groupsTypeID = "input$facet2Type", moduleName = "facet2Info", prtcpntView = reactive(prtcpntView$val), timepoints = reactive(current$subset))
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

  observeEvent(input$prtcpntViewSwitch, {
    if (input$prtcpntViewSwitch == "TRUE" | input$prtcpntViewSwitch == TRUE) {
      prtcpntView$val <- TRUE
    } else {
      prtcpntView$val <- FALSE
    }
  })

    output$xaxis_var <- renderUI({
      if (is.null(longitudinal2)) {
        return()
      }

      if (is.null(properties)) {
        radioButtons(inputId = "xaxisVar",
                     label = NULL,
                     choices = list("My Date Variable" = "dateVar", "My Age Variable" = "ageVar"),
                     selected = "dateVar",
                     inline = TRUE,
                     width = '100%')
      } else {
        radioButtons(inputId = "xaxisVar",
                     label = NULL,
                     choices = list("My Date Variable" = "dateVar", "My Age Variable" = "ageVar"),
                     selected = properties$selected[properties$input == "input$xaxisVar"],
                     inline = TRUE,
                     width = '100%')
      }
    })

    output$xaxis_stp2 <- renderUI({
      if (!contLongitudinal) {
        return()
      }

      myMin <- 2
      myMax <- 40

      if (is.null(properties)) {
        if (uniqueN(studyData[[longitudinal1]]) > 500) {
          mySelected <- 24
        } else {
          mySelected <- 12
          myMax <- 12
        }
      } else {
        mySelected <- properties$selected[properties$input == "input$xaxis_stp2"]
      }

      sliderInput(inputId = "xaxis_stp2",
                  min = myMin,
                  max = myMax,
                  value = mySelected,
                  step = 1,
                  label = "number of bins:")
    })

    output$xaxisBox <- renderUI({
       if (!contLongitudinal) {
         return()
       }

       if (is.null(longitudinal2)) {
         tagList(
           box(width = 6, status = "primary", title = "X-Axis",
                        uiOutput("xaxis_stp2")
                    )
         )
       } else {
         tagList(
           box(width = 6, status = "primary", title = "X-Axis",
                        uiOutput("xaxis_var"),
                        uiOutput("xaxis_stp2")
                    )
         )
       }
    })

    output$groupBox <- renderUI({

      if (!contLongitudinal) {
        box(width = 6, status = "primary", title = "X-Axis",
                     uiOutput("groups_type"),
                     customGroupsUI("group", colWidth = 12)
                 )
      } else {
        box(width = 6, status = "primary", title = "Stratify Line",
                     uiOutput("groups_type"),
                     customGroupsUI("group", colWidth = 12)
                 )
      }
    })

    output$groups_type <- renderUI({
      mySelected <- properties$selected[properties$input == "input$groupsType"]

      if (is.null(properties)) {
          selectInput(inputId = "groupsType",
                      label = NULL,
                      choices = c("All possible" = "direct", "Make my own" = "makeGroups", "None" = "none"),
                      selected = "direct",
                      width = '100%')
      } else {
          selectInput(inputId = "groupsType",
                      label = NULL,
                      choices = c("All possible" = "direct", "Make my own" = "makeGroups", "None" = "none"),
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
        #if (house.file.exists) {
          #htmp <- house.file[, !dates, with = FALSE]
          include <- c("Participant", "Household")
        #} else {
        #  include <- c("Participant")
        #}
      } else {
        include <- c("all")
      }

      return(include)
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
        #if (house.file.exists) {
        #htmp <- house.file[, !dates, with = FALSE]
        include <- c("Participant", "Household")
        #} else {
        #  include <- c("Participant")
        #}
      } else {
        include <- c("all")
      }

      return(include)
    })

    groupLabel <- reactive({
        if (is.null(input$groupsType)) {
          return()
        } else {
          groupsType <- input$groupsType
        }
        longitudinal <- longitudinal1
        if (!is.null(input$xaxisVar)) {
          if (input$xaxisVar == "ageVar") {
            longitudinal <- longitudinal2
          }
        }

        if (contLongitudinal) {
          if (groupsType == "direct") {
            label = "strata for"
          } else {
            label = "strata where:"
          }
        } else {
          if (groupsType == "direct") {
            label = "x-axis categories for"
          } else {
            label = "x-axis category where:"
          }
        }

        return(label)
    })

#figure how to remove dates 
    groupData <- reactive({
      if (is.null(input$groupsType)) {
        return()
      } else {
        groupsType <- input$groupsType
      }

      if (groupsType == "direct") {
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

    selectedGroup <- reactive({
      if (is.null(input$groupsType)) {
        return()
      } else {
        groupsType <- input$groupsType
      }

      if ("EUPATH_0000054" %in% colnames(studyData)) {
        selected <- "EUPATH_0000054"
      } else {
        include <- groupData()
        if (include != "all") {
          temp <- metadata.file[metadata.file$category %in% include]
        } else {
          temp <- metadata.file
        }
        myCols <- colnames(studyData)
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
          if ("EUPATH_0000054" %in% colnames(studyData)) {
            selected <- "EUPATH_0000054"
          } else {
            include <- facetData()
            if (include != "all") {
              temp <- metadata.file[metadata.file$category %in% include]
            } else {
              temp <- metadata.file
            }
            myCols <- colnames(studyData)
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
#message("selected Facet:", selected)
      return(selected)
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
          if ("EUPATH_0000054" %in% colnames(studyData)) {
            selected <- "EUPATH_0000054"
          } else {
            include <- facet2Data()
            if (include != "all") {
              temp <- metadata.file[metadata.file$category %in% include]
            } else {
              temp <- metadata.file
            }
            myCols <- colnames(studyData)
            temp <- temp[temp$source_id %in% myCols]
            parents <- temp$parent
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
      #message("selected Facet:", selected)
      return(selected)
    })

    observeEvent(groupInfo$group, {
      if (length(get_selected(groupInfo$group, format="names")) != 0) {

        mySelected <- get_selected(groupInfo$group, format="names")[[1]]
        myProp <- mySelected[1]
        myParent <- unlist(attributes(mySelected))[length(unlist(attributes(mySelected)))]
        if (length(myParent) != 0) {
          nextGroup <- metadata.file$source_id[metadata.file$property == myProp & metadata.file$parentlabel == myParent]
        } else {
          nextGroup <- metadata.file$source_id[metadata.file$property == myProp & (metadata.file$parentlabel == "null" | metadata.file$parentlabel == "" | is.null(metadata.file$parentlabel))]
        }
        nextGroup <- unique(nextGroup)

        if (length(nextGroup) != 1) {
          message("Warning: non-unique source_ids returned ", nextGroup)
        }

        #if (is.null(getMyGroups$val)) {
          getMyGroups$val <- nextGroup
        #} else if (getMyGroups$val != nextGroup) {
        #  getMyGroups$val <- nextGroup
        #}
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

message("nextFacet: ", nextFacet)
        #if (is.null(getMyFacet$val)) {
          getMyFacet$val <- nextFacet
        #} else if (getMyFacet$val != nextFacet) {
        #  getMyFacet$val <- nextFacet
        #}
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

        message("nextFacet: ", nextFacet)
        #if (is.null(getMyFacet2$val)) {
          getMyFacet2$val <- nextFacet
        #} else if (getMyFacet2$val != nextFacet) {
        #  getMyFacet2$val <- nextFacet
        #}
      }
    })

    output$yaxis <- renderTree({

      longitudinal <- longitudinal1
      if (!is.null(input$xaxisVar)) {
        if (input$xaxisVar == "ageVar") {
          longitudinal <- longitudinal2
        }
      }

      #dont remember why this is
      if (contLongitudinal) {
        include <- c("Observation", "Sample")
      } else {
        include <- c("all")
      }

      outChoiceList <- getUIList(metadata.file = metadata.file, include = include, timepoints.keep = current$subset)

      outChoiceList
    })

    output$choose_yaxis <- renderUI({
      myLabel <- getYaxisLabel()

      tagList(
        div(
          dropdownButton(label=myLabel,
                         status = "default",
                         tags$div(
                           class = "treeContainer",
                           shinyTree("yaxis", search = TRUE)
                         )),
          style="margin-bottom: 10px"
        )
      )
    })

    getYaxisLabel <- reactive({
      myY <- getMyY$val

      if (is.null(myY)) {
        if (is.null(properties)) {
          label <- "Please select one"
        } else {
          myYSourceId <- properties$selected[properties$input == "input$yaxis"]
          label <- metadata.file$property[metadata.file$source_id == myYSourceId]
          getMyY$val <- myYSourceId
        }
      } else {
        label <- metadata.file$property[metadata.file$source_id == myY]
      }

      label
    })

    observeEvent(input$yaxis, {
      if (length(get_selected(input$yaxis, format="names")) != 0) {
        nextY <- metadata.file$source_id[metadata.file$property == get_selected(input$yaxis, format="names")[1][[1]]][1]

        #if (is.null(getMyY$val)) {
          getMyY$val <- nextY
          print("resetting myY")
        #} else if (getMyY$val != nextY) {
        #  getMyY$val <- nextY
        #  print("resetting myY")
        #}
      }
    })

    observeEvent(getMyY$val, {
      #execute javascript to virtually click outside the dropdown
      print("clicking!!!!!!!!!!!")
      js$virtualBodyClick();
    })

    observeEvent(getMyGroups$val, {
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

    output$yaxis_stp1 <- renderUI({
      if (is.null(getMyY$val)) {
        return()
      }
      myY <- getMyY$val
      myPrtcpntView <- prtcpntView$val
      myYSelected <- properties$selected[properties$input == "input$yaxis"]
      mySelected <- properties$selected[properties$input == "input$yaxis_stp1"]

      attrStp1List <- getUIStp1List(metadata.file, myY)
      nums <- getNums(metadata.file)

      dontUseProps <- FALSE
      if (is.null(properties)) {
        dontUseProps <- TRUE
      } else {
        if (myY != myYSelected) {
          dontUseProps = TRUE
        }
      }
      if (myPrtcpntView == TRUE) {
        if (dontUseProps) {
          if (!myY %in% nums$source_id) {
            selectInput(inputId = "yaxis_stp1",
                        label = "are / is",
                        choices = list("always" = "all", "ever" = "any"),
                        selected = "any",
                        width = '100%')
          }
        } else {
          if (!myY %in% nums$source_id) {
            selectInput(inputId = "yaxis_stp1",
                        label = "are / is",
                        choices = list("always" = "all", "ever" = "any"),
                        selected = mySelected,
                        width = '100%')
          }
        }
      } else {
        maxInputs <- length(attrStp1List)
        if (dontUseProps) {
          if (!myY %in% nums$source_id) {
            selectizeInput(inputId = "yaxis_stp1",
                           label = "are / is",
                           choices = attrStp1List,
                           selected = "EUPATH_0000338",
                           width = '100%',
                           multiple = TRUE,
                           options = list(maxItems = maxInputs,
                                          placeholder = '-Selected Items Will Appear Here-'))
          }
        } else {
          if (!myY %in% nums$source_id) {
            selectizeInput(inputId = "yaxis_stp1",
                           label = "are / is",
                           choices = attrStp1List,
                           selected = mySelected,
                           width = '100%',
                           multiple = TRUE,
                           options = list(maxItems = maxInputs,
                                          placeholder = '-Selected Items Will Appear Here-'))
          }
        }
      }


    })

    output$yaxis_stp2 <- renderUI({
      if (is.null(getMyY$val)) {
        return()
      }
      myY <- getMyY$val
      myPrtcpntView <- prtcpntView$val

      myYSelected <- properties$selected[properties$input == "input$yaxis"]
      mySelected <- properties$selected[properties$input == "input$yaxis_stp2"]

      attrStp1List <- getUIStp1List(metadata.file, myY)
      nums <- getNums(metadata.file)

      dontUseProps <- FALSE
      if (is.null(properties)) {
        dontUseProps <- TRUE
      } else {
        if (myY != myYSelected) {
          dontUseProps = TRUE
        }
      }

      if(myPrtcpntView == TRUE) {
        maxInputs <- length(attrStp1List)
        if (dontUseProps) {
          if (!myY %in% nums$source_id) {
            selectizeInput(inputId = "yaxis_stp2",
                           label = NULL,
                           choices = attrStp1List,
                           selected = "EUPATH_0000338",
                           width = '100%',
                           multiple = TRUE,
                           options = list(maxItems = maxInputs,
                                          placeholder = '-Selected Items Will Appear Here-'))
          }
        } else {
          if (!myY %in% nums$source_id) {
            selectizeInput(inputId = "yaxis_stp2",
                           label = NULL,
                           choices = attrStp1List,
                           selected = mySelected,
                           width = '100%',
                           multiple = TRUE,
                           options = list(maxItems = maxInputs,
                                          placeholder = '-Selected Items Will Appear Here-'))
          }
        }
      }

    })

    output$yaxis_stp3 <- renderUI({
      if (is.null(getMyY$val)) {
        return()
      } else {
        myY <- getMyY$val
      }
      nums <- getNums(metadata.file)
      longitudinal <- longitudinal1
      if (!is.null(input$xaxisVar)) {
        if (input$xaxisVar == "ageVar") {
          longitudinal <- longitudinal2
        }
      }
      myYSelected <- properties$selected[properties$input == "input$yaxis"]
      mySelected <- properties$selected[properties$input == "input$yaxis_stp3"]

      dontUseProps <- FALSE
      if (is.null(properties)) {
        dontUseProps <- TRUE
      } else {
        if (myY != myYSelected) {
          dontUseProps = TRUE
        }
      }

      if (dontUseProps) {
        if (myY %in% nums$source_id) {
          if (contLongitudinal) {
            radioButtons(inputId = "yaxis_stp3",
                         label = "Display as:",
                         choices = list("Mean" = "mean", "Smoothed Conditional Mean" = "smooth"),
                         selected = "smooth",
                         width = '100%',
                         inline = TRUE)
          } else {
            #later think up better way. shouldnt show something with only one option. but this is a required input later on. will need if statements below instead.
            radioButtons(inputId = "yaxis_stp3",
                         label = "Display as:",
                         choices = list("Mean" = "mean"),
                         selected = "mean",
                         width = '100%',
                         inline = TRUE)
          }
        } else {
          if (prtcpntView$val == TRUE) {
            if (is.null(input$yaxis_stp2)) {
              return()
            }
          } else {
            if (is.null(input$yaxis_stp1)) {
              return()
            }
          }
          radioButtons(inputId = "yaxis_stp3",
                       label = "Display as:",
                       choices = list("Count" = "count", "Proportion" = "proportion"),
                       selected = "proportion",
                       width = '100%',
                       inline = TRUE)
        }
      } else {
        if (myY %in% nums$source_id) {
          if (contLongitudinal) {
            radioButtons(inputId = "yaxis_stp3",
                         label = "Display as:",
                         choices = list("Mean" = "mean", "Smoothed Conditional Mean" = "smooth"),
                         selected = mySelected,
                         width = '100%',
                         inline = TRUE)
          } else {
            #later think up better way. shouldnt show something with only one option. but this is a required input later on. will need if statements below instead.
            radioButtons(inputId = "yaxis_stp3",
                         label = "Display as:",
                         choices = list("Mean" = "mean"),
                         selected = mySelected,
                         width = '100%',
                         inline = TRUE)
          }
        } else {
          if (prtcpntView$val == TRUE) {
            if (is.null(input$yaxis_stp2)) {
              return()
            }
          } else {
            if (is.null(input$yaxis_stp1)) {
              return()
            }
          }
          radioButtons(inputId = "yaxis_stp3",
                       label = "Display as:",
                       choices = list("Count" = "count", "Proportion" = "proportion"),
                       selected = mySelected,
                       width = '100%',
                       inline = TRUE)
        }
      }

    })

