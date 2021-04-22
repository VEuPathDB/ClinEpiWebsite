  observeEvent(reactiveVal(0), {
    current <<- callModule(timeline, "timeline", longitudinal.file, metadata.file)
  }, once = TRUE)

  observeEvent(input$groupsType, {
      groupInfo <<- callModule(customGroups, "group", groupLabel = reactive(NULL), metadata.file = metadata.file, include = groupData, selected = selectedGroup, groupsType = reactive(input$groupsType), groupsTypeID = "input$groupsType", moduleName = "groupInfo", prtcpntView = reactive(prtcpntView$val), timepoints = reactive(current$subset))
  }, once = TRUE)

  observeEvent(input$facetType, {
      facetInfo <<- callModule(customGroups, "facet", groupLabel = reactive(NULL), metadata.file = metadata.file, include = facetData, selected = selectedFacet, groupsType = reactive(input$facetType), groupsTypeID = "input$facetType", moduleName = "facetInfo", prtcpntView = reactive(prtcpntView$val), timepoints = reactive(current$subset))
  }, once = TRUE)

  observeEvent(input$facet2Type, {
      facet2Info <<- callModule(customGroups, "facet2", groupLabel = reactive(NULL), metadata.file = metadata.file, include = facet2Data, selected = selectedFacet2, groupsType = reactive(input$facet2Type), groupsTypeID = "input$facet2Type", moduleName = "facet2Info", prtcpntView = reactive(prtcpntView$val), timepoints = reactive(current$subset))
  }, once = TRUE)

#  output$prtcpntViewSwitch <- renderUI({
#    if (is.null(isParticipant)) { return() }
#
#    if (isParticipant != TRUE) {
#      tagList(
#        box(width = 6, status = "primary", title = "Unit of Analysis",
#          radioButtons(inputId = "prtcpntViewSwitch",
#                      label = NULL,
#                      choiceNames = c("Participant View", "Observation View"),
#                      choiceValues = c(TRUE, FALSE),
#                      selected = "FALSE",
#                      inline = TRUE)
#        )
#      )
#    }
#  })

#  observeEvent(input$prtcpntViewSwitch, {
#    if (input$prtcpntViewSwitch == "TRUE" | input$prtcpntViewSwitch == TRUE) {
  observeEvent(isParticipant, {
    if (isParticipant) {
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
        if (metadata.file$NUMBER_DISTINCT_VALUES[metadata.file$SOURCE_ID == longitudinal1] > 500) {
          mySelected <- 24
        } else {
          mySelected <- 12
          myMax <- 12
        }
      } else {
        mySelected <- properties$selected[properties$input == "input$xaxis_stp2"]
      }

      numXBins$val <<- mySelected
      sliderInput(inputId = "xaxis_stp2",
                  min = myMin,
                  max = myMax,
                  value = mySelected,
                  step = 1,
                  label = "number of bins:")
    })

    observeEvent(numXBins$val, {

      myMin <- 2
      myMax <- 40

      if (metadata.file$NUMBER_DISTINCT_VALUES[metadata.file$SOURCE_ID == longitudinal1] <= 500) {
        myMax <- 12
      }

      updateSliderInput(session, 
                        inputId = "xaxis_stp2",
                        min = myMin,
                        max = myMax,
                        value = numXBins$val,
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

   typeChoices <- reactive({
     
     label <- "Stratify by a non-longitudinal variable"
 
     if (!isParticipant & !prtcpntView$val) {
       label <- "Stratify by a variable"
     }

     myChoices <- c(label = "direct", "Stratify by a variable you will dichotomize" = "makeGroups", "Do not stratify" = "none")
     names(myChoices)[names(myChoices) == "label"] <- label

     myChoices
   })

    output$groups_type <- renderUI({
      mySelected <- properties$selected[properties$input == "input$groupsType"]

      if (is.null(properties)) {
          selectInput(inputId = "groupsType",
                      label = NULL,
                      choices = typeChoices(),
                      selected = "direct",
                      width = '100%')
      } else {
          selectInput(inputId = "groupsType",
                      label = NULL,
                      choices = typeChoices(),
                      selected = mySelected,
                      width = '100%')
      }

    })

    output$facet_type <- renderUI({
      mySelected <- properties$selected[properties$input == "input$facetType"]
      if (is.null(isParticipant)) { return() }
      

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
      if (isParticipant & !is.null(longitudinal1)) {
        if (facetType == "direct") {
          include <- c("Participant", "Study")
          if (is.null(hlongitudinal1)) {
            include <- c(include, "Household", "Study")
          }
          if (is.null(clongitudinal1)) {
            include <- c(include, "Community", "Study")
          }
        }
      }

      return(include)
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
      if (isParticipant & !is.null(longitudinal1)) {
        if (facet2Type == "direct") {
          include <- c("Participant", "Study")
          if (is.null(hlongitudinal1)) {
            include <- c(include, "Household", "Study")
          }
          if (is.null(clongitudinal1)) {
            include <- c(include, "Community", "Study")
          }
        }
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

    groupData <- reactive({
      if (is.null(input$groupsType)) {
        return()
      } else {
        groupsType <- input$groupsType
      }
      message("groupsType: ", groupsType)
      message("isParticipant: ", isParticipant)
      message("hlongitudinal1:", hlongitudinal1)
      include <- c("all")
      if (isParticipant & !is.null(longitudinal1)) {
        if (groupsType == "direct") {
          include <- c("Participant", "Study")
          if (is.null(hlongitudinal1)) {
            include <- c(include, "Household", "Study")
          }
          if (is.null(clongitudinal1)) {
            include <- c(include, "Community", "Study")
          }
        }
      }
	message("group include: ", include)
      return(include)
    })

    selectedGroup <- reactive({
      if (is.null(input$groupsType)) {
        return()
      } else {
        groupsType <- input$groupsType
      }

      if ("EUPATH_0000054" %in% metadata.file$SOURCE_ID) {
        selected <- "EUPATH_0000054"
      } else {
        include <- groupData()
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

    output$yaxis <- renderTree({
      getMyY$val

      longitudinal <- longitudinal1
      if (!is.null(input$xaxisVar)) {
        if (input$xaxisVar == "ageVar") {
          longitudinal <- longitudinal2
        }
      }

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
                           shinyTree("yaxis", search = TRUE, themeIcon = FALSE, themeDots = FALSE)
                         )),
          style="margin-bottom: 10px"
        )
      )
    })

    getYaxisLabel <- reactive({
      myY <- getMyY$val

      if (is.null(myY)) {
        if (is.null(properties)) {
          if (contLongitudinal) {
            label <- "Select a longitudinal variable"
          } else {
            label <- "Select a variable"
          }
        } else {
          myYSourceId <- properties$selected[properties$input == "input$yaxis"]
          label <- metadata.file$PROPERTY[metadata.file$SOURCE_ID == myYSourceId]
          getMyY$val <- myYSourceId
        }
      } else {
        label <- metadata.file$PROPERTY[metadata.file$SOURCE_ID == myY]
      }

      label
    })

    observeEvent(input$yaxis, {
      if (length(get_selected(input$yaxis, format="slices")) != 0) {
        mySelected <- get_selected(input$yaxis, format="slices")
        mySelected <- unlist(strsplit(names(unlist(mySelected)), ".", fixed=TRUE))
        myProp <- mySelected[length(mySelected)]
        myParent <- mySelected[length(mySelected)-1]
        if (length(myParent) != 0) {
          nextY <- metadata.file$SOURCE_ID[metadata.file$PROPERTY == myProp & metadata.file$PARENTLABEL == myParent]
        } else {
          nextY <- metadata.file$SOURCE_ID[metadata.file$PROPERTY == myProp & (metadata.file$PARENTLABEL == "null" | metadata.file$PARENTLABEL == "" | is.null(metadata.file$PARENTLABEL))]
        }
        nextY <- unique(nextY)

        if (length(nextY) > 1) {
          message("Warning: non-unique source_ids returned ", nextY)
        }
        getMyY$val <- nextY
      }
    })

    observeEvent(getMyY$val, {
      #execute javascript to virtually click outside the dropdown
      print("clicking!!!!!!!!!!!")
      js$virtualBodyClick();
    })

    observeEvent(groupInfo()$group, {
      #execute javascript to virtually click outside the dropdown
      print("clicking!!!!!!!!!!!")
      js$virtualBodyClick();
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
          if (!myY %in% nums$SOURCE_ID) {
            selectInput(inputId = "yaxis_stp1",
                        label = NULL,
                        choices = list("always" = "all", "at least once" = "any"),
                        selected = "any",
                        width = '100%')
          }
        } else {
          if (!myY %in% nums$SOURCE_ID) {
            selectInput(inputId = "yaxis_stp1",
                        label = NULL,
                        choices = list("always" = "all", "at least once" = "any"),
                        selected = mySelected,
                        width = '100%')
          }
        }
      } else {
        maxInputs <- length(attrStp1List)
        if (dontUseProps) {
          if (!myY %in% nums$SOURCE_ID) {
            selectizeInput(inputId = "yaxis_stp1",
                           label = NULL,
                           choices = attrStp1List,
                           selected = "EUPATH_0000338",
                           width = '100%',
                           multiple = TRUE,
                           options = list(maxItems = maxInputs,
                                          placeholder = '-Selected Items Will Appear Here-'))
          }
        } else {
          if (!myY %in% nums$SOURCE_ID) {
            selectizeInput(inputId = "yaxis_stp1",
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
          if (!myY %in% nums$SOURCE_ID) {
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
          if (!myY %in% nums$SOURCE_ID) {
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
        if (myY %in% nums$SOURCE_ID) {
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
        if (myY %in% nums$SOURCE_ID) {
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

