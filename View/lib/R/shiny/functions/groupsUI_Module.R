# shiny module to create the 5 ui necessary for making custom groups
#check it has access to the functions files. source those in global.R before this file, rather than in the server.R file
#server logic should return inputs and range info

#fix ui options. all possible facets shows more steps than necessary.
#check out the width of cols. its not consistent.

customGroupsUI <- function(id, colWidth = 6) {
  #need a namespace
  ns <- NS(id)
  
  #here use uiOutput and maybe also taglist.. not sure yet how to get it into a column
  tagList(
    column(colWidth, align = "left",
           fluidRow(
             uiOutput(ns("choose_group"))
           ),
           fluidRow(
             uiOutput(ns("choose_stp1"))
           ),
           fluidRow(
             uiOutput(ns("choose_stp2"))
           ),
           fluidRow(
             uiOutput(ns("choose_stp3"))
           ),
           fluidRow(
             uiOutput(ns("choose_stp4"))
           )
    )
  )
}

#make sure this returns inputs and range info 
customGroups <- function(input, output, session, groupLabel = "Name Me!!", metadata.file, include, selected = reactive("custom"), groupsType = reactive("makeGroups"), groupsTypeID = NULL, moduleName, prtcpntView = reactive(NULL), timepoints = reactive(NULL)) {
  ns <- session$ns
  needTreeUpdate <- reactiveValues()
  needTreeUpdate$val <- FALSE

  force(timepoints())
  force(prtcpntView())
  force(groupsType())
  force(include())
  force(selected())

  propUrl <<- getPropertiesUrl(session) 
  message("\n", Sys.time(), " functions/groupsUI_Module.R: propUrl: ", propUrl)
  properties <<- suppressWarnings(try(fread(propUrl)))
  if (length(properties) > 0) {
    message(Sys.time(), " functions/groupsUI_Module.R: reading properties...")
    if (grepl("Error", properties)) {
      message(Sys.time(), " functions/groupsUI_Module.R: Error! properties will not be used")
      properties <<- NULL
    } else {message(Sys.time(), " functions/groupsUI_Module.R: properties read:\n", properties)}
  } else {
    message(Sys.time(), " functions/groupsUI_Module.R: no properties! new analysis")
    properties <<- NULL
  } 


  groupRange <- reactiveValues()
  getMyGroups <- reactiveValues() 

  obsView <- reactive({
    obsView <- FALSE
    if (!is.null(prtcpntView())) {
      if (!prtcpntView() == TRUE) {
        obsView <- TRUE
      }   
    }
  
    obsView
  })
 
  setGroupVals <- reactive({
    if (is.null(getMyGroups$val)) { return() }
    if (length(getMyGroups$val) == 0) { return() }     

    myGroup <- getMyGroups$val
    nums <- getNums(metadata.file)
    dates <- getDates(metadata.file)
   
    if (myGroup %in% nums$SOURCE_ID | myGroup %in% dates$SOURCE_ID) {
      
      groupRange$myMin <- as.numeric(metadata.file$MIN[metadata.file$SOURCE_ID == myGroup]) 

      groupRange$myMax <- as.numeric(metadata.file$MAX[metadata.file$SOURCE_ID == myGroup])
      
      if (myGroup %in% nums$SOURCE_ID) {
        groupRange$mean <- as.numeric(metadata.file$AVERAGE[metadata.file$SOURCE_ID == myGroup])
      } else if (myGroup %in% dates$SOURCE_ID) {
        groupRange$startDate <- metadata.file$lower_quantile[metadata.file$SOURCE_ID == myGroup]
        groupRange$endDate <- metadata.file$upper_quantile[metadata.file$SOURCE_ID == myGroup]
        if (is.null(groupRange$myMin)) {
          groupRange$myMin <- groupRange$myMax
        } 
        groupRange$myMin <- as.Date(groupRange$myMin)
        groupRange$myMax <- as.Date(groupRange$myMax)
      }
    }
    
  })
  
  #make sure ranges are updated when attr or out change
  observeEvent(getMyGroups$val, setGroupVals())
 
  observeEvent(groupsType(), {
    output$group <- renderTree({
      attrChoiceList <- NULL
      
      if (isolate(groupsType()) == "makeGroups") {
        attrChoiceList <- suppressWarnings(getUIList(metadata.file = metadata.file, include = isolate(include()), timepoints.keep = isolate(timepoints())))
      } else if (isolate(groupsType()) == "direct") {
        attrChoiceList <- suppressWarnings(getUIList(metadata.file = metadata.file, include = isolate(include()), maxLevels = 12, timepoints.keep = isolate(timepoints())))
      }

      attrChoiceList
    })
  }, once = TRUE)

  observeEvent({groupsType() 
                needTreeUpdate$val}, {
    if (groupsType() == "makeGroups") {
      attrChoiceList <- suppressWarnings(getUIList(metadata.file = metadata.file, include = include(), timepoints.keep = timepoints()))
    } else if (groupsType() == "direct") {
      attrChoiceList <- suppressWarnings(getUIList(metadata.file = metadata.file, include = include(), maxLevels = 12, timepoints.keep = timepoints()))
    } else {
      attrChoiceList <- NULL 
    }
    
    if (!is.null(attrChoiceList)) {    
      updateTree(session, "group", data = attrChoiceList)
    }
    needTreeUpdate$val <<- FALSE
  }, ignoreInit = TRUE)
  
  output$choose_group <- renderUI({
    if (is.null(groupsType())) {
      return()
    }
    if (groupsType() != "makeGroups" & groupsType() != "direct") {
      return()
    }
    
    myGroupBtnLabel <- getGroupBtnLabel()
    if (is.null(myGroupBtnLabel)) {
      return()
    }

    if (is.null(include())) { return() }
    
    tagList(
      div(
        div(
          tags$b(groupLabel()),
          style="margin-bottom: 5px;"
        ),
        dropdownButton(label=myGroupBtnLabel, 
                       status = "default", 
                       tags$div(
                         class = "treeContainer",
                         shinyTree(ns("group"), search = TRUE, themeIcon=FALSE, themeDots = FALSE)
                       )),
        style="margin-bottom: 10px"
      )
    )
  })

  observeEvent(input$group, {
    if (length(get_selected(input$group, format="slices")) != 0) {

      mySelected <- get_selected(input$group, format = "slices")
      mySelected <- unlist(strsplit(names(unlist(mySelected)), ".", fixed=TRUE))
      myProp <- mySelected[length(mySelected)]
      myParent <- mySelected[length(mySelected)-1]
      if (length(myParent) != 0) {
          nextGroup <- metadata.file$SOURCE_ID[metadata.file$PROPERTY == myProp & metadata.file$PARENTLABEL == myParent]
        } else {
          nextGroup <- metadata.file$SOURCE_ID[metadata.file$PROPERTY == myProp & (metadata.file$PARENTLABEL == "null" | metadata.file$PARENTLABEL == "" | is.null(metadata.file$PARENTLABEL))]
        }
      nextGroup <- unique(nextGroup)

      if (length(nextGroup) > 1) {
        message("Warning: non-unique source_ids returned ", nextGroup)
      }   
 
      getMyGroups$val <- nextGroup
      needTreeUpdate$val <<- TRUE
    }
  })

  getGroupBtnLabel <- reactive({
    
    if (is.null(properties)) {
      propUrl <<- getPropertiesUrl(session)
      properties <- suppressWarnings(try(fread(propUrl)))
    }

    if (length(properties) > 0) {
      if (grepl("Error", properties)[1]) {
        properties <- NULL
      }
    } else {
      properties <- NULL
    }

    myGroup <- getMyGroups$val
    #if (!is.null(myGroup)) {
    #  category <- metadata.file$CATEGORY[metadata.file$SOURCE_ID == myGroup]
    #  if (category %in% c("Observation", "Sample") & groupsType() == "direct") {
    #    myGroup <- NULL
    #  }
    #}

    if (!is.null(groupsTypeID)) {
      groupsTypeSelected <- properties$selected[properties$input == groupsTypeID]
    } else {
      groupsTypeSelected <- NULL
    } 
      dontUseProps <- FALSE
      if (is.null(properties)) {
        dontUseProps <- TRUE
      } else {
          if (!is.null(groupsType()) & !is.null(groupsTypeSelected)) {
	    if (!is.na(groupsType()) & !is.na(groupsTypeSelected)) {
              if (groupsTypeSelected != groupsType()) {
                dontUseProps <- TRUE
	      }
            }
          } else {
            return
          }
      }
      if (dontUseProps) {
        mySelected = selected()
      } else {
        mySelected = properties$selected[properties$input == paste0(moduleName, "$group")]
      }

    if (length(mySelected) == 0) { mySelected <- "custom" }
    if (is.na(mySelected)) { mySelected <- "custom" } 
	
    if (is.null(myGroup)) {
      label <- metadata.file$PROPERTY[metadata.file$SOURCE_ID == mySelected]
      getMyGroups$val <- mySelected
      if (is.null(label)) {
        label <- mySelected
      }
    } else {
      label <- metadata.file$PROPERTY[metadata.file$SOURCE_ID == myGroup]
    }
    label
  })

  output$choose_stp1 <- renderUI({
    if (is.null(getMyGroups$val)) {
      return()
    }
 
    if (groupsType() != "makeGroups") {
      return()
    }

    myGroup <- getMyGroups$val
    nums <- getNums(metadata.file)
    dates <- getDates(metadata.file)
    
    myGroupSelected <- properties$selected[properties$input == paste0(moduleName, "$group")]
    mySelected <- properties$selected[properties$input == paste0(moduleName, "$group_stp1")]
    groupsTypeSelected <- properties$selected[properties$input == groupsTypeID]  

    attrStp1List <- getUIStp1List(metadata.file, myGroup)
    observations <- metadata.file$SOURCE_ID[metadata.file$CATEGORY %in% c("Observation", "Sample")]

    dontUseProps <- FALSE
    if (is.null(properties)) {
      dontUseProps <- TRUE
    } else {
      if (length(myGroupSelected) != 0) {
        if (myGroupSelected != myGroup) {
          dontUseProps <- TRUE
	}
      }
      if (length(groupsTypeID) != 0) {
        if (length(groupsTypeSelected) != 0) {
          if (groupsType() != groupsTypeSelected | myGroup %in% dates$SOURCE_ID) {
            dontUseProps <- TRUE
          }
        }
      }
      if (length(mySelected) == 0) {
	dontUseProps <- TRUE
      }
    }

    obs <- FALSE
    anthro <- FALSE
    if (dontUseProps) {
      if (myGroup %in% observations & !obsView()) {
        obs <- TRUE
        mySelected = "any"
      } else {
        if (myGroup %in% nums$SOURCE_ID) {
          mySelected = "greaterThan"
        } else if (myGroup %in% dates$SOURCE_ID) {
          mySelected1 = groupRange$startDate
          mySelected2 = groupRange$endDate
        } else {
          if (length(attrStp1List) == 2) {
            if (any(attrStp1List %in% "Yes")) {
              mySelected = "Yes"
            } else if (any(attrStp1List %in% "TRUE")) {
              mySelected = "TRUE"
            } else {
              mySelected = NULL
            }
          } else {
            mySelected = NULL
          }
        }
      }

    } else {
      if (mySelected == 'all' | mySelected == 'any') {
        obs <- TRUE
      }
    }

    if (anthro) {
          selectInput(inputId = ns("group_stp1"),
                      label = "where",
                      choices = list('the change in value over time selected' = 'delta', 'more than the following percent of days' = 'percentDays', "a direct comparison" = "direct"),
                      selected = "delta",
                      width = '100%')
    } else if (obs){
       selectInput(inputId = ns("group_stp1"),
                   label = "are / is",
                   choices = list("always" = "all", "ever" = "any"),
                   selected = mySelected,
                   width = '100%')
    } else {
      if (myGroup %in% nums$SOURCE_ID) {
        selectInput(inputId = ns("group_stp1"),
                    label = "is",
                    choices = list('<' = 'lessThan', '>' = 'greaterThan', '=' = 'equals'),
                    selected = mySelected,
                    width = '100%')
      } else if (myGroup %in% dates$SOURCE_ID) {
        mySelected1 <- properties$selected[properties$input == paste0(moduleName, "$group_stp1[1]")]
        mySelected2 <- properties$selected[properties$input == paste0(moduleName, "$group_stp1[2]")]
        dateRangeInput(inputId = ns("group_stp1"),
                       label = "is between",
                       start = mySelected1, end = mySelected2,
                       min = groupRange$myMin, max = groupRange$myMax,
                       separator = "and",
                       startview = "year")
      } else {
        #let select multiple
        maxInputs <- length(attrStp1List) -1
        if (maxInputs == 1) {
          selectInput(inputId = ns("group_stp1"),
                      label = "are / is",
                      choices = attrStp1List,
                      selected = mySelected,
                      width = '100%')
        } else {
          selectizeInput(inputId = ns("group_stp1"),
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
  
  output$choose_stp2 <- renderUI ({
    assign(paste0(moduleName, "$group_stp1"), input$group_stp1, pos=parent.frame())
    if (is.null(input$group_stp1)) {
       return()
    }
    if (input$group_stp1 == "") {
      return()
    }
    if (length(input$group_stp1) == 0) {
      return()
    }
    if (groupsType() != "makeGroups") {
      return()
    }
    
    myStp1Val <- input$group_stp1
    myStp1Selected <- properties$selected[properties$input == paste0(moduleName, "$group_stp1")]
    mySelected <- properties$selected[properties$input == paste0(moduleName, "$group_stp2")]    
    myGroup <- getMyGroups$val 

    nums <- getNums(metadata.file)
    dates <- getDates(metadata.file)
    attrStp1List <- getUIStp1List(metadata.file, myGroup)

    numeric <- c("lessThan", "greaterThan", "equals")
    anthro <- c("percentDays", "delta", "direct")
    obs <- c("all", "any")

    dontUseProps <- FALSE
    if (is.null(properties)) {
      dontUseProps <- TRUE
    } else {
      if (length(myStp1Selected) != 0) {
	if (myStp1Selected != myStp1Val) {
          dontUseProps <- TRUE
	}
      } else {
	dontUseProps <- TRUE
      }
    }
    
    if (dontUseProps) {
      if (myStp1Val %in% anthro) {
        if (myStp1Val == 'percentDays') {
          mySelected = 50
        } else {
          mySelected = "greaterThan"
        }
      } else if (myStp1Val %in% obs) {
        if (myGroup %in% nums$SOURCE_ID) {
          mySelected = "greaterThan"
        } else if (myGroup %in% dates$SOURCE_ID) {
          mySelected1 = groupRange$startDate
          mySelected2 = groupRange$endDate
        } else {
          if (length(attrStp1List) == 2) {
            if (any(attrStp1List %in% "Yes")) {
              mySelected = "Yes"
            } else if (any(attrStp1List %in% "TRUE")) {
              mySelected = "TRUE"
            } else {
              mySelected = NULL
            }
          } else {
            mySelected = NULL
          }
        }
      } else {
        if (myStp1Val %in% numeric) {
          mySelected = groupRange$mean
        }
      }
    }

    if (myStp1Val %in% anthro) {
      if (myStp1Val == 'percentDays') {
        numericInput(inputId = ns("group_stp2"),
                     label = NULL,
                     value = mySelected,
                     min = 0,
                     max = 100,
                     width = '100%')
      } else {
        selectInput(inputId = ns("group_stp2"),
                    label = "is",
                    choices = list('<' = 'lessThan', '>' = 'greaterThan', '=' = 'equals'),
                    selected = mySelected,
                    width = '100%')
      }
    } else if (myStp1Val %in% obs) {
      if (myGroup %in% nums$SOURCE_ID) {
        selectInput(inputId = ns("group_stp2"),
                    label = NULL,
                    choices = list('<' = 'lessThan', '>' = 'greaterThan', '=' = 'equals'),
                    selected = mySelected,
                    width = '100%')
      } else if (myGroup %in% dates$SOURCE_ID) {
        mySelected1 <- properties$selected[properties$input == paste0(moduleName, "$group_stp1[1]")]
        mySelected2 <- properties$selected[properties$input == paste0(moduleName, "$group_stp1[2]")]
        dateRangeInput(inputId = ns("group_stp2"),
                       label = "between",
                       start = mySelected1, end = mySelected2,
                       min = groupRange$myMin, max = groupRange$myMax,
                       separator = "and",
                       startview = "year")
      } else {
        #for obs vairs that are strings, want a multiplick that has a max of one less than the total choices
        maxInputs <- length(attrStp1List) -1
        selectizeInput(inputId = ns("group_stp2"),
                    label = NULL,
                    choices = attrStp1List,
                    selected = mySelected,
                    width = '100%',
                    multiple = TRUE,
                    options = list(maxItems = maxInputs,
                                   placeholder = '-Selected Items Will Appear Here-'))
      }
    } else {
      if (myStp1Val %in% numeric) {
        #just going to set default value to whatever the mean is
        sliderInput(ns("group_stp2"), NULL,
                  min = groupRange$myMin, max = groupRange$myMax, value = mySelected, step = .1, width = '100%')
      }
    }

  })
  
  output$choose_stp3 <- renderUI ({
    if (is.null(input$group_stp1)) {
      return()
    }
    assign(paste0(moduleName, "$group_stp2"), input$group_stp2, pos=parent.frame())
    if (is.null(input$group_stp2)) {
      return()
    }
    if (groupsType() != "makeGroups") {
      return()
    }

    myStp1Val <- input$group_stp1
    myStp1Selected <- properties$selected[properties$input == paste0(moduleName, "$group_stp1")]
    mySelected <- properties$selected[properties$input == paste0(moduleName, "$group_stp3")]
    myStp2Val <- input$group_stp2

    nums <- getNums(metadata.file)
    dates <- getDates(metadata.file)

    anthro <- c("delta", "direct", "percentDays")
    numeric <- c("lessThan", "greaterThan", "equals")
    obs <- c("all", "any")
    dontUseProps <- FALSE
    if (is.null(properties)) {
      dontUseProps <- TRUE
    } else {
      if (!is.null(myStp1Selected)) {
        if (myStp1Selected != myStp1Val) {
          dontUseProps <- TRUE
 	}
      }
    }

    if (is.null(mySelected)) {
      dontUseProps <- FALSE
    }

    if (dontUseProps) {
      if (myStp1Val %in% anthro) {
        if (myStp1Val == "percentDays") {
          mySelected = "greaterThan"
        } else if (myStp1Val == "direct") {
          mySelected = groupRange$mean
        } else {
          mySelected = -1
        }
      } else {
        if (myStp2Val %in% numeric) {
          mySelected = groupRange$mean
        }
      }
    }

    if (myStp1Val %in% anthro) {
      if (myStp1Val == "percentDays") {
        selectInput(inputId = ns("group_stp3"),
                    label = "are",
                    choices = list('<' = 'lessThan', '>' = 'greaterThan', '=' = 'equals'),
                    selected = mySelected,
                    width = '100%')
      } else if (myStp1Val == "direct") {
        sliderInput(ns("group_stp3"), NULL,
                    min = groupRange$myMin, max = groupRange$myMax, value = mySelected, step = .1, width='100%')
      } else {
        sliderInput(ns("group_stp3"), NULL,
                    min = -20, max = 20, value = mySelected, step = .1, width = '100%')
      }
    } else if (myStp1Val %in% obs) {
      if (myStp2Val %in% numeric) {
        sliderInput(ns("group_stp3"), NULL,
                    min = groupRange$myMin, max = groupRange$myMax, value = mySelected, step = .1, width='100%')
      }
    }
 
  })
  
  output$choose_stp4 <- renderUI({
    if (is.null(input$group_stp1)) {
      return()
    }
    if (is.null(input$group_stp2)) {
      return()
    }
    assign(paste0(moduleName, "$group_stp3"), input$group_stp3, pos=parent.frame())
    if (is.null(input$group_stp3)) {
      return()
    }
    if (groupsType() != "makeGroups") {
      return()
    }
    myStp1Val <- input$group_stp1
    myStp1Selected <- properties$selected[properties$input == paste0(moduleName, "$group_stp1")]
    mySelected <- properties$selected[properties$input == paste0(moduleName, "$group_stp4")]    

    dontUseProps <- FALSE
    if (is.null(properties)) {
      dontUseProps <- TRUE
    } else {
      if (myStp1Selected != myStp1Val) {
        dontUseProps <- TRUE
      }
    }

    if (dontUseProps) {
      mySelected = groupRange$mean
    }

    if (!any(c("POSIXct", "Date") %in% class(myStp1Val))) {
      if (myStp1Val == "percentDays") {
        sliderInput(ns("group_stp4"), NULL,
                    min = groupRange$myMin, max = groupRange$myMax, value = mySelected, step = .1, width='100%')
      }
    }

  })
 
  return(reactive({ list('group' = getMyGroups$val, "group_stp1" = input$group_stp1, "group_stp2" = input$group_stp2, "group_stp3" = input$group_stp3, "group_stp4" = input$group_stp4) }))
}
