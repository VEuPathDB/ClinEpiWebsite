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
customGroups <- function(input, output, session, groupLabel = "Name Me!!", metadata.file, include, singleVarData, event.file, selected = reactive("custom"), groupsType = reactive("makeGroups"), groupsTypeID = NULL, moduleName, prtcpntView = NULL) {
  ns <- session$ns

  propUrl <- "https://dcallan.clinepidb.org:443/ce.dcallan/service/users/221248430/steps/100005220/analyses/100001990/properties?accessToken=092e13cc862e4159"
  properties <- try(fread(propUrl))

  if (grepl("Error", properties)) {
    properties <- NULL
  }   

  obsView <- FALSE
  if (!is.null(prtcpntView())) {
    if (!prtcpntView() == TRUE) {
      obsView <- TRUE
    } 
  } 

  groupRange <- reactiveValues()
  getMyGroups <- reactiveValues() 
 
  setGroupVals <- reactive({
    if(is.null(getMyGroups$val)) {
      return()
    }
    myGroup <- getMyGroups$val
    print(paste("myGroup:", myGroup))
    nums <- getNums(metadata.file)
    dates <- getDates(metadata.file)
    
    if (myGroup %in% nums$source_id | myGroup %in% dates$source_id) {
      data <- singleVarData
      tempDF <- completeDT(data, myGroup)
      
      if (any(colnames(event.file) %in% myGroup) & any(colnames(tempDF) %in% "BFO_0000015")) {
        if (levels(as.factor(tempDF$BFO_0000015)) == "Diarrhea Episode") {
          groupRange$myMin = 0
        }
      } else {
        print(head(tempDF))
        print(tempDF[[myGroup]])
        message(paste("class myGroup:", class(tempDF[[myGroup]])))
        groupRange$myMin <- min(tempDF[[myGroup]])
      }
      groupRange$myMax <- max(tempDF[[myGroup]])
      
      if (myGroup %in% nums$source_id) {
        groupRange$mean <- mean(tempDF[[myGroup]])
      } else {
        groupRange$startDate <- as.Date(quantile(as.POSIXct(tempDF[[myGroup]]), .25))
        groupRange$endDate <- as.Date(quantile(as.POSIXct(tempDF[[myGroup]]), .75))
        groupRange$myMin <- as.Date(groupRange$myMin)
        groupRange$myMax <- as.Date(groupRange$myMax)
        message(paste("start and end dates:", groupRange$startDate, groupRange$endDate))
      }
    }
    
  })
  
  #make sure ranges are updated when attr or out change
  observeEvent(getMyGroups$val, setGroupVals())
  
  output$group <- renderTree({
    if (groupsType() != "makeGroups" & groupsType() != "direct") {
      return()
    }
    
    if (groupsType() == "makeGroups") {
      attrChoiceList <- getUIList(data = singleVarData, metadata.file = metadata.file, include = include())
    } else {
      attrChoiceList <- getUIList(data = singleVarData, metadata.file = metadata.file, include = include(), maxLevels = 12)
    }
    
    attrChoiceList

  })
  
  output$choose_group <- renderUI({
    if (groupsType() != "makeGroups" & groupsType() != "direct") {
      return()
    }
    
    myGroupBtnLabel <- getGroupBtnLabel()
    
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
                         shinyTree(ns("group"), search = TRUE)
                       )),
        style="margin-bottom: 10px"
      )
    )
  })

  observeEvent(input$group, {
    if (length(get_selected(input$group, format="names")) != 0) {
      nextGroup <- metadata.file$source_id[metadata.file$property == get_selected(input$group, format="names")[1][[1]]][1]
    
      if (is.null(getMyGroups$val)) {
        getMyGroups$val <- nextGroup
      } else if (getMyGroups$val != nextGroup) {
        getMyGroups$val <- nextGroup
      }
    }
  })
  
  getGroupBtnLabel <- reactive({
    myGroup <- getMyGroups$val
   
    groupsTypeSelected <- properties$selected[properties$input == groupsTypeID]

      dontUseProps <- FALSE
      if (is.null(properties)) {
        dontUseProps <- TRUE
      } else {
          if (!is.null(groupsType()) & !is.null(groupsTypeID)) {
            if (groupsTypeSelected != groupsType()) {
              dontUseProps <- TRUE
            }
          }
      }

      if (dontUseProps) {
        mySelected = selected()
      } else {
        mySelected = properties$selected[properties$input == paste0(moduleName, "$group")]
      }

    if (is.null(myGroup)) {
      label <- metadata.file$property[metadata.file$source_id == mySelected]
      getMyGroups$val <- mySelected
      if (is.null(label)) {
        label <- mySelected
      }
      #label <- "Please select one"
    } else {
      label <- metadata.file$property[metadata.file$source_id == myGroup]
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
    
    data <- singleVarData
    tempDF <- completeDT(data, myGroup)

    myGroupSelected <- properties$selected[properties$input == paste0(moduleName, "$group")]
    mySelected <- properties$selected[properties$input == paste0(moduleName, "$group_stp1")]
    groupsTypeSelected <- properties$selected[properties$input == groupsTypeID]  

    attrStp1List <- getUIStp1List(singleVarData, myGroup)
    observations <- colnames(event.file)

    dontUseProps <- FALSE
    if (is.null(properties)) {
      dontUseProps <- TRUE
    } else {
      if (myGroupSelected != myGroup) {
        dontUseProps <- TRUE
      }
      if (!is.null(groupsTypeID)) {
        if (!is.null(groupsTypeSelected)) {
          if (groupsType() != groupsTypeSelected & myGroup %in% dates$source_id) {
            dontUseProps <- TRUE
          }
        }
      }
    }

    obs <- FALSE
    anthro <- FALSE
    if (dontUseProps) {
      if (myGroup %in% observations & !obsView) {
        obs <- TRUE
        mySelected = "any"
      } else {
        if (myGroup %in% nums$source_id) {
          mySelected = "greaterThan"
        } else if (myGroup %in% dates$source_id) {
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

      if (any(colnames(tempDF) %in% "BFO_0000015")) {
        if (any(colnames(event.file) %in% myGroup) & levels(as.factor(tempDF$BFO_0000015)) == "Anthropometry") {
          mySelected = "delta"
          anthro <- TRUE
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
      if (myGroup %in% nums$source_id) {
        selectInput(inputId = ns("group_stp1"),
                    label = "is",
                    choices = list('<' = 'lessThan', '>' = 'greaterThan', '=' = 'equals'),
                    selected = mySelected,
                    width = '100%')
      } else if (myGroup %in% dates$source_id) {
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
    if (is.null(input$group_stp1)) {
       return()
    }
    if (input$group_stp1 == "") {
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
    attrStp1List <- getUIStp1List(singleVarData, myGroup)

    numeric <- c("lessThan", "greaterThan", "equals")
    anthro <- c("percentDays", "delta", "direct")
    obs <- c("all", "any")

    dontUseProps <- FALSE
    if (is.null(properties)) {
      dontUseProps <- TRUE
    } else {
      if (myStp1Selected != myStp1Val) {
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
        if (myGroup %in% nums$source_id) {
          mySelected = "greaterThan"
        } else if (myGroup %in% dates$source_id) {
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
      if (myGroup %in% nums$source_id) {
        selectInput(inputId = ns("group_stp2"),
                    label = NULL,
                    choices = list('<' = 'lessThan', '>' = 'greaterThan', '=' = 'equals'),
                    selected = mySelected,
                    width = '100%')
      } else if (myGroup %in% dates$source_id) {
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
      if (myStp1Selected != myStp1Val) {
        dontUseProps <- TRUE
      }
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
  
  return(input)
}
