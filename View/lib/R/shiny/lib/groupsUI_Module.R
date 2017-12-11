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
customGroups <- function(input, output, session, groupLabel = "Name Me!!", metadata.file, useData, singleVarData, event.file, selected = reactive("EUPATH_0000704"), groupsType = reactive("makeGroups")) {
  ns <- session$ns
  
  groupRange <- reactiveValues()
  
  setGroupVals <- reactive({
    myGroup <- input$group
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
  observeEvent(input$group, setGroupVals())
  
  output$choose_group <- renderUI({
    print("in choose_group")
    print(paste("group label:", groupLabel()))
    print(paste("selected:", selected()))
    print(paste("groupsType:", groupsType()))
    if (groupsType() != "makeGroups" & groupsType() != "direct") {
      return()
    }
    
    attrChoiceList <- lapply(useData(), getUIList, metadata.file = metadata.file)
    attrChoiceList <- unlist(attrChoiceList, recursive = FALSE)
  
    selectInput(inputId = ns("group"),
                label = groupLabel(),
                choices = attrChoiceList,
                selected = selected(),
                width = '100%')
  })
  
  output$choose_stp1 <- renderUI({
    print(paste("in choose_stp1:", input$group))
    if (is.null(input$group)) {
      return()
    }
 
    if (groupsType() != "makeGroups") {
      return()
    }
    myGroup <- input$group
    nums <- getNums(metadata.file)
    dates <- getDates(metadata.file)
    
    data <- singleVarData
    tempDF <- completeDT(data, myGroup)
    
    if (any(colnames(tempDF) %in% "BFO_0000015")) {
      if (any(colnames(event.file) %in% myGroup) & levels(as.factor(tempDF$BFO_0000015)) == "Anthropometry") {
        selectInput(inputId = ns("group_stp1"),
                    label = "where",
                    choices = list('the change in value over time selected' = 'delta', 'more than the following percent of days' = 'percentDays', "a direct comparison" = "direct"),
                    selected = "delta",
                    width = '100%')
      } else {
        if (myGroup %in% nums$source_id) {
          selectInput(inputId = ns("group_stp1"),
                      label = "is",
                      choices = list('<' = 'lessThan', '>' = 'greaterThan', '=' = 'equals'),
                      selected = "greaterThan",
                      width = '100%')
        } else if (myGroup %in% dates$source_id) {
          dateRangeInput(inputId = ns("group_stp1"),
                         label = "is between",
                         start = groupRange$startDate, end = groupRange$endDate,
                         min = groupRange$myMin, max = groupRange$myMax,
                         separator = "and",
                         startview = "year")
        } else {
          attrStp1List <- getUIStp1List(singleVarData, myGroup)
          if (length(attrStp1List) == 2) {
            if (any(attrStp1List %in% "Yes")) {
              selectInput(inputId = ns("group_stp1"),
                          label = NULL,
                          choices = attrStp1List,
                          selected = "Yes",
                          width = '100%')
            } else if (any(attrStp1List %in% "TRUE")) {
              selectInput(inputId = ns("group_stp1"),
                          label = NULL,
                          choices = attrStp1List,
                          selected = "TRUE",
                          width = '100%')
            } else {
              selectInput(inputId = ns("group_stp1"),
                          label = NULL,
                          choices = attrStp1List,
                          width = '100%')
            }
          } else {
            selectInput(inputId = ns("group_stp1"),
                        label = NULL,
                        choices = attrStp1List,
                        width = '100%')
          }
        }
      }
    } else {
      if (myGroup %in% nums$source_id) {
        selectInput(inputId = ns("group_stp1"),
                    label = "is",
                    choices = list('<' = 'lessThan', '>' = 'greaterThan', '=' = 'equals'),
                    selected = "greaterThan",
                    width = '100%')
      } else if (myGroup %in% dates$source_id) {
        dateRangeInput(inputId = ns("group_stp1"),
                       label = "is between",
                       start = groupRange$startDate, end = groupRange$endDate,
                       min = groupRange$myMin, max = groupRange$myMax,
                       separator = "and",
                       startview = "year")
      } else {
        attrStp1List <- getUIStp1List(singleVarData, myGroup)
        if (length(attrStp1List) == 2) {
          if (any(attrStp1List %in% "Yes")) {
            selectInput(inputId = ns("group_stp1"),
                        label = NULL,
                        choices = attrStp1List,
                        selected = "Yes",
                        width = '100%')
          } else if (any(attrStp1List %in% "TRUE")) {
            selectInput(inputId = ns("group_stp1"),
                        label = NULL,
                        choices = attrStp1List,
                        selected = "TRUE",
                        width = '100%')
          } else {
            selectInput(inputId = ns("group_stp1"),
                        label = NULL,
                        choices = attrStp1List,
                        width = '100%')
          }
        } else {
          selectInput(inputId = ns("group_stp1"),
                      label = NULL,
                      choices = attrStp1List,
                      width = '100%')
        }
      }
    }
    
  })
  
  output$choose_stp2 <- renderUI ({
    if (is.null(input$group_stp1)) {
      return()
    }
    myStp1Val <- input$group_stp1
    
    numeric <- c("lessThan", "greaterThan", "equals")
    anthro <- c("percentDays", "delta", "direct")
    
    if (myStp1Val %in% anthro) {
      if (myStp1Val == 'percentDays') {
        numericInput(inputId = ns("group_stp2"),
                     label = NULL,
                     value = 50,
                     min = 0,
                     max = 100,
                     width = '100%')
      } else {
        selectInput(inputId = ns("group_stp2"),
                    label = "is",
                    choices = list('<' = 'lessThan', '>' = 'greaterThan', '=' = 'equals'),
                    selected = "greaterThan",
                    width = '100%')
      }
    } else {
      if (myStp1Val %in% numeric) {
        #just going to set default value to whatever the mean is
        sliderInput(ns("group_stp2"), NULL,
                    min = groupRange$myMin, max = groupRange$myMax, value = groupRange$mean, step = .1, width = '100%')
      }
    }
    
  })
  
  output$choose_stp3 <- renderUI ({
    if (is.null(input$group_stp1)) {
      return()
    }
    myStp1Val <- input$group_stp1
    
    anthro <- c("delta", "direct", "percentDays")
    
    if (myStp1Val %in% anthro) {
      if (myStp1Val == "percentDays") {
        selectInput(inputId = ns("group_stp3"),
                    label = "are",
                    choices = list('<' = 'lessThan', '>' = 'greaterThan', '=' = 'equals'),
                    selected = "greaterThan",
                    width = '100%')
      } else if (myStp1Val == "direct") {
        sliderInput(ns("group_stp3"), NULL,
                    min = groupRange$myMin, max = groupRange$myMax, value = groupRange$mean, step = .1, width='100%')
      } else {
        sliderInput(ns("group_stp3"), NULL,
                    min = -20, max = 20, value = -1, step = .1, width = '100%')
      }
      
    }
    
  })
  
  output$choose_stp4 <- renderUI({
    if (is.null(input$group_stp1)) {
      return()
    }
    myStp1Val <- input$group_stp1
    
    if (!any(c("POSIXct", "Date") %in% class(myStp1Val))) {
      if (myStp1Val == "percentDays") {
        sliderInput(ns("group_stp4"), NULL,
                    min = groupRange$myMin, max = groupRange$myMax, value = groupRange$mean, step = .1, width='100%')
      }
    }
  })
  
  return(input)
}
