# shiny module to create the 5 ui necessary for making custom groups
#check it has access to the functions files. source those in global.R before this file, rather than in the server.R file
#server logic should return inputs and range info

#fix ui options. all possible facets shows more steps than necessary.
#check out the width of cols. its not consistent.

timelineUI <- function(id) {
  #need a namespace
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(3, align = "left",
        uiOutput(ns("choose_longitudinal"))
      ),
      column(9,
        uiOutput(ns("choose_timeframe"))
      )
    )
  )
}

#make sure this returns inputs and range info 
timeline <- function(input, output, session, data, longitudinal, metadata.file) {
  ns <- session$ns

  propUrl <- getPropertiesUrl(session)
  properties <- try(fread(propUrl))

  if (grepl("Error", properties)) {
    properties <- NULL
  }

  output$choose_longitudinal <- renderUI({
    colnames <- longitudinal$columns
    choices <- subset(metadata.file, source_id %in% colnames)
    if (nrow(choices) == 0) {
      return()
    }
    choiceList <- as.vector(choices$source_id)
    names(choiceList) <- as.vector(choices$property)
    mylist <- as.list(choiceList)
    
    if (is.null(properties)) {
      selectInput(inputId = ns("longitudinal"),
                  label = "Select Time/Age Variable:",
                  choices = mylist)
    } else {
      selectInput(inputId = ns("longitudinal"),
                  label = "Select Time/Age Variable:",
                  choices = mylist,
                  selected = properties$selected[properties$input == "current$longitudinal"])
    }

  })

  output$choose_timeframe <- renderUI({
    selected <- input$longitudinal   
    if (is.null(selected)) {
      return()
    } else {
      tempDF <- completeDT(data, selected)
      myMin <- min(tempDF[[selected]])
      myMax <- max(tempDF[[selected]]) 
      mySelected <- properties$selected[properties$input == "current$longitudinal"]
      
      message(paste("current selection:", selected))
      message(paste("former selection:", mySelected))
  
      dontUseProps <- FALSE
      if (is.null(properties)) {
        dontUseProps <- TRUE
      } else {
        if (selected != mySelected) {
          dontUseProps <- TRUE
        }
      }

      if (dontUseProps) {
        message("making from new")
        sliderInput(ns("timeframe"), "Timeframe:",
                    min = myMin, max = myMax, value = c(myMin,myMax), round=TRUE, width = '100%')
      } else {
        message("making from props")
        dates <- getDates(metadata.file)
        selectedMin <- properties$selected[properties$input == "current$timeframe[1]"]
        selectedMax <- properties$selected[properties$input == "current$timeframe[2]"]
        if (selected %in% dates$source_id) {
          selectedMin <- as.Date(selectedMin)
          selectedMax <- as.Date(selectedMax)
        }
        sliderInput(ns("timeframe"), "Timeframe:",
                    min = myMin, max = myMax, value = c(selectedMin,selectedMax), round=TRUE, width = '100%')
      }
    }
    
  })
  
  return(input)
}
