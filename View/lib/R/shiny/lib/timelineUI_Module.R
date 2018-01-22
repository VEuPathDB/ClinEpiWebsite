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
        uiOutput(ns("choose_var1"))
      ),
      column(9,
        uiOutput(ns("choose_range1"))
      )
    ),
    fluidRow(
      column(3, align = "left",
        uiOutput(ns("choose_var2"))
      ),
      column(9,
        uiOutput(ns("choose_range2"))
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

  dates <- getDates(metadata.file)$source_id
  nums <- getNums(metadata.file)$source_id
  if (all(longitudinal$columns %in% dates) | all(longitudinal$columns %in% nums)) {
    numTimelines <<- 1
  } else {
    numTimelines <<- 2
  }
  if (numTimelines == 1) {
    longitudinal1 <- longitudinal
    longitudinal2 <- NULL
  } else {
    longitudinal1 <- subset(longitudinal, longitudinal$columns %in% dates)
    longitudinal2 <- subset(longitudinal, longitudinal$columns %in% nums)
  }

  output$choose_var1 <- renderUI({
    colnames <- longitudinal1$columns
    choices <- subset(metadata.file, source_id %in% colnames)
    if (nrow(choices) == 0) {
      return()
    }
    choiceList <- as.vector(choices$source_id)
    names(choiceList) <- as.vector(choices$property)
    mylist <- as.list(choiceList)
    if (all(longitudinal1$columns %in% dates)) {
      label <- "Select Date Variable:"
    } else {
      label <- "Select Age Variable:"
    } 
    
    if (is.null(properties)) {
      selectInput(inputId = ns("var1"),
                  label = label,
                  choices = mylist)
    } else {
      selectInput(inputId = ns("var1"),
                  label = label,
                  choices = mylist,
                  selected = properties$selected[properties$input == "current$var1"])
    }

  })

  output$choose_range1 <- renderUI({
    selected <- input$var1 
    if (is.null(selected)) {
      return()
    } else {
      tempDF <- completeDT(data, selected)
      myMin <- min(tempDF[[selected]])
      myMax <- max(tempDF[[selected]]) 
      mySelected <- properties$selected[properties$input == "current$var1"]
      
      dontUseProps <- FALSE
      if (is.null(properties)) {
        dontUseProps <- TRUE
      } else {
        if (selected != mySelected) {
          dontUseProps <- TRUE
        }
      }

      if (dontUseProps) {
        sliderInput(ns("range1"), "Range:",
                    min = myMin, max = myMax, value = c(myMin,myMax), round=TRUE, width = '100%')
      } else {
        selectedMin <- properties$selected[properties$input == "current$range1[1]"]
        selectedMax <- properties$selected[properties$input == "current$range1[2]"]
        if (selected %in% dates) {
          selectedMin <- as.Date(selectedMin)
          selectedMax <- as.Date(selectedMax)
        }
        sliderInput(ns("range1"), "Range:",
                    min = myMin, max = myMax, value = c(selectedMin,selectedMax), round=TRUE, width = '100%')
      }
    }

  })
    
  output$choose_var2 <- renderUI({
    if(is.null(longitudinal2)) {
      return()
    }
    colnames <- longitudinal2$columns
    choices <- subset(metadata.file, source_id %in% colnames)
    if (nrow(choices) == 0) {
      return()
    }
    choiceList <- as.vector(choices$source_id)
    names(choiceList) <- as.vector(choices$property)
    mylist <- as.list(choiceList)

    if (all(longitudinal2$columns %in% dates)) {
      label <- "Select Date Variable:"
    } else {
      label <- "Select Age Variable:"
    } 

    if (is.null(properties)) {
      selectInput(inputId = ns("var2"),
                  label = label,
                  choices = mylist)
    } else {
      selectInput(inputId = ns("var2"),
                  label = label,
                  choices = mylist,
                  selected = properties$selected[properties$input == "current$var2"])
    }

  })

  output$choose_range2 <- renderUI({
    if(is.null(longitudinal2)) {
      return()
    }
    selected <- input$var2
    if (is.null(selected)) {
      return()
    } else {
      tempDF <- completeDT(data, selected)
      myMin <- min(tempDF[[selected]])
      myMax <- max(tempDF[[selected]])
      mySelected <- properties$selected[properties$input == "current$var2"]

      dontUseProps <- FALSE
      if (is.null(properties)) {
        dontUseProps <- TRUE
      } else {
        if (selected != mySelected) {
          dontUseProps <- TRUE
        }
      }

      if (dontUseProps) {
        sliderInput(ns("range2"), "Range:",
                    min = myMin, max = myMax, value = c(myMin,myMax), round=TRUE, width = '100%')
      } else {
        selectedMin <- properties$selected[properties$input == "current$range2[1]"]
        selectedMax <- properties$selected[properties$input == "current$range2[2]"]
        if (selected %in% dates) {
          selectedMin <- as.Date(selectedMin)
          selectedMax <- as.Date(selectedMax)
        }
        sliderInput(ns("range2"), "Range:",
                    min = myMin, max = myMax, value = c(selectedMin,selectedMax), round=TRUE, width = '100%')
      }
    }  

  })
  
  return(input)
}
