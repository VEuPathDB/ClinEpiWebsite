# shiny module to create the 5 ui necessary for making custom groups
#check it has access to the functions files. source those in global.R before this file, rather than in the server.R file
#server logic should return inputs and range info

#fix ui options. all possible facets shows more steps than necessary.
#check out the width of cols. its not consistent.

timelineUI <- function(id) {
  #need a namespace
  ns <- NS(id)
  
  uiOutput(ns("timelineBox"))
}

#make sure this returns inputs and range info 
timeline <- function(input, output, session, data, longitudinal, metadata.file) {
  ns <- session$ns

  propUrl <<- getPropertiesUrl(session)
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

  output$timelineBox <- renderUI({

    timeline1 <- "GO"
    timeline2 <- "GO"
    #timeline1
    selected <- longitudinal1$columns[1]
    if (is.null(selected) | is.na(selected) | selected == "NA") {
      timeline1 <- NULL
    } else {
      tempDF <- completeDT(data, selected)
      myMin <- min(tempDF[[selected]])
      myMax <- max(tempDF[[selected]]) 
     
      if (length(longitudinal1$columns) == 1) {
        label <- paste0("Filter by ", metadata.file$property[metadata.file$source_id == longitudinal1$columns[1]])
      } else {
        if (all(longitudinal1$columns %in% dates)) {
          label <- "Date Variable:"
        } else {
          label <- "Age Variable:"
        }
      }
      
      dontUseProps <- FALSE
      if (is.null(properties)) {
        dontUseProps <- TRUE
      }
    
      if (dontUseProps) {
        selectedMin <- myMin
        selectedMax <- myMax 
      } else {
        selectedMin <- properties$selected[properties$input == "current$range1[1]"]
        selectedMax <- properties$selected[properties$input == "current$range1[2]"]
        if (selected %in% dates) {
            selectedMin <- as.Date(selectedMin)
            selectedMax <- as.Date(selectedMax)
        }
      }
    }

    #timeline2
    if(is.null(longitudinal2)) {
      timeline2 <- NULL 
    } else {
      selected2 <- longitudinal2$columns[1]
      if (is.null(selected2) | is.na(selected2) | selected2 == "NA") {
        timeline2 <- NULL
      } else {
        tempDF2 <- completeDT(data, selected2)
        myMin2 <- min(tempDF[[selected2]])
        myMax2 <- max(tempDF[[selected2]])

        if (length(longitudinal2$columns) == 1) {
          label2 <- paste0("Filter by ", metadata.file$property[metadata.file$source_id == longitudinal2$columns[1]])
        } else {
          if (all(longitudinal2$columns %in% dates)) {
            label2 <- "Date Variable:"
          } else {
            label2 <- "Age Variable:"
          }
        }

        if (dontUseProps) {
          selectedMin2 <- myMin2
          selectedMax2 <- myMax2
        } else {
          selectedMin2 <- properties$selected[properties$input == "current$range2[1]"]
          selectedMax2 <- properties$selected[properties$input == "current$range2[2]"]
          if (selected %in% dates) {
            selectedMin2 <- as.Date(selectedMin2)
            selectedMax2 <- as.Date(selectedMax2)
          }
        }
      }  
    }

    if (is.null(timeline1)) {
      print("returning empty timelineBox")
      return
    } else {
      if (is.null(timeline2)) {
        tagList(
          box(width = 12, status = "primary", title = "Timeline(s)",
              fluidRow(
                column(12,
                      sliderInput(ns("range1"), label,
                      min = myMin, max = myMax, value = c(selectedMin,selectedMax), round=TRUE, width = '100%')
                )
              )
          )
        )
      } else {
        tagList(
          box(width = 12, status = "primary", title = "Timeline(s)",
              fluidRow(
                column(12,
                      sliderInput(ns("range1"), label,
                      min = myMin, max = myMax, value = c(selectedMin,selectedMax), round=TRUE, width = '100%')
                )
              ),
              fluidRow(
                column(12,
                       sliderInput(ns("range2"), label2,
                                   min = myMin2, max = myMax2, value = c(selectedMin,selectedMax), round=TRUE, width = '100%')
                )
              )
          )
        )
      }
    }
    
  })
  
  return(input)
}
