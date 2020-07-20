timelineUI <- function(id) {
  #need a namespace
  ns <- NS(id)
  
  uiOutput(ns("timelineBox"))
}

timeline <- function(input, output, session, longitudinal, metadata.file) {
  ns <- session$ns

  propUrl <<- getPropertiesUrl(session)
  message("\n", Sys.time(), " functions/timelineUI_Module.R: propUrl: ", propUrl)
  properties <<- suppressWarnings(try(fread(propUrl)))
  if (length(properties) > 0) {
    properties <- unique(properties)
    message(Sys.time(), " functions/timelineUI_Module.R: reading properties...")
    if (grepl("Error", properties)) {
      message(Sys.time(), " functions/timelineUI_Module.R: Error! properties will not be used")
      properties <<- NULL
    } else {
      message(Sys.time(), " functions/timelineUI_Module.R: properties read:\n", properties)
    }
  } else {
    message(Sys.time(), " functions/timelineUI_Module.R: no properties! new analysis")
    properties <<- NULL
  } 
 
  dates <- getDates(metadata.file)$SOURCE_ID
  nums <- getNums(metadata.file)$SOURCE_ID
  strings <- getStrings(metadata.file)$SOURCE_ID
  if (all(longitudinal$columns %in% dates) | all(longitudinal$columns %in% nums) | all(longitudinal$columns %in% strings)) {
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

      if (selected %in% strings) {
        label <- paste0("Filter by ", metadata.file$PROPERTY[metadata.file$SOURCE_ID == longitudinal1$columns[1]])   

        dontUseProps <- FALSE
        if (is.null(properties)) {
          dontUseProps <- TRUE
        } 
      } else {
        myMin <- unique(metadata.file$MIN[metadata.file$SOURCE_ID == selected])
        myMax <- unique(metadata.file$MAX[metadata.file$SOURCE_ID == selected])
   message("my max orig: ", myMax)  
        if (length(longitudinal1$columns) == 1) {
          label <- paste0("Filter by ", metadata.file$PROPERTY[metadata.file$SOURCE_ID == longitudinal1$columns[1]])
	  if (longitudinal1$columns[1] %in% dates) {
	    myMin <- as.Date(myMin, format = "%Y-%m-%d")
            myMax <- as.Date(myMax, format = "%Y-%m-%d")
          } else {
	    myMin <- as.numeric(myMin)
            myMax <- as.numeric(myMax)
	  }
        } else {
          if (all(longitudinal1$columns %in% dates)) {
            label <- "Date Variable:"
	    myMin <- as.Date(myMin, format = "%Y-%m-%d")
	    myMax <- as.Date(myMax, format = "%Y-%m-%d")
          } else {
            label <- "Age Variable:"
	    myMin <- as.numeric(myMin)
	    myMax <- as.numeric(myMax)
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
    }
 message("timeline 1 max: " , selectedMax)

    #timeline2
    if(is.null(longitudinal2)) {
      timeline2 <- NULL 
    } else {
      selected2 <- longitudinal2$columns[1]
      if (is.null(selected2) | is.na(selected2) | selected2 == "NA") {
        timeline2 <- NULL
      } else {
        tempDF2 <- completeDT(data, selected2)
        myMin2 <- metadata.file$MIN[metadata.file$SOURCE_ID == selected2]
        myMax2 <- metadata.file$MAX[metadata.file$SOURCE_ID == selected2]

        if (length(longitudinal2$columns) == 1) {
          label2 <- paste0("Filter by ", metadata.file$PROPERTY[metadata.file$SOURCE_ID == longitudinal2$columns[1]])
 	  if (longitudinal2$columns[1] %in% dates) {
            myMin2 <- as.Date(myMin2, format = "%Y-%m-%d")
            myMax2 <- as.Date(myMax2, format = "%Y-%m-%d")
          } else {
	    myMin2 <- as.numeric(myMin2)
            myMax2 <- as.numeric(myMax2)
	  }
        } else {
          if (all(longitudinal2$columns %in% dates)) {
            label2 <- "Date Variable:"
            myMin2 <- as.Date(myMin2, format = "%Y-%m-%d")
            myMax2 <- as.Date(myMax2, format = "%Y-%m-%d")
          } else {
            label2 <- "Age Variable:"
            myMin2 <- as.numeric(myMin2)
            myMax2 <- as.numeric(myMax2)
          }
        }

        if (dontUseProps) {
          selectedMin2 <- myMin2
          selectedMax2 <- myMax2
        } else {
          selectedMin2 <- properties$selected[properties$input == "current$range2[1]"]
          selectedMax2 <- properties$selected[properties$input == "current$range2[2]"]
          if (selected2 %in% dates) {
            selectedMin2 <- as.Date(selectedMin2)
            selectedMax2 <- as.Date(selectedMax2)
          }
        }
      }  
    }

    if (is.null(timeline1)) {
      return()
    } else {
      if (is.null(timeline2)) {
        if (selected %in% strings) {
          #need to decide if i wantto keep on with this name or create new ui
          #consider the saving and reading of params and that there can be more or less than 2 values
          if (dontUseProps) {
            tagList(
              box(width = 12, status = "primary", title = "Timepoint Filter",
                  selectizeInput(inputId = ns("subset"),
                                 label = label,
                                 choices = getUIStp1List(metadata.file, selected),
                                 width = '100%',
                                 multiple = TRUE,
                                 options = list(placeholder = '-Selected Items Will Appear Here-'))    
              )
            )
          } else {
            tagList(
              box(width = 12, status = "primary", title = "Timepoint Filter", 
                  selectizeInput(inputId = ns("subset"),
                                 label = label,
                                 choices = getUIStp1List(metadata.file, selected),
                                 selected = properties$selected[properties$input == "current$subset"],
                                 width = '100%',
                                 multiple = TRUE,
                                 options = list(placeholder = '-Selected Items Will Appear Here-'))
              )
            ) 
          } 
        } else {
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
        }
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
                                   min = myMin2, max = myMax2, value = c(selectedMin2,selectedMax2), round=TRUE, width = '100%')
                )
              )
          )
        )
      }
    }
    
  })
  
  return(input)
}
