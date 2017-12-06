# shiny module to create the 5 ui necessary for making custom groups
#check it has access to the functions files. source those in global.R before this file, rather than in the server.R file
#server logic should return inputs and range info

#fix ui options. all possible facets shows more steps than necessary.
#check out the width of cols. its not consistent.

timelineUI <- function(id) {
  #need a namespace
  ns <- NS(id)
  
  uiOutput(ns("choose_timeframe"))
}

#make sure this returns inputs and range info 
timeline <- function(input, output, session, data) {
  ns <- session$ns
  print("in timeline module")
  output$choose_timeframe <- renderUI({
    ageDays = "EUPATH_0000644"
    print("about to renderUI")
    if (any(colnames(data) %in% ageDays)) {
      tempDF <- completeDT(data, ageDays)
      print("finding min and max")
      myMin <- min(tempDF[, (ageDays), with=FALSE])
      myMax <- max(tempDF[, (ageDays), with=FALSE]) 
      
      sliderInput(ns("timeframe"), "Timeframe:",
                  min = myMin, max = myMax, value = c(myMin,myMax), round=TRUE, width = '100%')
    }
    
  })
  
  return(input)
}