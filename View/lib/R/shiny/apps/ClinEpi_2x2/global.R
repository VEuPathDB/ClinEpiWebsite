require(shiny)
require(data.table)
require(tidyr)
require(shinyjs)
require(DT)
require(plotly)
require(viridisLite)
require(httr)
require(shinyTree)
require(shinydashboard)
require(data.tree)
require(digest)
require(jsonlite)
source("../../functions/wdkDataset.R")
source("config.R")
source("../../functions/ebrc_functions.R")
source("../../functions/clinepi_functions.R")
source("../../functions/groupsUI_Module.R")
source("../../functions/timelineUI_Module.R")
source("ui/sidebar.R")
source("ui/plotParams.R")
source("ui/summaryStats.R")
source("ui/contTable.R")
source("ui/plotGrid.R")
source("ui/individualPlot.R")
source("ui/helpTab.R")
source("ui/body.R")

timeoutSeconds <- 900

inactivity <- sprintf("function idleTimer() {
var t = setTimeout(logout, %s);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
Shiny.setInputValue('timeOut', '%ss')
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, %s);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();", timeoutSeconds*1000, timeoutSeconds, timeoutSeconds*1000)

#options(shiny.fullstacktrace = TRUE)

my.jsonToAttr <- function(json){
  ret <- list()

  if (length(json) > 0) { 
    if (! "text" %in% names(json)){
      # This is a top-level list, not a node.
      for (i in 1:length(json)){
        ret[[json[[i]]$text]] <- Recall(json[[i]])
        ret[[json[[i]]$text]] <- shinyTree:::supplementAttr(ret[[json[[i]]$text]], json[[i]])
      }
      return(ret)
    }
    
    if (length(json$children) > 0){
      return(Recall(json[["children"]]))
    } else {
      ret <- 0
      ret <- shinyTree:::supplementAttr(ret, json)    
      return(ret)
    }
  }

  ret
}

assignInNamespace("jsonToAttr", my.jsonToAttr, ns = "shinyTree")

my.get_selected_names <- function(tree, ancestry=NULL, vec=list()){
  if (is.list(tree) & length(tree) > 0) {
    for (i in 1:length(tree)){
      anc <- c(ancestry, names(tree)[i])
      vec <- Recall(tree[[i]], anc, vec)
    }    
  }
  
  a <- attr(tree, "stselected", TRUE)
  if (!is.null(a) && a == TRUE){
    # Get the element name
    el <- tail(ancestry,n=1)
    vec[length(vec)+1] <- el
    attr(vec[[length(vec)]], "ancestry") <- head(ancestry, n=length(ancestry)-1)
    #save attributes that start with "st" (ShinyTree)
    lapply(names(attributes(tree)),function(attribute){
        if(grepl("^st",attribute)){
            attr(vec[[length(vec)]], attribute) <<- attr(tree,attribute)
        }
    })
  }
  return(vec)
}

assignInNamespace("get_selected_names", my.get_selected_names, ns = "shinyTree")

my.get_selected_slices <- function(tree, ancestry=NULL, vec=list()){
  
  if (is.list(tree) & length(tree) > 0) {
    for (i in 1:length(tree)){
      anc <- c(ancestry, names(tree)[i])
      vec <- Recall(tree[[i]], anc, vec)
    }    
  }
  
  a <- attr(tree, "stselected", TRUE)
  if (!is.null(a) && a == TRUE){
    # Get the element name
    ancList <- 0
    
    for (i in length(ancestry):1){
      nl <- list()
      nl[ancestry[i]] <- list(ancList)
      ancList <- nl
    }
    vec[length(vec)+1] <- list(ancList)
  }
  return(vec)
}

assignInNamespace("get_selected_slices", my.get_selected_slices, ns = "shinyTree")
