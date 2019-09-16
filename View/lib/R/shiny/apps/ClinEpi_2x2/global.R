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
#require(jsonlite)
require(ROracle)
require(XML)
require(shinycssloaders)
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

my.get_flatList <- function(nestedList, flatList = NULL, parent = "#") {
  for (name in names(nestedList)) {
    additionalAttributes <- list(
      "icon" = shinyTree:::fixIconName(attr(nestedList[[name]],"sticon")),
      "type" = attr(nestedList[[name]],"sttype")
    )
    additionalAttributes <- additionalAttributes[which(sapply(additionalAttributes,Negate(is.null)))]
    
    data <- lapply(names(attributes(nestedList[[name]])),function(key){
      if(key %in% c("icon","type","names","stopened","stselected","sttype", "stdisabled")){
        NULL
      }else{
        attr(nestedList[[name]],key)
      }
    })
    if(!is.null(data) && length(data) > 0){
      names(data) <- names(attributes(nestedList[[name]]))
      data <- data[which(sapply(data,Negate(is.null)))]
    }
    
    nodeData <- append(
      list(
        id = as.character(length(flatList) + 1),
        text = name,
        parent = parent,
        state = list(
          opened   = isTRUE(attr(nestedList[[name]], "stopened")),
          selected = isTRUE(attr(nestedList[[name]], "stselected")),
	  disabled = isTRUE(attr(nestedList[[name]], "stdisabled"))
        ),
        data = data
      ),
      additionalAttributes
    )

    flatList = c(flatList,list(nodeData))
    if (is.list(nestedList[[name]]))
      flatList =
        Recall(nestedList[[name]], flatList, parent = as.character(length(flatList)))
  }
  flatList
}

assignInNamespace("get_flatList", my.get_flatList, ns = "shinyTree")

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

"%||%" <- function(x, y) {
  if (length(x) > 0 || plotly:::is_blank(x)) x else y
}

trimFacets <- function(dimensions) {
  dimensions <- dimensions - .001
  if (sum(dimensions) > 1) {
    dimensions <- Recall(dimensions)
  }
  dimensions
}

my.get_domains <- function(nplots = 1, nrows = 1, margins = 0.01, 
                        widths = NULL, heights = NULL) {
  if (length(margins) == 1) margins <- rep(margins, 4)
  if (length(margins) != 4) stop("margins must be length 1 or 4", call. = FALSE)
  ncols <- ceiling(nplots / nrows)
  widths <- widths %||% rep(round(1 / ncols, 2), ncols)
  if (sum(widths) > 1) {
    widths <- trimFacets(widths)
  }
  heights <- heights %||% rep(round(1 / nrows, 2), nrows)
  if (sum(heights) > 1) {
    heights <- trimFacets(heights)
  }
  if (length(widths) != ncols) {
    stop("The length of the widths argument must be equal ",
         "to the number of columns", call. = FALSE)
  }
  if (length(heights) != nrows) {
    stop("The length of the heights argument is ", length(heights),
         ", but the number of rows is ", nrows, call. = FALSE)
  }
  if (any(widths < 0) | any(heights < 0)) {
    stop("The widths and heights arguments must contain positive values")
  }
  if (sum(widths) > 1 | sum(heights) > 1) {
    stop("The sum of the widths and heights arguments must be less than 1")
  }
  
  widths <- cumsum(c(0, widths))
  heights <- cumsum(c(0, heights))
  # 'center' these values if there is still room left 
  widths <- widths + (1 - max(widths)) / 2
  heights <- heights + (1 - max(heights)) / 2
  
  xs <- vector("list", ncols)
  for (i in seq_len(ncols)) {
    xs[[i]] <- c(
      xstart = widths[i] + if (i == 1) 0 else margins[1],
      xend = widths[i + 1] - if (i == ncols) 0 else margins[2]
    )
  }
  xz <- rep_len(xs, nplots)
  
  ys <- vector("list", nrows)
  for (i in seq_len(nplots)) {
    j <- ceiling(i / ncols)
    ys[[i]] <- c(
      ystart = 1 - (heights[j]) - if (j == 1) 0 else margins[3],
      yend = 1 - (heights[j + 1]) + if (j == nrows) 0 else margins[4]
    )
  }
  plotly:::list2df(Map(c, xz, ys))
}

assignInNamespace("get_domains", my.get_domains, ns = "plotly")
