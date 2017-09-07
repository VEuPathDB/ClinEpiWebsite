dropdownButton <- function(label = "", status = c("default", "primary", "success", "info", "warning", "danger"), ..., width = NULL) {
  
  status <- match.arg(status)
  # dropdown button content
  html_ul <- list(
    class = "dropdown-menu",
    style = if (!is.null(width)) 
      paste0("width: ", validateCssUnit(width), ";"),
    lapply(X = list(...), FUN = tags$li, style = "margin-left: 10px; margin-right: 10px;")
  )
  # dropdown button appearence
  html_button <- list(
    class = paste0("btn btn-", status," dropdown-toggle"),
    type = "button", 
    `data-toggle` = "dropdown"
  )
  html_button <- c(html_button, list(label))
  html_button <- c(html_button, list(tags$span(class = "caret")))
  # final result
  tags$div(
    class = "dropdown",
    do.call(tags$button, html_button),
    do.call(tags$ul, html_ul),
    tags$script(
      "$('.dropdown-menu').click(function(e) {
      e.stopPropagation();
});"
      )
  )
}

completeDF <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

completeDT <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, (desiredCols), with=FALSE])
  return(data[completeVec, ])
}

geom_tooltip <- function (mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", real.geom = NULL, ...) {
  
  #this because geom_histogram is just an alias for geom_bar with stat_bin
  #currently only possible override of stat_bin is stat_count. otherwise, 
  #you should be using geom_bar anyhow.
  if (real.geom == "geom_histogram") {
    if (stat == "count") {
      rg <- geom_bar(mapping = mapping, data = data, stat = "count",
                     position = position, ...)
    } else {
      rg <- geom_bar(mapping = mapping, data = data, stat = "bin",
                     position = position, ...)
    }
  } else {
    rg <- real.geom(mapping = mapping, data = data, stat = stat,
                    position = position, ...)       
  }
  
  #store the original ggproto object to inherit from as var parent 
  parent=rg$geom
  
  if(is.ggproto(parent)){
    #geom_area is handled differently because it relies on draw_group rather than draw panel to 
    #create the grobs that need to be garnished
    if(class(parent)[1]=="GeomArea" || class(parent)[1]=="GeomDensity") {
      rg$geom <-ggproto(parent, parent,
                         draw_group = function(data, panel_scales, coord, na.rm = FALSE) {
                           grob <- parent$draw_group(data, panel_scales, coord, na.rm = FALSE)
                           grob <- garnishGrob(grob, onmousemove=paste("showTooltip(evt, '",
                                                                       data[1,]$tooltip , "')"), onmouseout="hideTooltip(evt)",
                                               "pointer-events"="all")
                         },
                         required_aes = c("tooltip", parent$required_aes)
      )
      #geom line is handled seperate from segment in order to get the tooltips to match the groups.
      #we must force it to draw each line/ group seperately rather than as one grob
    } else if (class(parent)[1]=="GeomLine") {
      rg$geom <-ggproto(parent, parent,
                         draw_panel = function(self, data, panel_scales, coord,
                                               arrow = NULL, lineend = "butt", na.rm = FALSE) {
                           groups = unique(data$group)
                           grobs <- list()
                           for (i in 1:length(groups)) {
                             currGroup = groups[[i]]
                             currData = subset(data, data$group == currGroup)
                             grob <- parent$draw_panel(currData, panel_scales, coord, arrow, lineend)
                             grobs[[i]] <- garnishGrob(grob, onmousemove=paste("showTooltip(evt, '",
                                                                               currData[1,]$tooltip , "')"), onmouseout="hideTooltip(evt)",
                                                       "pointer-events"="all")
                           }
                           ggplot2:::ggname("geom_tooltip", gTree(children = do.call("gList", grobs)))
                         },
                         required_aes = c("tooltip", parent$required_aes)
      )
      #segment is handled seperately from the standard because the params passed to draw_panel are
      #different
    } else if (class(parent)[1]=="GeomSegment") {
      rg$geom <-ggproto(parent, parent,
                         draw_panel = function(self, data, panel_scales, coord,
                                               arrow = NULL, lineend = "butt", na.rm = FALSE) {
                           grobs <- list()
                           for (i in 1:nrow(data)) {
                             grob <- parent$draw_panel(data[i,], panel_scales, coord, arrow, lineend)
                             grobs[[i]] <- garnishGrob(grob, onmousemove=paste("showTooltip(evt, '",
                                                                               data[i,]$tooltip , "')"), onmouseout="hideTooltip(evt)",
                                                       "pointer-events"="all")
                           }
                           ggplot2:::ggname("geom_tooltip", gTree(children = do.call("gList", grobs)))
                         },
                         required_aes = c("tooltip", parent$required_aes)
      )
    } else {
      #replace ggproto object with one of our own design. overwrite draw_panel and required_aes.
      rg$geom <-ggproto(parent, parent,
                         draw_panel = function(self, data, panel_scales, coord, width = NULL,
                                               na.rm = FALSE) {
                           grobs <- list()
                           for (i in 1:nrow(data)) {
                             #call draw_panel of the original ggproto object. then garnish the grob it returns
                             #with svg attributes for tooltips
                             grob <- parent$draw_panel(data[i,], panel_scales, coord)
                             grobs[[i]] <- garnishGrob(grob, onmousemove=paste("showTooltip(evt, '",
                                                                               data[i,]$tooltip , "')"), onmouseout="hideTooltip(evt)",
                                                       "pointer-events"="all")
                           }
                           #adding our grobs to gtree to be rendered
                           ggplot2:::ggname("geom_tooltip", gTree(children = do.call("gList", grobs)))
                         },
                         #add tooltip to the aesthetics for our ggproto object
                         required_aes = c("tooltip", parent$required_aes)
      )
    }
    rg
  } else {
    stop("Geom layer specified by real.geom is not a known ggplot layer.");
  }
}
