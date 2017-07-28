## server.r
require(rCharts)

source("../../lib/wdkDataset.R")
source("config.R")

shinyServer(function(input, output, session) {

  datasetFetcher <- reactive({
    file=read.csv(
        getWdkDatasetFile('attributes.tab', session, FALSE, dataStorageDir),
        sep = "\t",
        check.names = FALSE
     )
     names(file) =  tolower(gsub("\\W", "", names(file)))

     return(file);
  })
  
    output$myChart <- renderChart({

      cv <- datasetFetcher()


      #Set values for graphing
      x_ <- input$x
      y_ <- input$y
      clr <- input$color
      fct <- input$facet
      fill <- input$fill
      grp <- input$group
      typ <- input$type
      sdate <- input$dateR[1]
      edate <- input$dateR[2]
      
      #scv <- subset(cv, visit_date > min.d & visit_date < max.d, select = c(x_, y_, clr,fill,grp,visit_date))
      #paste(scv)

      p1 <- rPlot(input$x, input$y, data = cv, type = typ, group =grp,facet=fct, color=clr)
      p1$addParams(dom = 'myChart')
      return(p1)
  })
})
