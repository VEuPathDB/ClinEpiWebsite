summaryStats <- tabItem(tabName = "summaryStats", 
         fluidRow(
           box(width = 12, status = "primary", title = "Summary Statistics",
               uiOutput("statsTable")
           ),
           box()
         )
        )